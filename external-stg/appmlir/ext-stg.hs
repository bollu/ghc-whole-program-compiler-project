{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ParallelListComp #-}
-- https://github.com/bollu/coremlir/blob/master/core2MLIR/Core2MLIR/ConvertToMLIR.hs
import Control.Monad
import Data.Maybe
import Data.List (sortBy)
import Data.Monoid (mconcat, (<>), mempty)
import Data.Ord

import MLIR
import Options.Applicative
-- import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
-- import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Data.ByteString.Char8 as BS8

import Prettyprinter
import Stg.Syntax
-- import Stg.Pretty
import Stg.IO


codegenPrimRep :: PrimRep -> MLIR.Type
codegenPrimRep VoidRep = error "cannot codegen void type"
codegenPrimRep Int8Rep = TypeIntegerSignless 8
codegenPrimRep Int16Rep = TypeIntegerSignless 16
codegenPrimRep Int32Rep = TypeIntegerSignless 32
codegenPrimRep Int64Rep = TypeIntegerSignless 64
codegenPrimRep IntRep = TypeIntegerSignless 64
codegenPrimRep rep = error "unhandled primrep"

lzvaluetype :: MLIR.Type
lzvaluetype = MLIR.TypeCustom ("!hask.value")

codegenType :: Stg.Syntax.Type -> MLIR.Type
codegenType (SingleValue primrep) = codegenPrimRep primrep
codegenType (UnboxedTuple primreps) = error "cannot codegen unboxed tuple type"
codegenType (PolymorphicRep) = lzvaluetype
codegenType t = error "unknown type"

haskreturnop :: (MLIR.SSAId, MLIR.Type) -> MLIR.Operation
haskreturnop (retv, rett) = MLIR.defaultop {
       MLIR.opname = "hask.return", 
       MLIR.opvals = MLIR.ValueUseList [retv],
       MLIR.opty = MLIR.FunctionType [rett] [rett] 
  }


data GenMState = GenMState { genmuuid :: Int }
initGenMState :: GenMState; initGenMState = GenMState 0

data GenM a = GenM { runGenM :: GenMState -> (a, GenMState) }


gensymSSAId :: GenM SSAId
gensymSSAId = 
  GenM $ \(GenMState uuid) -> (SSAId (show uuid), GenMState (uuid + 1))

instance Monad GenM where
   return a = GenM $ \s -> (a, s)
   ma >>= a2mb = GenM $ \s -> 
        let (a, s') = runGenM ma s
            mb = a2mb a
            (b, s'') = runGenM mb s'
        in (b, s'')
instance Applicative GenM where
    pure = return
    (<*>) = ap
instance Functor GenM where
  fmap f ma = do 
    a <- ma; 
    let b = f a;
    return b


-- | convert a name to a string
binder2String :: Binder -> String
binder2String = BS8.unpack . binderUniqueName 


name2String :: Name -> String
name2String = BS8.unpack

-- | helper to create BBs
block :: String -- ^ BB name
  -> [(SSAId, MLIR.Type)] -- ^ BB arg list 
  -> [Operation] -- ^ BB instructions
  -> Block
block name _ []= error $ "empty list passed to: " ++ name
block name args ops = Block (BlockLabel (BlockId name) args) ops

constantop :: SSAId -> AttributeValue -> MLIR.Type -> Operation
constantop out v t = MLIR.defaultop {
  opname ="std.contant",
  opattrs=AttributeDict[("value", v)],
  opty=FunctionType [] [t],
  opresults=OpResultList [out]
}

constructop :: SSAId -- return ID
   -> String -- constructor name
   -> [SSAId] -- argument IDs 
   -> [MLIR.Type] -- argument types
   -> Operation -- final operation
constructop out name argvs argtys = MLIR.defaultop {
  opname="lz.construct",
  opvals=ValueUseList argvs,
  opty=FunctionType argtys [lzvaluetype],
  opattrs=AttributeDict[("value", attributeSymbolRef name)],
  opresults=OpResultList [out]
}                     


codegenLit :: Lit -> (AttributeValue, MLIR.Type)
codegenLit (LitNumber LitNumInt i) = (AttributeInteger i, TypeIntegerSignless 64)
codegenLit (LitNumber LitNumInt64 i) = (AttributeInteger i, TypeIntegerSignless 64)
codegenLit (LitNumber LitNumWord i) = error "unhandled literal"
codegenLit (LitNumber LitNumWord64 i) = error "unhandled literal"

codegenArg :: Arg -> GenM ([Operation], SSAId)
codegenArg (StgVarArg vbinder) = return ([], SSAId $ binder2String vbinder)
codegenArg (StgLitArg l) = do
  newid <- gensymSSAId
  let (val, ty) = codegenLit l
  return ([constantop newid val ty], newid)

codegenTycon :: TyCon -> MLIR.Type
codegenTycon tycon = MLIR.TypeCustom ("!lz.value")


forceop :: SSAId -- return ID
  -> SSAId -- input ID
  -> MLIR.Type -- input type
  -> Operation
forceop retid argid argty = MLIR.defaultop {
  opname="lz.force",
  opvals=ValueUseList [argid],
  opty=FunctionType [argty] [lzvaluetype],
  opresults=OpResultList [retid]
}                                    
  
-- | Generate the appropriate force for the case at hand
codegenCaseScrutineeForce :: AltType
  -> SSAId -- scrutinee
  -> GenM ([Operation], SSAId)
codegenCaseScrutineeForce PolyAlt argid = error $ "unknown alt type polyalt"
codegenCaseScrutineeForce (PrimAlt prim) argid = return ([], argid) 
codegenCaseScrutineeForce (AlgAlt tycon) argid = do
  newid <- gensymSSAId
  let force = forceop newid argid (codegenTycon tycon)
  return ([force], newid)


codegenAltLHS :: AltCon -> AttributeValue
codegenAltLHS (AltLit lit) =  let (val, ty) = codegenLit lit in val
codegenAltLHS (AltDataCon dc) = attributeSymbolRef . name2String . dcName $ dc
codegenAltLHS (AltDefault) = attributeSymbolRef "default"

-- https://github.com/bollu/coremlir/blob/master/core2MLIR/Core2MLIR/ConvertToMLIR.hs#L203
-- given an alt, generate the alt LHS, and the region RHS
codegenAlt :: Alt -> GenM (AttributeValue, Region)
codegenAlt (Alt altcon altbndrs rhs) = do
  let params = map (SSAId . binder2String) altbndrs
  let paramtys = map (codegenType . binderType) altbndrs
  (rhsops, retval) <- codegenExpr rhs
  let entry = block "entry"  (zip params paramtys) (rhsops ++ [haskreturnop (retval, lzvaluetype)])
  let r = MLIR.Region [entry]
  return (codegenAltLHS altcon, r)
  
caseop :: SSAId -- ^ return value
    -> SSAId -- ^ scrutinee value
    -> [(AttributeValue, Region)] -- ^ alt LHS, alt RHS
    -> Operation
caseop out scrutinee alts = MLIR.defaultop {
  opname="lz.case",
  opvals=ValueUseList [scrutinee] ,
  opty=FunctionType [lzvaluetype] [lzvaluetype],
  opattrs=AttributeDict [("value" <> show ix, lhs)  | ix <- [1..]  | (lhs, _) <- alts],
  opregions=RegionList (map snd alts),
  opresults=OpResultList [out]
}                     


-- | helper for codegen arg
codegenArgs :: [Arg] -> GenM ([Operation], [SSAId])
codegenArgs args = do
  -- | [([Operation, SSAId]]
  argCode <- forM args codegenArg
  let argIds = [id | (_, id) <- argCode]
  let argops = mconcat [ops | (ops, _) <- argCode]
  return (argops, argIds)

fnref :: Binder -> GenM ([Operation], SSAId)
fnref binder = do 
  newid <- gensymSSAId
  let name = (attributeSymbolRef . binder2String $ binder)
  let ty = codegenType (binderType binder)
  -- | what to do if this a local function? x(
  -- | FML?
  -- TODO: ask Csaba to easily test if this is a lambda bound or some such,
  -- or an actual global
  -- TODO2: GHC doesn't perform lambda lifting, so where are the lambdas?
  let op = constantop newid name ty
  return ([op], newid)

apop :: SSAId -- ^ new ID
  -> SSAId -- ^ fn argument
  -> [SSAId] -- ^ arguments
  -> Operation
apop out fn args = defaultop {
  opname="lz.ap",
  opvals=ValueUseList (fn:args),
  opty=FunctionType [lzvaluetype] [lzvaluetype],
  opresults=OpResultList [out]
}

codegenExpr :: Expr -> GenM ([Operation], SSAId)
codegenExpr (StgApp fn args resty _) = do
  (argops, argids) <- codegenArgs args
  (fnrefops, fnrefid) <- fnref fn
  newid <- gensymSSAId
  let op = apop newid fnrefid argids
  return $ (argops ++ fnrefops ++ [op], newid)
codegenExpr (StgLit lit) = do
  newid <- gensymSSAId
  let (val, ty) = codegenLit lit
  return ([constantop newid val ty], newid)
  
codegenExpr (StgConApp datacon args argtys) = do
  -- [([operation, SSAId])
  -- argCode <- forM args codegenArg
  -- let argIds = [id | (_, id) <- argCode]
  -- let argops = mconcat [ops | (ops, _) <- argCode]
  (argops, argIds) <- codegenArgs args
  newid <- gensymSSAId
  let construct = constructop newid (name2String (dcName datacon)) argIds (map codegenType argtys)
  return (argops ++ [construct], newid)

codegenExpr (StgOpApp stgop args resty idontknow_result_type_nme) = error $ "opap"
codegenExpr (StgCase exprscrutinee  resultName altType altsDefaultFirst) = do
  (exprops, exprid) <- codegenExpr exprscrutinee
  (forceops, scrutineeid) <- codegenCaseScrutineeForce altType exprid
  altsLhssRhss <- forM altsDefaultFirst codegenAlt
  newid <- gensymSSAId
  let case_ = caseop newid scrutineeid altsLhssRhss
  return (exprops ++ forceops ++ [case_], newid)

  -- we need to replace 'resultName' with the output of the case

codegenExpr (StgLet binding body) = error $ "let"
codegenExpr (StgLetNoEscape binding body) = error $ "letNoEscape"
codegenExpr e = error $ "unknown expression"

translateRhs :: String -> Rhs -> GenM [Operation]
translateRhs name (StgRhsClosure updflag args exprbody) = do
  (ops, finalval) <- codegenExpr exprbody
  let entry = block "entry"  [] (ops ++ [haskreturnop (finalval, lzvaluetype)])
  let r = MLIR.Region [entry]
  -- let r = MLIR.Region []
  return $ [MLIR.defaultop { 
       MLIR.opname = "hask.func", 
       MLIR.opattrs = MLIR.AttributeDict [("sym_name", MLIR.AttributeString name)],
       MLIR.opregions = MLIR.RegionList [r]
  }]

translateRhs name (StgRhsCon datacon args) = return []
  -- return $ MLIR.defaultop { 
  --      MLIR.opname = "hask.datacon", 
  --      MLIR.opattrs = MLIR.AttributeDict [("sym_name", MLIR.AttributeString name)],
  --      MLIR.opregions = MLIR.RegionList []
  -- }
  -- error $ "have not implemented translating rhs: |" <> name <> "|"


translateTopBinding :: TopBinding -> GenM [Operation]
translateTopBinding (StgTopLifted (StgNonRec name rhs)) = 
  translateRhs (binder2String name) rhs
translateTopBinding (StgTopLifted (StgRec bindings)) = do
  ops <- mconcat <$> forM  bindings (\(n, rhs) -> translateRhs (binder2String n) rhs)
  return ops
translateTopBinding (StgTopStringLit _ _) =  do
  return []
  -- error $ "dont know how to generate string"


stg2mlir :: Module -> GenM MLIRModule
stg2mlir m = do 
  bs <- mconcat <$>  forM (moduleTopBindings m)  translateTopBinding
  -- b <- translateTopBinding $ moduleTopBindings m !! 0
  return $ MLIRModule bs

modes :: Parser (IO ())
modes = subparser
    (  mode "show" showMode (progDesc "print Stg")
    )
  where
    mode :: String -> Parser a -> InfoMod a -> Mod CommandFields a
    mode name f opts = command name (info (helper <*> f) opts)

    modpakFile :: Parser FilePath
    modpakFile = argument str (metavar "MODPAK_FILENAME" <> help "pretty prints external stg from .modpak file")

    showMode :: Parser (IO ())
    showMode =
        run <$> modpakFile
      where
        run fname = do
            dump <- Stg.IO.readModpakL fname modpakStgbinPath decodeStgbin
            let !(!mlir, _) = runGenM (stg2mlir dump) initGenMState
            print $ "dumping module to MLIR!..." -- pprModule dump
            print (pretty mlir)

main :: IO ()
main = join $ execParser $ info (helper <*> modes) mempty
