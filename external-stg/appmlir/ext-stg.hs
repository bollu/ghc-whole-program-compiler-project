{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE OverloadedStrings #-}
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

liftedreptype :: MLIR.Type
liftedreptype = MLIR.TypeCustom ("!stg.liftedrep")

codegenPrimRep :: PrimRep -> MLIR.Type
codegenPrimRep VoidRep = error "cannot codegen void type"
codegenPrimRep Int8Rep = TypeIntegerSignless 8
codegenPrimRep Int16Rep = TypeIntegerSignless 16
codegenPrimRep Int32Rep = TypeIntegerSignless 32
codegenPrimRep Int64Rep = TypeIntegerSignless 64
codegenPrimRep IntRep = TypeIntegerSignless 64
codegenPrimRep DoubleRep = TypeCustom "f64"
codegenPrimRep FloatRep = TypeCustom "f32"
codegenPrimRep LiftedRep = liftedreptype
codegenPrimRep rep = error $ "unhandled primrep: " <> show rep

lzvaluetype :: MLIR.Type
lzvaluetype = MLIR.TypeCustom ("!lz.value")

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
codegenLit (LitDouble rat) = (AttributeFloat (fromRational rat), TypeCustom "f64")
codegenLit (LitFloat rat) = (AttributeFloat (fromRational rat), TypeCustom "f32")
codegenLit (LitFloat rat) = error "unhandled literal"
codegenLit lit = error $ "unknown lit: |" <> show lit <> "|"

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

callop :: SSAId -- ^ new ID
  -> SSAId -- ^ fn argument
  -> [SSAId] -- ^ arguments
  -> Operation
callop out fn args = defaultop {
  opname="std.call",
  opvals=ValueUseList (fn:args),
  opty=FunctionType [lzvaluetype] [lzvaluetype],
  opresults=OpResultList [out]
}

addiop :: SSAId -- ^ new ID
  -> [SSAId] -- ^ arguments
  -> Operation
addiop out args = defaultop {
  opname="std.addi",
  opvals=ValueUseList args,
  opty=FunctionType [lzvaluetype] [lzvaluetype],
  opresults=OpResultList [out]
}

subiop :: SSAId -- ^ new ID
  -> [SSAId] -- ^ arguments
  -> Operation
subiop out args = defaultop {
  opname="std.subi",
  opvals=ValueUseList args,
  opty=FunctionType [lzvaluetype] [lzvaluetype],
  opresults=OpResultList [out]
}

muliop :: SSAId -- ^ new ID
  -> [SSAId] -- ^ arguments
  -> Operation
muliop out args = defaultop {
  opname="std.muli",
  opvals=ValueUseList args,
  opty=FunctionType [lzvaluetype] [lzvaluetype],
  opresults=OpResultList [out]
}

-- | say that old id equals new id
equalop :: SSAId -- ^ new ID
  -> SSAId -- ^ old ID
  -> Operation
equalop out arg = defaultop {
  opname="std.equal",
  opvals=ValueUseList [arg],
  opty=FunctionType [lzvaluetype] [lzvaluetype],
  opresults=OpResultList [out]
}


codegenRhsInner :: SSAId -- out ID
  -> Rhs
  -> GenM([Operation])
codegenRhsInner outid (StgRhsCon datacon args) = do
  (argops, argIds) <- codegenArgs args
  let tys = replicate (length args) lzvaluetype 
  let construct = constructop outid (name2String (dcName datacon)) argIds tys
  return (argops <> [construct])

codegenRhsInner outid (StgRhsClosure updflags argbinders ebody) = do
  (bodyops, bodyid) <- codegenExpr ebody
  let args = [(SSAId $ binder2String arg, codegenType (binderType arg)) | arg <- argbinders]
  let entry = block "entry"  args bodyops
  --   (bodyops ++ [haskreturnop (bodyid, lzvaluetype)])
  let r = MLIR.Region [entry]
  -- -- (argops, argIds) <- codegenArgs args
  let lam = defaultop {
    opname="lz.lambda",
    -- opvals=ValueUseList argIds,
    opty=FunctionType [lzvaluetype] [lzvaluetype],
    opregions=RegionList [r],
    opresults=OpResultList [outid]
  }
  return []
  -- return [lam]

-- | let ...
--    x = data constructor  <= binding
--    y = lambda <= binding
codegenInnerBinding :: Binding -> GenM ([Operation], SSAId)
codegenInnerBinding (StgNonRec lhs rhs) = do
  let lhsid = SSAId $ binder2String $ lhs
  rhsops <- codegenRhsInner lhsid rhs
  return (rhsops , lhsid) 


-- | convert to destination passing style, where caller tells you
-- name to store the result in.
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

codegenExpr (StgOpApp stgop args resty idk_result_type_name) = do
  (argops, argIds) <- codegenArgs args
  case stgop of
    StgPrimOp "+#" -> do
       newid <- gensymSSAId
       let op = addiop  newid argIds
       return (argops <> [op], newid)
    StgPrimOp "-#" -> do
       newid <- gensymSSAId
       let op = subiop  newid argIds
       return (argops <> [op], newid)
    StgPrimOp "*#" -> do
       newid <- gensymSSAId
       let op = muliop  newid argIds
       return (argops <> [op], newid)
    _ -> error $ "unimplemented stg op |" <> show stgop <> "|"
codegenExpr (StgCase exprscrutinee  resultName altType altsDefaultFirst) = do
  (exprops, exprid) <- codegenExpr exprscrutinee
  (forceops, scrutineeid) <- codegenCaseScrutineeForce altType exprid
  altsLhssRhss <- forM altsDefaultFirst codegenAlt
  newid <- gensymSSAId
  let case_ = caseop newid scrutineeid altsLhssRhss
  return (exprops ++ forceops ++ [case_], newid)

  -- we need to replace 'resultName' with the output of the case

codegenExpr (StgLet binding body) = do
  (bindingops, bindingval) <- codegenInnerBinding binding
  (bodyops, bodyval) <- codegenExpr body
  return (bindingops <> bodyops, bodyval)

codegenExpr (StgLetNoEscape binding body) = error $ "letNoEscape"
codegenExpr e = error $ "unknown expression"

codegenRhsToplevel :: String -> Rhs -> GenM [Operation]
codegenRhsToplevel name (StgRhsClosure updflag args exprbody) = do
  (ops, finalval) <- codegenExpr exprbody
  let entry = block "entry"  [] (ops ++ [haskreturnop (finalval, lzvaluetype)])
  let r = MLIR.Region [entry]
  -- let r = MLIR.Region []
  return $ [MLIR.defaultop { 
       MLIR.opname = "hask.func", 
       MLIR.opattrs = MLIR.AttributeDict [("sym_name", MLIR.AttributeString name)],
       MLIR.opregions = MLIR.RegionList [r]
  }]

codegenRhsToplevel name (StgRhsCon datacon args) = return []
  -- return $ MLIR.defaultop { 
  --      MLIR.opname = "hask.datacon", 
  --      MLIR.opattrs = MLIR.AttributeDict [("sym_name", MLIR.AttributeString name)],
  --      MLIR.opregions = MLIR.RegionList []
  -- }
  -- error $ "have not implemented translating rhs: |" <> name <> "|"


codegenTopBinding :: TopBinding -> GenM [Operation]
codegenTopBinding (StgTopLifted (StgNonRec name rhs)) = 
  codegenRhsToplevel (binder2String name) rhs
codegenTopBinding (StgTopLifted (StgRec bindings)) = do
  ops <- mconcat <$> forM  bindings (\(n, rhs) -> codegenRhsToplevel (binder2String n) rhs)
  return ops
codegenTopBinding (StgTopStringLit _ _) =  do
  return []
  -- error $ "dont know how to generate string"


stg2mlir :: Module -> GenM MLIRModule
stg2mlir m = do 
  bs <- mconcat <$>  forM (moduleTopBindings m)  codegenTopBinding
  -- b <- codegenTopBinding $ moduleTopBindings m !! 0
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
