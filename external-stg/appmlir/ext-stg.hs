{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
-- https://github.com/bollu/coremlir/blob/master/core2MLIR/Core2MLIR/ConvertToMLIR.hs

import Control.Monad
import Data.Maybe
import Data.List (sortBy)
import Data.Monoid
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

codegenArg :: Arg -> GenM ([Operation], SSAId)
codegenArg (StgVarArg vbinder) = return ([], SSAId $ binder2String vbinder)
codegenArg (StgLitArg (LitNumber LitNumInt i)) = do 
  newid <- gensymSSAId
  return ([constantop newid (AttributeInteger i) (TypeIntegerSignless 64)], newid)

codegenArg (StgLitArg (LitNumber LitNumInt64 i)) = do
  newid <- gensymSSAId
  return ([constantop newid (AttributeInteger i) (TypeIntegerSignless 64)], newid)

codegenArg (StgLitArg (LitNumber LitNumWord i)) = error "unhandled literal"
codegenArg (StgLitArg (LitNumber LitNumWord64 i)) = error "unhandled literal"
codegenArg (StgLitArg _) = error "unhandled literal"

codegenExpr :: Expr -> GenM ([Operation], SSAId)
codegenExpr (StgApp fn args resty _) = error $ "ap"
codegenExpr (StgLit lit) = error $ "lit"
codegenExpr (StgConApp datacon args argtys) = do
  -- [([operation, SSAId])
  argCode <- forM args codegenArg
  let argIds = [id | (_, id) <- argCode]
  let argops = mconcat [ops | (ops, _) <- argCode]
  newid <- gensymSSAId
  let construct = constructop newid (name2String (dcName datacon)) argIds (map codegenType argtys)
  return (argops ++ [construct], newid)

  

codegenExpr (StgOpApp stgop args resty idontknow_result_type_nme) = error $ "opap"
codegenExpr (StgCase exprscrutinee  resultName altType altsDefaultFirst) = error $ "stgcase"
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
