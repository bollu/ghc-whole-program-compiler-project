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


haskreturnop :: (MLIR.SSAId, MLIR.Type) -> MLIR.Operation
haskreturnop (retv, rett) = MLIR.defaultop {
       MLIR.opname = "hask.return", 
       MLIR.opvals = MLIR.ValueUseList [retv],
       MLIR.opty = MLIR.FunctionType [rett] [rett] 
  }

haskvalty :: MLIR.Type
haskvalty = MLIR.TypeCustom ("!hask.value")

data GenMState = GenMState
initGenMState :: GenMState; initGenMState = GenMState

data GenM a = GenM { runGenM :: GenMState -> (a, GenMState) }

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
name2String :: Binder -> String
name2String = BS8.unpack . binderUniqueName 

-- | helper to create BBs
block :: String -- ^ BB name
  -> [(SSAId, MLIR.Type)] -- ^ BB arg list 
  -> [Operation] -- ^ BB instructions
  -> Block
block name _ []= error $ "empty list passed to: " ++ name
block name args ops = Block (BlockLabel (BlockId name) args) ops


codegenExpr :: Expr -> GenM ([Operation], SSAId)
codegenExpr (StgApp fn args resty _) = error $ "ap"
codegenExpr (StgLit lit) = error $ "lit"
codegenExpr (StgConApp constructor args argtys) = error $ "constructor"
codegenExpr (StgOpApp stgop args resty idontknow_result_type_nme) = error $ "opap"
codegenExpr (StgCase exprscrutinee  resultName altType altsDefaultFirst) = error $ "stgcase"
codegenExpr (StgLet binding body) = error $ "let"
codegenExpr (StgLetNoEscape binding body) = error $ "letNoEscape"
codegenExpr e = error $ "unknown expression"

translateRhs :: String -> Rhs -> GenM [Operation]
translateRhs name (StgRhsClosure updflag args exprbody) = do
  (ops, finalval) <- codegenExpr exprbody
  let entry = block "entry"  [] (ops ++ [haskreturnop (finalval, haskvalty)])
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
  translateRhs (name2String name) rhs
translateTopBinding (StgTopLifted (StgRec bindings)) = do
  ops <- mconcat <$> forM  bindings (\(n, rhs) -> translateRhs (name2String n) rhs)
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
