module LLVM.Emitter.Bootstrap (bootstrap) where

import           Data.Word
import           LLVM.AbcOps
import           LLVM.Lang
import           LLVM.Util
import           LLVM.Passes.AbcT
import           LLVM.Passes.Branch
import           LLVM.Passes.LLVMT
import qualified Abc.Def as Abc
import qualified MonadLib as ML

bootstrap :: Abc -> IO [TopStmt]
bootstrap = return [
    FunctionDef_ $ FunctionDef Nothing Nothing Nothing I32 "main"
  ]
