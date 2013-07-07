module LLVM.Emitter.Bootstrap (bootstrap) where

import           Data.Word
import           LLVM.AbcOps
import           LLVM.Lang
import           LLVM.Passes.AbcT
import           LLVM.Passes.Branch
import           LLVM.Passes.LLVMT
import           LLVM.Runtime.Types
import           LLVM.Util
import qualified Abc.Def as Abc
import qualified MonadLib as ML

bootstrap :: Abc.Abc -> IO [TopStmt]
bootstrap abc = return []
