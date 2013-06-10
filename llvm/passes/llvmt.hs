module LLVM.Passes.LLVMT (emitLLVM) where

import           LLVM.AbcOps
import           LLVM.Lang
import qualified MonadLib as ML

type State = ML.StateT [R] IO

nextR :: D -> State R
nextR d = do
  rs <- ML.get
  let next = maxT rs
  ML.set $ next:rs
  return next
  where
    maxT :: [R] -> R
    maxT [] = RT d 1
    maxT rs = case maximum rs of
      R _ _ -> RT d 1
      RN _ i -> RT d (i+1)
      RAS3 _ i -> RT d (i+1)
      RT _ i -> RT d (i+1)

lastR :: State R
lastR = do
  (r:_) <- ML.get
  return r

emitLLVM :: [(Label, [OpCode])] -> State [Block]
emitLLVM ((l, ops):ts) = return [Block l [Load (R Bool "A") (R Bool "B")]]
