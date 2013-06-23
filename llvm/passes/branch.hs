module LLVM.Passes.Branch (
  insertLabels
, testInsertLabels
, BranchPrim(..)
) where

import           Abc.Util (toBytes)
import qualified Abc.Def as Abc

data BranchPrim  = ConditionalP Int Int | JumpP Int | DestP Int | NoPrim
type BranchTags = [(BranchPrim, Abc.OpCode)]

testInsertLabels :: [Abc.OpCode] -> IO ()
testInsertLabels ops = do
  mapM_ p $ insertLabels ops
  where
    p (DestP l, op) = putStrLn $ show l ++ "\n\t" ++ show op
    p (ConditionalP t f, op) = putStrLn $ "\t" ++ show op ++ "    " ++ show t ++ " " ++ show f
    p (JumpP l, op) = putStrLn $ "\t" ++ show op ++ "    " ++ show l
    p (_, op) = putStrLn $ "\t" ++ show op

insertLabels :: [Abc.OpCode] -> BranchTags
insertLabels = falseBranch . prependEntry . insertMarkers

-- insert label for false branch targets
falseBranch :: BranchTags -> BranchTags
falseBranch (c@(ConditionalP t f, _):(_, op):ts) = [c, (DestP f, op)] ++ falseBranch ts
falseBranch (c@(_, _):ts) = [c] ++ falseBranch ts
falseBranch [] = []

-- make sure we have an "entry:"
prependEntry :: BranchTags -> BranchTags
prependEntry ((NoPrim, op):ts) = (DestP 0, op):ts

insertMarkers :: [Abc.OpCode] -> BranchTags
insertMarkers ops = loop 0 0 ops $ map ((,) NoPrim) ops
  where
    loop :: Int          -- current index
         -> Int          -- last label
         -> [Abc.OpCode] -- ops being parsed for branching instructions
         -> BranchTags
         -> BranchTags
    loop i l (Abc.Jump a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) False ts
    loop i l (Abc.IfNotLessThan a:ops) ts = loop (i+1) (l+2) ops $ addLabel i a (l+1) True ts
    loop i l (Abc.IfNotLessEqual a:ops) ts = loop (i+1) (l+2) ops $ addLabel i a (l+1) True ts
    loop i l (Abc.IfNotGreaterThan a:ops) ts = loop (i+1) (l+2) ops $ addLabel i a (l+1) True ts
    loop i l (Abc.IfNotGreaterEqual a:ops) ts = loop (i+1) (l+2) ops $ addLabel i a (l+1) True ts
    loop i l (Abc.IfTrue a:ops) ts = loop (i+1) (l+2) ops $ addLabel i a (l+1) True ts
    loop i l (Abc.IfFalse a:ops) ts = loop (i+1) (l+2) ops $ addLabel i a (l+1) True ts
    loop i l (Abc.IfEqual a:ops) ts = loop (i+1) (l+2) ops $ addLabel i a (l+1) True ts
    loop i l (Abc.IfNotEqual a:ops) ts = loop (i+1) (l+2) ops $ addLabel i a (l+1) True ts
    loop i l (Abc.IfLessThan a:ops) ts = loop (i+1) (l+2) ops $ addLabel i a (l+1) True ts
    loop i l (Abc.IfLessEqual a:ops) ts = loop (i+1) (l+2) ops $ addLabel i a (l+1) True ts
    loop i l (Abc.IfGreaterThan a:ops) ts = loop (i+1) (l+2) ops $ addLabel i a (l+1) True ts
    loop i l (Abc.IfGreaterEqual a:ops) ts = loop (i+1) (l+2) ops $ addLabel i a (l+1) True ts
    loop i l (Abc.IfStrictEqual a:ops) ts = loop (i+1) (l+2) ops $ addLabel i a (l+1) True ts
    loop i l (Abc.IfStrictNotEqual a:ops) ts = loop (i+1) (l+2) ops $ addLabel i a (l+1) True ts
    -- TODO LookupSwitch
    loop i l (op:ops) ts = loop (i+1) l ops ts
    loop i l [] ts = ts

addLabel :: Int     -- current index
         -> Abc.S24 -- jump
         -> Int     -- label
         -> Bool    -- is conditional branch
         -> BranchTags
         -> BranchTags
addLabel i j l conditional ts = newZipper
  where
    (as, (_, branchingOp):bs) = splitAt i ts
    (as2, (_, op):bs2) = case conditional of
      True -> jump j ((ConditionalP l (l+1), branchingOp):reverse as, bs)
      False -> jump j ((JumpP l, branchingOp):reverse as, bs)
    newZipper = reverse as2 ++ (DestP l, op):bs2

jump :: Abc.S24
     -> (BranchTags, BranchTags)
     -> (BranchTags, BranchTags)
jump s24 tuple
  | s24 > 0 = pos_jump s24 tuple
  | s24 < 0 = neg_jump s24 tuple
  | otherwise = tuple

pos_jump :: Abc.S24
         -> (BranchTags, BranchTags)
         -> (BranchTags, BranchTags)
pos_jump _ (aops, []) = (aops, [])
pos_jump s24 t@(aops, (m, bop):bops)
  | s24 > 0 = pos_jump (s24 - (fromIntegral $ toBytes bop)) ((m, bop):aops, bops)
  | otherwise = t

neg_jump :: Abc.S24
         -> (BranchTags, BranchTags)
         -> (BranchTags, BranchTags)
neg_jump _ ([], bops) = ([], bops)
neg_jump s24 t@((m, aop):aops, bops)
  | s24 < 0 = neg_jump (s24 + (fromIntegral $ toBytes aop)) (aops, (m, aop):bops)
  | otherwise = t
