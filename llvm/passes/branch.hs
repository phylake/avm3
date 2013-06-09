module LLVM.Passes.Branch (insertLabels, testInsertLabels) where

import Abc.Def as Abc
import Abc.Util (toBytes)
import LLVM.Lang

testInsertLabels :: [Abc.OpCode] -> IO ()
testInsertLabels ops = do
  mapM_ f $ insertLabels ops
  where
    f (Nothing, op) = putStrLn $ "\t" ++ show op
    f (Just l, op) = putStrLn $ show l ++ "\n\t" ++ show op

insertLabels :: [Abc.OpCode] -> [(Maybe Label, Abc.OpCode)]
insertLabels = falseBranch . prependEntry . intLabelsToString . insertMarkers

falseBranch :: [(Maybe Label, Abc.OpCode)] -> [(Maybe Label, Abc.OpCode)]
falseBranch ops = loop (maxLabel ops) ops
  where
    loop :: Int -> [(Maybe Label, Abc.OpCode)] -> [(Maybe Label, Abc.OpCode)]
    loop i (c@(_, Abc.IfNotLessThan a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, Abc.IfNotLessEqual a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, Abc.IfNotGreaterThan a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, Abc.IfNotGreaterEqual a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, Abc.Jump a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, Abc.IfTrue a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, Abc.IfFalse a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, Abc.IfEqual a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, Abc.IfNotEqual a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, Abc.IfLessThan a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, Abc.IfLessEqual a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, Abc.IfGreaterThan a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, Abc.IfGreaterEqual a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, Abc.IfStrictEqual a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, Abc.IfStrictNotEqual a):(_, op):ts) = [c, (Just $ L "L" i, op)] ++ loop (i+1) ts
    loop i (c@(_, _):ts) = [c] ++ loop i ts
    loop i [] = []

maxLabel :: [(Maybe Label, Abc.OpCode)] -> Int
maxLabel ((Just (L _ i),_):ts) = max i $ maxLabel ts
maxLabel ((Nothing,_):ts) = maxLabel ts
maxLabel [] = 0

prependEntry :: [(Maybe Label, Abc.OpCode)] -> [(Maybe Label, Abc.OpCode)]
prependEntry ((Nothing, op):ts) = (Just $ L "entry" 0, op):ts

intLabelsToString :: [(Int, Abc.OpCode)] -> [(Maybe Label, Abc.OpCode)]
intLabelsToString = map toLabel
  where
    toLabel (0, op) = (Nothing, op)
    toLabel (i, op) = (Just $ L "L" i, op)

insertMarkers :: [Abc.OpCode] -> [(Int, Abc.OpCode)]
insertMarkers ops = loop 0 0 ops $ map ((,) 0) ops
  where
    loop :: Int          -- current index
         -> Int          -- last label
         -> [Abc.OpCode] -- ops being parsed for branching instructions
         -> [(Int, Abc.OpCode)]
         -> [(Int, Abc.OpCode)]
    loop i l (Abc.IfNotLessThan a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    loop i l (Abc.IfNotLessEqual a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    loop i l (Abc.IfNotGreaterThan a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    loop i l (Abc.IfNotGreaterEqual a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    loop i l (Abc.Jump a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    loop i l (Abc.IfTrue a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    loop i l (Abc.IfFalse a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    loop i l (Abc.IfEqual a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    loop i l (Abc.IfNotEqual a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    loop i l (Abc.IfLessThan a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    loop i l (Abc.IfLessEqual a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    loop i l (Abc.IfGreaterThan a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    loop i l (Abc.IfGreaterEqual a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    loop i l (Abc.IfStrictEqual a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    loop i l (Abc.IfStrictNotEqual a:ops) ts = loop (i+1) (l+1) ops $ addLabel i a (l+1) ts
    -- TODO LookupSwitch
    loop i l (op:ops) ts = loop (i+1) l ops ts
    loop i l [] ts = ts

addLabel :: Int     -- current index
         -> Abc.S24 -- jump
         -> Int     -- label
         -> [(Int, Abc.OpCode)]
         -> [(Int, Abc.OpCode)]
addLabel i j l ts = newZipper
  where
    (as, bs) = splitAt (i+1) ts
    (as2, (_, op):bs2) = jump j (reverse as, bs)
    newZipper = reverse as2 ++ (l, op):bs2

jump :: Abc.S24
     -> ([(Int, Abc.OpCode)], [(Int, Abc.OpCode)])
     -> ([(Int, Abc.OpCode)], [(Int, Abc.OpCode)])
jump s24 tuple
  | s24 > 0 = pos_jump s24 tuple
  | s24 < 0 = neg_jump s24 tuple
  | otherwise = tuple

pos_jump :: Abc.S24
         -> ([(Int, Abc.OpCode)], [(Int, Abc.OpCode)])
         -> ([(Int, Abc.OpCode)], [(Int, Abc.OpCode)])
pos_jump _ (aops, []) = (aops, [])
pos_jump s24 t@(aops, (m, bop):bops)
  | s24 > 0 = pos_jump (s24 - (fromIntegral $ toBytes bop)) ((m, bop):aops, bops)
  | otherwise = t

neg_jump :: Abc.S24
         -> ([(Int, Abc.OpCode)], [(Int, Abc.OpCode)])
         -> ([(Int, Abc.OpCode)], [(Int, Abc.OpCode)])
neg_jump _ ([], bops) = ([], bops)
neg_jump s24 t@((m, aop):aops, bops)
  | s24 < 0 = neg_jump (s24 + (fromIntegral $ toBytes aop)) (aops, (m, aop):bops)
  | otherwise = t
