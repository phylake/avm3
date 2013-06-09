module LLVM.Passes.Branch (insertLabels, testInsertLabels) where

import Abc.Def as Abc
import Abc.Util (toBytes)
import LLVM.Lang

{-
  TT   true target
  FT   false target
  Dest destination
-}
data BranchPrim  = ConditionalP Int Int | JumpP Int | DestP Int | NoPrim
data BranchPrim2 = ConditionalP2 Label Label | JumpP2 Label | DestP2 Label | NoPrim2

type BranchTags = [(BranchPrim, Abc.OpCode)]
type BranchTags2 = [(BranchPrim2, Abc.OpCode)]

testInsertLabels :: [Abc.OpCode] -> IO ()
testInsertLabels ops = do
  mapM_ f $ insertLabels ops
  where
    f (DestP2 l, op) = putStrLn $ show l ++ "\n\t" ++ show op
    f (ConditionalP2 t f, op) = putStrLn $ "\t" ++ show op ++ "    " ++ show t ++ " " ++ show f
    f (JumpP2 l, op) = putStrLn $ "\t" ++ show op ++ "    " ++ show l
    f (_, op) = putStrLn $ "\t" ++ show op

insertLabels :: [Abc.OpCode] -> BranchTags2
insertLabels = falseBranch . prependEntry . intLabelsToString . insertMarkers

falseBranch :: BranchTags2 -> BranchTags2
falseBranch (c@(ConditionalP2 t f, _):(_, op):ts) = [c, (DestP2 f, op)] ++ falseBranch ts
falseBranch (c@(_, _):ts) = [c] ++ falseBranch ts
falseBranch [] = []

maxLabel :: BranchTags2 -> Int
maxLabel ((ConditionalP2 (L _ t) (L _ f),_):ts) = max t $ max f $ maxLabel ts
maxLabel ((JumpP2 (L _ t),_):ts) = max t $ maxLabel ts
maxLabel ((DestP2 (L _ t),_):ts) = max t $ maxLabel ts
maxLabel ((NoPrim2,_):ts) = maxLabel ts
maxLabel [] = 0

prependEntry :: BranchTags2 -> BranchTags2
prependEntry ((NoPrim2, op):ts) = (DestP2 $ L "entry" 0, op):ts

intLabelsToString :: BranchTags -> BranchTags2
intLabelsToString = map toLabel
  where
    toLabel (NoPrim, op) = (NoPrim2, op)
    toLabel (ConditionalP t f, op) = (ConditionalP2 (L "L" t) (L "L" f), op)
    toLabel (JumpP t, op) = (JumpP2 (L "L" t), op)
    toLabel (DestP t, op) = (DestP2 (L "L" t), op)

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
