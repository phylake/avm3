module LLVM.Util.Helpers where

import           LLVM.Lang

obj :: D
obj = P I8

func :: D -> String -> [D] -> [Block] -> FunctionDef
func = FunctionDef Nothing Nothing Nothing

-- for manual bitcodes reify llvm ops in functions here
-- have these static modules return [TopStatement]
load :: D -> D
load (P d) = d

alloca :: D -> D
alloca = P
