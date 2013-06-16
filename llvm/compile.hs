module LLVM.Compile where

import LLVM.Passes.Branch

import           Abc.Def as Abc
import           Abc.Deserialize
import           LLVM.Emitter
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified MonadLib as ML

main :: IO ()
main = do
  abc <- E.run_ (EB.enumFile "abc/Test.abc" E.$$ parseAbc)
  emitLLVM abc >>= mapM_ (putStrLn . show)
  return ()

-- opt -std-compile-opts llvm/compiled.ll -o llvm/compiled.o && time ./llvm/compiled.o
-- opt -std-compile-opts -print-before-all llvm/compiled.ll &> llvm/compile.opt.ll
test :: IO ()
test = do
  testInsertLabels theops
  putStrLn "------------"
  emitLLVM abc >>= mapM (putStrLn . show)
  emitLLVM abc >>= mapM (return . show) >>= return . unlines >>= writeFile "llvm/compiled.ll"
  return ()
  where
    abc = Abc {
      abcInts = [0, 1, 1000000, 123456789]
    , abcUints = [0]
    , abcDoubles = [0, 1.209178234]
    , abcStrings = [
        "",
        "String",
        "Object",
        "void",
        "Boolean",
        "int",
        "Function",
        "Test",
        "foo",
        "Test/foo"
      ]
    , abcNsInfo = [
        Abc.NSInfo_Namespace 7 -- "Test"
      , Abc.NSInfo_Namespace 0 -- ""
      ]
    , abcNsSet = []
    , abcMultinames = [
        Abc.Multiname_QName 1 5 -- return type "int"
      ]
    , abcMethodSigs = [
        Abc.MethodSignature {
          msReturnType = 0
        , msParamTypes = []
        , msMethodName = 9
        , msFlags = 0
        , msOptionInfo = Nothing
        , msParamNames = Nothing
        }
      ]
    , abcMetadata = [
      ]
    , abcInstances = [
      ]
    , abcClasses = [
      ]
    , abcScripts = [
      ]
    , abcMethodBodies = [
        MethodBody {
          mbMethod = 0
        , mbMaxStack = 0
        , mbLocalCount = 0
        , mbInitScopeDepth = 0
        , mbMaxScopeDepth = 0
        , mbCode = theops
        , mbExceptions = []
        , mbTraits = []
        }
      ]
    }

{-i1
@Test_foo ()
{
entry0:
  %T1 = alloca i32
  store i32 1000000, i32* %T1
  store %T1, i32* %as_2
  %T2 = alloca i32
  store i32 0, i32* %T2
  store %T2, i32* %as_3
  br label %L1

L2:
  %T3 = load i32* %as_2
  %T4 = sub i32 %T3, 1
  store %T4, i32 %as_2
  %T5 = load i32* %as_3
  %T6 = add i32 %T5, 1
  store %T6, i32 %as_3

L1:
  %T7 = load i32* %as_2
  %T8 = load i32* %as_3

L3:
  %T9 = load i32* %as_2
  %T10 = load i32* %as_3

}
-}

theops :: [Abc.OpCode]
theops =
  [
    Abc.PushInt 2           --
  , Abc.SetLocal2           -- store i32 1000000, i32* %reg_2
  , Abc.PushByte 0          -- 
  , Abc.SetLocal3           -- store i32 0, i32* %reg_3
  , Abc.Jump 6              -- br label %L1; L2:
  , Abc.GetLocal2           -- %T21 = load i32* %reg_2
  , Abc.DecrementInt        -- %T22 = sub i32 %T21, 1
  , Abc.SetLocal2           -- store i32 %T22, i32* %reg_2
  , Abc.IncLocalInt 3       -- %T31 = load i32* %reg_3; %T32 = add i32 %T31, 1; store i32 %T32, i32* %reg_3
  , Abc.Label               -- L1:
  , Abc.GetLocal2           -- %T41 = load i32* %reg_2
  , Abc.GetLocal3           -- %T42 = load i32* %reg_3
  , Abc.IfGreaterThan (-12) -- %cond = icmp ugt i32 %T41, %T42; br i1 %cond, label %L2, label %L3; L3:
  , Abc.GetLocal2           -- %T51 = load i32* %reg_2
  , Abc.GetLocal3           -- %T52 = load i32* %reg_3
  , Abc.Add                 -- %T53 = add i32 %T51, %T52
  , Abc.ReturnValue         -- ret i32 %T53
  ]

{-
TODO passes in order
  - detect local register types and alloca them
  - look ahead for unary ops like Push* since they resolve to a single llvm op
  - i think binary ops only need the last 2 temp registers which are already
    captured in the StateT
  - detect types being added to know whether to call a function or just add
    and possibly insert arbitary new codes to keep transform to LLVMOp simple
-}

{-
many to many
push + any instruction == 1 AbcT.OpCode
inclocalint == 3 AbcT.OpCodes
-}
