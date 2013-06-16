module LLVM.Compile where

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

test :: IO ()
test = do
  emitLLVM abc >>= mapM (putStrLn . show)
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
        Abc.Multiname_QName 1 4 -- return type "Boolean"
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

theops :: [Abc.OpCode]
theops =
  [
    Abc.PushInt 2     -- 
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
