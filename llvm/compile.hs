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
  --mapM_ (putStrLn . show) $ emitLLVM abc
  return ()

op2String :: [Abc.OpCode] -> [[String]]

-- assuming %local_* are already allocaed
op2String (Abc.PushInt i:Abc.SetLocal2:ops) = ["store i32 " ++ show i ++ ", i32 * %local_2"] : op2String ops

-- assuming %local_* are already allocaed
op2String (Abc.PushByte i:Abc.SetLocal3:ops) = ["store i32 " ++ show i ++ ", i32 * %local_3"] : op2String ops
op2String (Abc.Jump i:ops) = ["br label %L1"] : op2String ops
op2String (Abc.Label:ops) = ["L1:"] : op2String ops

op2String (Abc.GetLocal2:ops) = ["%T21 = load i32* %local_2"] : op2String ops
op2String (Abc.DecrementInt:ops) = ["%T22 = add i32 -1, %T21"] : op2String ops
op2String (Abc.SetLocal2:ops) = ["store i32 %T22, i32 * %local_2"] : op2String ops

-- translate IncLocalInt 3 to GetLocal3 IncrementInt SetLocal3 POST Label insertion
op2String (Abc.GetLocal3:ops) = ["%T31 = load i32* %local_3"] : op2String ops
op2String (Abc.IncrementInt:ops) = ["%T32 = add i32 1, %T31"] : op2String ops
op2String (Abc.SetLocal3:ops) = ["store i32 %T32, i32 * %local_3"] : op2String ops

op2String (Abc.GetLocal2:ops) = ["%T23 = load i32* %local_2"] : op2String ops
op2String (Abc.GetLocal3:ops) = ["%T33 = load i32* %local_3"] : op2String ops

-- need to insert Label after IfGreaterThan
op2String (Abc.IfGreaterThan i:ops) = ["%gt0 = icmp ugt i32 %T23, %T33", "br i1 %gt0, label %L1, label %L2", "L2:"] : op2String ops

-- for every register i need (1) the temporary register count and (2) the data type
op2String (Abc.GetLocal2:ops) = ["%T24 = load i32* %local_2"] : op2String ops
op2String (Abc.GetLocal3:ops) = ["%T34 = load i32* %local_3"] : op2String ops
-- will need to know the data types of the two things i'm adding as this will either be
-- a simple add or a call to an object's add function
op2String (Abc.Add:ops) = ["%add0 = add i32 %T24, %T34"] : op2String ops
op2String (Abc.ReturnValue:ops) = ["ret i32 %add0"] : op2String ops
op2String _ = []

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
