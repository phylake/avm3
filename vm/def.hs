module Vm.Def where

import           Abc.Def
import           Data.Int
import           Data.Word
import           Util.Misc
import qualified Data.HashTable.IO as H
import qualified MonadLib as ML

type ConstantPool = H.BasicHashTable String VmAbc
type Execution = (Registers, Ops, ScopeStack, ConstantPool)
type AVM3_State = ML.StateT Execution IO
type AVM3 = ML.ExceptionT String AVM3_State

data HeapObject = Hn VmObject | H_func VmObject ScopeStack

type VmObject = H.BasicHashTable String VmRt
type ScopeStack = [VmObject]
type Registers = [VmRt]
type Ops = [VmRtOp]

data VmRtOp = O OpCode
            | D VmRt
            deriving (Show)

data VmRt = VmRt_Undefined
          | VmRt_Null
          | VmRt_Int Int32
          | VmRt_Uint U30
          | VmRt_Double Double
          | VmRt_String String
          | VmRt_Object VmObject
          deriving (Show)

{- 1:1 transformation of Abc to an ADT -}
data VmAbc = VmAbc_Int Int32
           | VmAbc_Uint U30
           | VmAbc_Double Double
           | VmAbc_String String
           | VmAbc_NsInfo NSInfo
           | VmAbc_NsSet NSSet
           | VmAbc_Multiname Multiname
           | VmAbc_MethodSig MethodSignature
           | VmAbc_Metadata Metadata
           | VmAbc_Instance InstanceInfo
           | VmAbc_Class ClassInfo
           | VmAbc_Script ScriptInfo
           | VmAbc_MethodBody MethodBody
           deriving (Show)

liftIO :: IO a -> AVM3 a
liftIO = ML.lift . ML.lift

get :: AVM3 Execution
get = ML.lift$ ML.get

set :: Execution -> AVM3 ()
set = ML.lift . ML.set

get_reg :: AVM3 Registers
get_reg = get >>= return. t41

set_reg :: Registers -> AVM3 ()
set_reg reg = do
  (_,ops,ss,cp) <- get
  set (reg,ops,ss,cp)

mod_reg :: (Registers -> Registers) -> AVM3 ()
mod_reg f = get_reg >>= return.f >>= set_reg

get_ops :: AVM3 Ops
get_ops = get >>= return. t42

mod_ops :: (Ops -> Ops) -> AVM3 ()
mod_ops f = get_ops >>= return.f >>= set_ops

set_ops :: Ops -> AVM3 ()
set_ops ops = do
  (reg,_,ss,cp) <- get
  set (reg,ops,ss,cp)

get_ss :: AVM3 ScopeStack
get_ss = get >>= return. t43

mod_ss :: (ScopeStack -> ScopeStack) -> AVM3 ()
mod_ss f = get_ss >>= return.f >>= set_ss

set_ss :: ScopeStack -> AVM3 ()
set_ss ss = do
  (reg,ops,_,cp) <- get
  set (reg,ops,ss,cp)

get_cp :: AVM3 ConstantPool
get_cp = get >>= return. t44

mod_cp :: (ConstantPool -> ConstantPool) -> AVM3 ()
mod_cp f = get_cp >>= return.f >>= set_cp

set_cp :: ConstantPool -> AVM3 ()
set_cp cp = do
  (reg,ops,ss,_) <- get
  set (reg,ops,ss,cp)
