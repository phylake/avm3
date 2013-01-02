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

{-
  Seriously considering a different representation of VmRt_Object to enable
  pure functions outside of monads in order to keep Emca.Prims classes simple
  (and implementable). Part of the entire reason to use haskell is to find the
  path of least resistance toward correctness. If i'm resorting to a series of
  string lookups I'm hardly leveraging the type system anymore

  What keeps bringing me back to the hashtable is all the name resolution I have
  to do

  The lookups are primarily on the ScopeStack (i.e. findprop). Could I do a
  transformation of ScopeStack VmObjects to VmRt_Objects of a pure type on
  which to operate?
-}

{-
TODO
 1) the primitives are objects but since they have a fixed set of immutable
    methods I can pattern match for things like callproperty along the lines of
    (D vmrt:O CallProperty):ops -> custom_method
 2) method closures
      Maybe ScopeStack ?
      (Registers -> Ops -> AVM3 VmRt) ?
 3) a heap: type RefCount = Int ?
 4) space/time: eagerly resolve all strings, merge method info, etc
-}
data VmRt = VmRt_Undefined
          | VmRt_Null
          | VmRt_Boolean Bool
          | VmRt_Int Int32
          | VmRt_Uint Word32
          | VmRt_Number Double
          | VmRt_String String
          | VmRt_Object VmObject {-RefCount-} {-(Maybe ScopeStack)-}
          | VmRt_Closure (Registers -> Ops -> AVM3 VmRt) -- curried r_f

instance Show VmRt where
  show VmRt_Undefined   = "VmRt_Undefined"
  show VmRt_Null        = "VmRt_Null"
  show (VmRt_Boolean a) = "VmRt_Boolean " ++ show a
  show (VmRt_Int a)     = "VmRt_Int " ++ show a
  show (VmRt_Uint a)    = "VmRt_Uint " ++ show a
  show (VmRt_Number a)  = "VmRt_Number " ++ show a
  show (VmRt_String a)  = "VmRt_String " ++ show a
  show (VmRt_Object a)  = "VmRt_Object [Object]"
  show (VmRt_Closure _) = "VmRt_Closure"

{- 1:1 transformation of Abc to an ADT -}
data VmAbc = VmAbc_Int Int32
           | VmAbc_Uint U30
           | VmAbc_Double Double
           | VmAbc_String String
           | VmAbc_NsInfo NSInfo
           | VmAbc_NsSet NSSet
           | VmAbc_Multiname Multiname
           | VmAbc_MethodSig MethodSignature {- Maybe MethodBody -}
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
