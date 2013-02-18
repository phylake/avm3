module Vm.Def where

import           Abc.Def
import           Data.Hashable
import           Data.Int
import           Data.Word
import           Util.Misc
import qualified Data.HashTable.IO as H
import qualified MonadLib as ML

type ConstantPool = H.BasicHashTable String VmAbc
type VmObject = H.BasicHashTable VmRtP VmRt
type ScopeStack = [VmObject]
type Registers = [VmRt]
type Ops = [VmRtOp]
type Heap = Int -- TODO need one of these ;)

type FunctionStack = [(Int, Ops, ScopeStack, Registers)]
type Execution = (ConstantPool, FunctionStack)

type AVM3_State = ML.StateT Execution IO
type AVM3 = ML.ExceptionT String AVM3_State

data VmRtP = Ext String -- all run time, external properties
           | ClassIdx String -- the identity of the class
           deriving (Eq, Show)

instance Hashable VmRtP where
  hashWithSalt salt (Ext a) = hashWithSalt salt$ "Ext" ++ a
  hashWithSalt salt (ClassIdx a) = hashWithSalt salt$ "ClassIdx" ++ a

data VmRtOp = O OpCode
            | D VmRt
            deriving (Show)

{-
TODO
 1) the primitives are objects but since they have a fixed set of immutable
    methods I can pattern match for things like callproperty along the lines of
    (D (VmRt_Int int32):O CallProperty):ops -> custom_method
 2) method closures
      Maybe ScopeStack ?
      (Registers -> Ops -> AVM3 VmRt) ?
 3) a heap: type RefCount = Int ?
 4) space/time: eagerly resolve all strings, merge method info, etc
 5) move Traits off of scripts and onto the global object for find_property
-}
data VmRt = VmRt_Undefined
          | VmRt_Null
          | VmRt_Boolean Bool
          | VmRt_Int Int32
          | VmRt_Uint Word32
          | VmRt_Number Double
          | VmRt_String String
          | VmRt_Array [VmRt]
          | VmRt_Object VmObject {-RefCount-} {-(Maybe ScopeStack)-}
          | VmRt_Closure (Registers -> Ops -> AVM3 VmRt) -- curried r_f
          | VmRtInternalInt U30

instance Show VmRt where
  show VmRt_Undefined   = "VmRt_Undefined"
  show VmRt_Null        = "VmRt_Null"
  show (VmRt_Boolean a) = "VmRt_Boolean " ++ show a
  show (VmRt_Int a)     = "VmRt_Int " ++ show a
  show (VmRt_Uint a)    = "VmRt_Uint " ++ show a
  show (VmRt_Number a)  = "VmRt_Number " ++ show a
  show (VmRt_String a)  = "VmRt_String " ++ show a
  show (VmRt_Array a)   = "VmRt_Array " ++ show a
  show (VmRt_Object a)  = "VmRt_Object [Object]"
  show (VmRt_Closure _) = "VmRt_Closure"
  show (VmRtInternalInt a) = "VmRtInternalInt " ++ show a

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

{-
  Monad helpers
-}

get :: AVM3 Execution
get = ML.lift$ ML.get

set :: Execution -> AVM3 ()
set = ML.lift . ML.set

push_activation :: (Int, Ops, ScopeStack, Registers) -> AVM3 ()
push_activation f = do
  (cp, fs) <- get
  set (cp, f:fs)

pop_activation :: AVM3 ()
pop_activation = do
  (cp, (_:fs)) <- get
  set (cp, fs)

push :: VmRtOp -> AVM3 ()
push op = do
  (cp, ((sp,ops,ss,reg):fs)) <- get
  set (cp, ((sp+1,op:ops,ss,reg):fs))

pop :: AVM3 VmRtOp
pop = do
  (cp, ((sp,(op:ops),ss,reg):fs)) <- get
  if sp-1 < 0
    then ML.raise "pop would cause sp to be -1"
    else return ()
  set (cp, ((sp-1,ops,ss,reg):fs))
  return op

-- ConstantPool

get_cp :: AVM3 ConstantPool
get_cp = get >>= return. t21

set_cp :: ConstantPool -> AVM3 ()
set_cp cp = do
  (_,ops) <- get
  set (cp,ops)

-- Ops

get_ops :: AVM3 Ops
get_ops = do
  (cp, ((sp,ops,ss,reg):fs)) <- get
  return ops

mod_ops :: (Ops -> Ops) -> AVM3 ()
mod_ops f = do
  (cp, ((sp,ops,ss,reg):fs)) <- get
  set (cp, ((sp,f ops,ss,reg):fs))

set_ops :: Ops -> AVM3 ()
set_ops ops = mod_ops$ \_ -> ops

-- StackPointer

mod_sp :: (Int -> Int) -> AVM3 ()
mod_sp f = do
  (cp, ((sp,ops,ss,reg):fs)) <- get
  set (cp, ((f sp,ops,ss,reg):fs))

set_sp :: Int -> AVM3 ()
set_sp sp = mod_sp$ \_ -> sp

-- ScopeStack

get_ss :: AVM3 ScopeStack
get_ss = do
  (cp, ((a,b,ss,reg):fs)) <- get
  return ss

mod_ss :: (ScopeStack -> ScopeStack) -> AVM3 ()
mod_ss f = do
  (cp, ((a,b,ss,reg):fs)) <- get
  set (cp, (a,b,f ss,reg):fs)

push_ss :: VmObject -> AVM3 ()
push_ss = mod_ss . (:)

pop_ss :: AVM3 ()
pop_ss = mod_ss tail

-- Registers

mod_reg :: (Registers -> Registers) -> AVM3 ()
mod_reg f = do
  (cp, ((a,b,ss,reg):fs)) <- get
  set (cp, (a,b,ss,f reg):fs)







