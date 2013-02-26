module Vm.Def where

import           Abc.Def
import           Data.Hashable
import           Data.Int
import           Data.Word
import           Ecma.Prims
import           Util.Misc
import qualified Data.HashTable.IO as H
import qualified MonadLib as ML

type InstanceId = Word64
type ConstantPool = H.BasicHashTable String VmAbc
type VmObject = H.BasicHashTable VmRtP VmRt
type ScopeStack = [(VmObject, InstanceId)]
type Registers = H.BasicHashTable Int VmRt
type Ops = [VmRtOp]
type Heap = Int -- TODO need one of these ;)

type FunctionStack = [(Int, Ops, ScopeStack, Registers)]
type Execution = (ConstantPool, FunctionStack, InstanceId)

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
          | VmRt_Array [VmRt] InstanceId
          | VmRt_Object VmObject InstanceId{-RefCount-} {-(Maybe ScopeStack)-}
          | VmRt_Closure (Registers -> Ops -> AVM3 VmRt) -- curried r_f
          | VmRtInternalInt U30

instance Coerce VmRt where
  to_boolean VmRt_Undefined = False
  to_boolean VmRt_Null = False
  to_boolean (VmRt_Boolean a) = a
  to_boolean (VmRt_Int a) = a /= 0
  to_boolean (VmRt_Uint a) = a /= 0
  to_boolean (VmRt_Number a)
    | a == 0 = False
    | isNaN a = False
    | otherwise = True
  to_boolean (VmRt_String a) = length a > 0
  to_boolean (VmRt_Object _ _) = True

  to_number VmRt_Undefined = nan
  to_number VmRt_Null = 0
  to_number (VmRt_Boolean True) = 1
  to_number (VmRt_Boolean False) = 0
  to_number (VmRt_Int a) = fromIntegral a
  to_number (VmRt_Uint a) = fromIntegral a
  to_number (VmRt_Number a) = a
  to_number (VmRt_String a) = read a
  to_number (VmRt_Object _ _) = undefined

  to_int32 VmRt_Undefined = 0
  to_int32 VmRt_Null = 0
  to_int32 (VmRt_Boolean True) = 1
  to_int32 (VmRt_Boolean False) = 0
  to_int32 (VmRt_Int a) = a
  to_int32 (VmRt_Uint a) = fromIntegral a
  to_int32 (VmRt_Number a) = fromIntegral a
  to_int32 (VmRt_String a) = read a
  to_int32 (VmRt_Object _ a) = fromIntegral a

  to_uint32 VmRt_Undefined = 0
  to_uint32 VmRt_Null = 0
  to_uint32 (VmRt_Boolean True) = 1
  to_uint32 (VmRt_Boolean False) = 0
  to_uint32 (VmRt_Int a) = fromIntegral a
  to_uint32 (VmRt_Uint a) = a
  to_uint32 (VmRt_Number a) = fromIntegral a
  to_uint32 (VmRt_String a) = read a
  to_uint32 (VmRt_Object _ a) = fromIntegral a

  to_string VmRt_Undefined = "undefined"
  to_string VmRt_Null = "null"
  to_string (VmRt_Boolean True) = "true"
  to_string (VmRt_Boolean False) = "false"
  to_string (VmRt_Int a) = show a
  to_string (VmRt_Uint a) = show a
  to_string (VmRt_Number a) = show a
  to_string (VmRt_String a) = a
  to_string (VmRt_Object _ _) = "[Object]"

instance Eq VmRt where
  VmRt_Undefined == VmRt_Undefined = True
  VmRt_Undefined == VmRt_Null = True
  VmRt_Null == VmRt_Null = True
  VmRt_Null == VmRt_Undefined = True
  
  (VmRt_Int a) == (VmRt_Int b) = a == b
  (VmRt_Int a) == (VmRt_Uint b) = a == fromIntegral b
  (VmRt_Int a) == (VmRt_Number b) = a == fromIntegral b
  
  (VmRt_Uint a) == (VmRt_Uint b) = a == b
  (VmRt_Uint a) == (VmRt_Int b) = a == fromIntegral b
  (VmRt_Uint a) == (VmRt_Number b) = a == fromIntegral b
  
  (VmRt_Number a) == (VmRt_Number b) = a == b
  (VmRt_Number a) == (VmRt_Int b) = a == fromIntegral b
  (VmRt_Number a) == (VmRt_Uint b) = a == fromIntegral b
  
  (VmRt_String a) == (VmRt_String b) = a == b
  (VmRt_String a) == (VmRt_Number b) = a == show b
  (VmRt_String a) == (VmRt_Int b) = a == show b
  (VmRt_String a) == (VmRt_Uint b) = a == show b
  
  (VmRt_Array _ a) == (VmRt_Array _ b) = a == b
  (VmRt_Object _ a) == (VmRt_Object _ b) = a == b

instance Ord VmRt where
  compare VmRt_Undefined VmRt_Undefined = EQ
  compare VmRt_Undefined VmRt_Null = EQ

  compare VmRt_Null VmRt_Undefined = EQ
  compare VmRt_Null VmRt_Null = EQ

  compare (VmRt_Boolean a) (VmRt_Boolean b) = compare a b
  
  compare (VmRt_Int a) (VmRt_Int b) = compare a b
  compare (VmRt_Int a) (VmRt_Uint b) = compare a $ fromIntegral b
  compare (VmRt_Int a) (VmRt_Number b) = compare a $ fromIntegral b

  compare (VmRt_Uint a) (VmRt_Uint b) = compare a b
  compare (VmRt_Uint a) (VmRt_Int b) = compare a $ fromIntegral b
  compare (VmRt_Uint a) (VmRt_Number b) = compare a $ fromIntegral b

  compare (VmRt_Number a) (VmRt_Number b) = compare a b
  compare (VmRt_Number a) (VmRt_Int b) = compare a $ fromIntegral b
  compare (VmRt_Number a) (VmRt_Uint b) = compare a $ fromIntegral b

  compare (VmRt_Array _ a) (VmRt_Array _ b) = compare a b
  
  compare (VmRt_String a) (VmRt_String b) = compare a b
  
  compare (VmRt_Object _ a) (VmRt_Object _ b) = compare a b

instance Num VmRt where
  (VmRt_Int a) + (VmRt_Int b) = VmRt_Int$ a + b
  (VmRt_Int a) + (VmRt_Uint b) = VmRt_Int$ a + fromIntegral b
  (VmRt_Int a) + (VmRt_Number b) = VmRt_Number$ fromIntegral a + b

  (VmRt_Uint a) + (VmRt_Uint b) = VmRt_Uint$ a + b
  (VmRt_Uint a) + (VmRt_Int b) = VmRt_Uint$ a + fromIntegral b
  (VmRt_Uint a) + (VmRt_Number b) = VmRt_Number$ fromIntegral a + b

  (VmRt_Number a) + (VmRt_Number b) = VmRt_Number$ a + b
  (VmRt_Number a) + (VmRt_Int b) = VmRt_Number$ a + fromIntegral b
  (VmRt_Number a) + (VmRt_Uint b) = VmRt_Number$ a + fromIntegral b

  
  (VmRt_Int a) - (VmRt_Int b) = VmRt_Int$ a - b
  (VmRt_Int a) - (VmRt_Uint b) = VmRt_Int$ a - fromIntegral b
  (VmRt_Int a) - (VmRt_Number b) = VmRt_Number$ fromIntegral a - b

  (VmRt_Uint a) - (VmRt_Uint b) = VmRt_Uint$ a - b
  (VmRt_Uint a) - (VmRt_Int b) = VmRt_Uint$ a - fromIntegral b
  (VmRt_Uint a) - (VmRt_Number b) = VmRt_Number$ fromIntegral a - b

  (VmRt_Number a) - (VmRt_Number b) = VmRt_Number$ a - b
  (VmRt_Number a) - (VmRt_Int b) = VmRt_Number$ a - fromIntegral b
  (VmRt_Number a) - (VmRt_Uint b) = VmRt_Number$ a - fromIntegral b

  
  (VmRt_Int a) * (VmRt_Int b) = VmRt_Int$ a * b
  (VmRt_Int a) * (VmRt_Uint b) = VmRt_Int$ a * fromIntegral b
  (VmRt_Int a) * (VmRt_Number b) = VmRt_Number$ fromIntegral a * b

  (VmRt_Uint a) * (VmRt_Uint b) = VmRt_Uint$ a * b
  (VmRt_Uint a) * (VmRt_Int b) = VmRt_Uint$ a * fromIntegral b
  (VmRt_Uint a) * (VmRt_Number b) = VmRt_Number$ fromIntegral a * b

  (VmRt_Number a) * (VmRt_Number b) = VmRt_Number$ a * b
  (VmRt_Number a) * (VmRt_Int b) = VmRt_Number$ a * fromIntegral b
  (VmRt_Number a) * (VmRt_Uint b) = VmRt_Number$ a * fromIntegral b

  
  abs (VmRt_Number a) = abs$ fromIntegral a
  abs (VmRt_Int a) = abs$ fromIntegral a
  abs (VmRt_Uint a) = abs$ fromIntegral a

  signum (VmRt_Number a) = signum$ fromIntegral a
  signum (VmRt_Int a) = signum$ fromIntegral a
  signum (VmRt_Uint a) = signum$ fromIntegral a

  fromInteger = VmRt_Int . fromIntegral

instance Fractional Int32 where
  a / b = fromIntegral a / fromIntegral b

instance Fractional Word32 where
  a / b = fromIntegral a / fromIntegral b

instance Fractional VmRt where
  (VmRt_Int a) / (VmRt_Int b) = VmRt_Int$ a / b
  (VmRt_Int a) / (VmRt_Uint b) = VmRt_Int$ a / fromIntegral b
  (VmRt_Int a) / (VmRt_Number b) = VmRt_Number$ fromIntegral a / b

  (VmRt_Uint a) / (VmRt_Uint b) = VmRt_Uint$ a / b
  (VmRt_Uint a) / (VmRt_Int b) = VmRt_Uint$ a / fromIntegral b
  (VmRt_Uint a) / (VmRt_Number b) = VmRt_Number$ fromIntegral a / b
  
  --fromRational = VmRt_Number . fromIntegral

instance Show VmRt where
  show VmRt_Undefined      = "VmRt_Undefined"
  show VmRt_Null           = "VmRt_Null"
  show (VmRt_Boolean a)    = "VmRt_Boolean " ++ show a
  show (VmRt_Int a)        = "VmRt_Int " ++ show a
  show (VmRt_Uint a)       = "VmRt_Uint " ++ show a
  show (VmRt_Number a)     = "VmRt_Number " ++ show a
  show (VmRt_String a)     = "VmRt_String " ++ show a
  show (VmRt_Array a _)    = "VmRt_Array " ++ show a
  show (VmRt_Object a _)   = "VmRt_Object [Object]"
  show (VmRt_Closure _)    = "VmRt_Closure"
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
  (cp, fs, iid) <- get
  set (cp, f:fs, iid)

pop_activation :: AVM3 ()
pop_activation = do
  (cp, (_:fs), iid) <- get
  set (cp, fs, iid)

push :: VmRtOp -> AVM3 ()
push op = do
  (cp, ((sp,ops,ss,reg):fs), iid) <- get
  set (cp, ((sp+1,op:ops,ss,reg):fs), iid)

pushd :: VmRt -> AVM3 ()
pushd = push . D

pop :: AVM3 VmRtOp
pop = do
  (cp, ((sp,(op:ops),ss,reg):fs), iid) <- get
  if sp-1 < 0
    then ML.raise "pop would cause sp to be -1"
    else return ()
  set (cp, ((sp-1,ops,ss,reg):fs), iid)
  return op

-- InstanceId

next_iid :: AVM3 InstanceId
next_iid = do
  (cp, fs, iid) <- get
  set (cp, fs, iid+1)
  return$ iid+1

-- ConstantPool

get_cp :: AVM3 ConstantPool
get_cp = get >>= return. t31

set_cp :: ConstantPool -> AVM3 ()
set_cp cp = do
  (_,ops,iid) <- get
  set (cp,ops,iid)

-- Ops

get_ops :: AVM3 Ops
get_ops = do
  (cp, ((sp,ops,ss,reg):fs), iid) <- get
  return ops

mod_ops :: (Ops -> Ops) -> AVM3 ()
mod_ops f = do
  (cp, ((sp,ops,ss,reg):fs), iid) <- get
  set (cp, ((sp,f ops,ss,reg):fs), iid)

set_ops :: Ops -> AVM3 ()
set_ops ops = mod_ops$ \_ -> ops

-- StackPointer

mod_sp :: (Int -> Int) -> AVM3 ()
mod_sp f = do
  (cp, ((sp,ops,ss,reg):fs), iid) <- get
  set (cp, ((f sp,ops,ss,reg):fs), iid)

set_sp :: Int -> AVM3 ()
set_sp sp = mod_sp$ \_ -> sp

-- ScopeStack

get_ss :: AVM3 ScopeStack
get_ss = do
  (cp, ((a,b,ss,reg):fs), iid) <- get
  return ss

mod_ss :: (ScopeStack -> ScopeStack) -> AVM3 ()
mod_ss f = do
  (cp, ((a,b,ss,reg):fs), iid) <- get
  set (cp, (a,b,f ss,reg):fs, iid)

push_ss :: (VmObject, InstanceId) -> AVM3 ()
push_ss = mod_ss . (:)

pop_ss :: AVM3 ()
pop_ss = mod_ss tail

-- Registers

{-mod_reg :: (Registers -> Registers) -> AVM3 ()
mod_reg f = do
  (cp, ((a,b,ss,reg):fs), iid) <- get
  set (cp, (a,b,ss,f reg):fs, iid)-}
