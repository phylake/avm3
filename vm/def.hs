module Vm.Def where

import           Abc.Def as Abc
import           Data.Hashable
import           Data.Int
import           Data.Word
import           Ecma.Prims
import           Util.Misc
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashTable.IO as H

type VmObject = H.CuckooHashTable VmRtP VmRt

-- Part of FunctionStack
type ScopeStack = [(VmObject, InstanceId)] -- this is a tuple for purity
type Registers = H.CuckooHashTable Int VmRt
type D_Ops = [VmRt] -- data ops
type A_Ops = [OpCode] -- above stack pointer
type B_Ops = [OpCode] -- below stack pointer

-- Part of Execution
type ConstantPool = H.CuckooHashTable B.ByteString VmAbc
type InstanceId = Word64 -- Global instance id
type AVM3Exception = String -- Exception string

type Execution = (D_Ops, A_Ops, B_Ops, ScopeStack, Registers, ConstantPool, InstanceId)
type AVM3 = IO

data VmRtP = Ext B.ByteString -- all run time, external properties
           | ClassIdx B.ByteString -- the identity of the class
           deriving (Eq, Show)

-- consing to prevent hash collision
instance Hashable VmRtP where
  hashWithSalt salt (Ext a) = hashWithSalt salt$ B.cons 1 a
  hashWithSalt salt (ClassIdx a) = hashWithSalt salt$ B.cons 2 a

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
          | VmRt_String B.ByteString
          | VmRt_Array [VmRt] InstanceId
          | VmRt_Object VmObject InstanceId{-RefCount-} {-(Maybe ScopeStack)-}
          -- | VmRt_Closure -- TODO
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
  to_boolean (VmRt_String a) = B.length a > 0
  to_boolean (VmRt_Object _ _) = True

  to_number VmRt_Undefined = nan
  to_number VmRt_Null = 0
  to_number (VmRt_Boolean True) = 1
  to_number (VmRt_Boolean False) = 0
  to_number (VmRt_Int a) = fromIntegral a
  to_number (VmRt_Uint a) = fromIntegral a
  to_number (VmRt_Number a) = a
  to_number (VmRt_String a) = read$ BC.unpack a
  to_number (VmRt_Object _ _) = undefined

  to_int32 VmRt_Undefined = 0
  to_int32 VmRt_Null = 0
  to_int32 (VmRt_Boolean True) = 1
  to_int32 (VmRt_Boolean False) = 0
  to_int32 (VmRt_Int a) = a
  to_int32 (VmRt_Uint a) = fromIntegral a
  to_int32 (VmRt_Number a) = fromIntegral a
  to_int32 (VmRt_String a) = read$ BC.unpack a
  to_int32 (VmRt_Object _ a) = fromIntegral a

  to_uint32 VmRt_Undefined = 0
  to_uint32 VmRt_Null = 0
  to_uint32 (VmRt_Boolean True) = 1
  to_uint32 (VmRt_Boolean False) = 0
  to_uint32 (VmRt_Int a) = fromIntegral a
  to_uint32 (VmRt_Uint a) = a
  to_uint32 (VmRt_Number a) = fromIntegral a
  to_uint32 (VmRt_String a) = read$ BC.unpack a
  to_uint32 (VmRt_Object _ a) = fromIntegral a

  to_string VmRt_Undefined = "undefined"
  to_string VmRt_Null = "null"
  to_string (VmRt_Boolean True) = "true"
  to_string (VmRt_Boolean False) = "false"
  to_string (VmRt_Int a) = show a
  to_string (VmRt_Uint a) = show a
  to_string (VmRt_Number a) = show a
  to_string (VmRt_String a) = BC.unpack a
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

  {-(VmRt_String a) == (VmRt_String b) = a == b
  (VmRt_String a) == (VmRt_Number b) = a == show b
  (VmRt_String a) == (VmRt_Int b) = a == show b
  (VmRt_String a) == (VmRt_Uint b) = a == show b-}

  (VmRt_String a) == (VmRt_String b) = a == b
  (VmRt_String a) == (VmRt_Number b) = BC.unpack a == show b
  (VmRt_String a) == (VmRt_Int b) = BC.unpack a == show b
  (VmRt_String a) == (VmRt_Uint b) = BC.unpack a == show b

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
  --show (VmRt_Closure _)    = "VmRt_Closure"
  show (VmRtInternalInt a) = "VmRtInternalInt " ++ show a

{- 1:1 transformation of Abc to an ADT -}
data VmAbc = VmAbc_Int Int32
           | VmAbc_Uint U30
           | VmAbc_Double Double
           | VmAbc_String B.ByteString
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
