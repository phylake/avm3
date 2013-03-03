module Vm.Def where

import           Data.Hashable
import           Data.Int
import           Data.Word
import           Ecma.Prims
import           Util.Misc
import           Util.Words (u30Bytes)
import qualified Abc.Def as Abc
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
          | VmRtInternalInt Abc.U30

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

{- close to 1:1 transformation of Abc to an ADT -}
data VmAbc = VmAbc_Int Int32
           | VmAbc_Uint Abc.U30
           | VmAbc_Double Double
           | VmAbc_String B.ByteString
           | VmAbc_NsInfo Abc.NSInfo
           | VmAbc_NsSet Abc.NSSet
           | VmAbc_Multiname Abc.Multiname
           | VmAbc_MethodSig Abc.MethodSignature {- Maybe MethodBody -}
           | VmAbc_Metadata Abc.Metadata
           | VmAbc_Instance Abc.InstanceInfo
           | VmAbc_Class Abc.ClassInfo
           | VmAbc_Script Abc.ScriptInfo
           | VmAbc_MethodBody MethodBody
           deriving (Show)

{-
diff Abc.MethodSignature MethodSignature
  + maybeMethodBody
-}
{-data MethodSignature = MethodSignature {
                                         msReturnType :: Abc.MultinameIdx
                                       , msParamTypes :: [Abc.MultinameIdx]
                                       , msMethodName :: Abc.StringIdx -- debug
                                       , msFlags :: Word8
                                       , msOptionInfo :: Maybe [Abc.CPC]
                                       , msParamNames :: Maybe [Abc.StringIdx] -- debug
                                       , maybeMethodBody :: Maybe MethodBody
                                       }
                                       deriving (Show)-}

{-
TODO need to keep mbMethod until new MethodSignature comes online
diff Abc.MethodBody MethodBody
  - mbMethod
-}
data MethodBody = MethodBody {
                               mbMethod :: Abc.MethodSignatureIdx
                             , mbMaxStack :: Abc.U30
                             , mbLocalCount :: Abc.U30
                             , mbInitScopeDepth :: Abc.U30
                             , mbMaxScopeDepth :: Abc.U30
                             , mbCode :: [OpCode]
                             , mbExceptions :: [Abc.Exception]
                             , mbTraits :: [Abc.TraitsInfo]
                             }
                             deriving (Show)

data OpCode = {- 0x01 -} Breakpoint
            | {- 0x02 -} Nop
            | {- 0x03 -} Throw
            | {- 0x04 -} GetSuper Abc.MultinameIdx (Maybe B.ByteString)
            | {- 0x05 -} SetSuper Abc.MultinameIdx (Maybe B.ByteString)
            | {- 0x06 -} DefaultXmlNamespace Abc.U30
            | {- 0x07 -} DefaultXmlNamespaceL
            | {- 0x08 -} Kill Abc.U30
            | {- 0x09 -} Label
              {- 0x0A -}
              {- 0x0B -}
            | {- 0x0C -} IfNotLessThan Abc.S24
            | {- 0x0D -} IfNotLessEqual Abc.S24
            | {- 0x0E -} IfNotGreaterThan Abc.S24
            | {- 0x0F -} IfNotGreaterEqual Abc.S24
            | {- 0x10 -} Jump Abc.S24
            | {- 0x11 -} IfTrue Abc.S24
            | {- 0x12 -} IfFalse Abc.S24
            | {- 0x13 -} IfEqual Abc.S24
            | {- 0x14 -} IfNotEqual Abc.S24
            | {- 0x15 -} IfLessThan Abc.S24
            | {- 0x16 -} IfLessEqual Abc.S24
            | {- 0x17 -} IfGreaterThan Abc.S24
            | {- 0x18 -} IfGreaterEqual Abc.S24
            | {- 0x19 -} IfStrictEqual Abc.S24
            | {- 0x1A -} IfStrictNotEqual Abc.S24
            | {- 0x1B -} LookupSwitch Abc.S24 [Abc.S24] {- default offset, case offsets -}
            | {- 0x1C -} PushWith
            | {- 0x1D -} PopScope
            | {- 0x1E -} NextName
            | {- 0x1F -} HasNext
            | {- 0x20 -} PushNull
            | {- 0x21 -} PushUndefined
            | {- 0x22 -} PushConstant
            | {- 0x23 -} NextValue
            | {- 0x24 -} PushByte Abc.U8
            | {- 0x25 -} PushShort Abc.U30
            | {- 0x26 -} PushTrue
            | {- 0x27 -} PushFalse
            | {- 0x28 -} PushNaN
            | {- 0x29 -} Pop
            | {- 0x2A -} Dup
            | {- 0x2B -} Swap
            | {- 0x2C -} PushString Abc.StringIdx B.ByteString
            | {- 0x2D -} PushInt Abc.IntIdx Abc.S32
            | {- 0x2E -} PushUInt Abc.UintIdx Abc.U30
            | {- 0x2F -} PushDouble Abc.DoubleIdx Double
            | {- 0x30 -} PushScope
            | {- 0x31 -} PushNamespace Abc.NSInfoIdx
            | {- 0x32 -} HasNext2 Word32 Word32
            | {- 0x33 -} PushDecimal
            | {- 0x34 -} PushDNaN
              {- 0x35 -} {-GetByte-}
              {- 0x36 -} {-GetShort-}
              {- 0x37 -} {-GetInt-}
              {- 0x38 -} {-GetFloat-}
              {- 0x39 -} {-GetDouble-}
              {- 0x3A -} {-SetByte-}
              {- 0x3B -} {-SetShort-}
              {- 0x3C -} {-SetInt-}
              {- 0x3D -} {-SetFloat-}
              {- 0x3E -} {-SetDouble-}
              {- 0x3F -}
            | {- 0x40 -} NewFunction Abc.U30
            | {- 0x41 -} Call Abc.U30
            | {- 0x42 -} Construct Abc.U30
            | {- 0x43 -} CallMethod Abc.MultinameIdx Abc.U30 (Maybe B.ByteString)
            | {- 0x44 -} CallStatic Abc.MultinameIdx Abc.U30 (Maybe B.ByteString)
            | {- 0x45 -} CallSuper Abc.MultinameIdx Abc.U30 (Maybe B.ByteString)
            | {- 0x46 -} CallProperty Abc.MultinameIdx Abc.U30 (Maybe B.ByteString)
            | {- 0x47 -} ReturnVoid
            | {- 0x48 -} ReturnValue
            | {- 0x49 -} ConstructSuper Abc.U30
            | {- 0x4A -} ConstructProp Abc.MultinameIdx Abc.U30 (Maybe B.ByteString)
            | {- 0x4B -} CallSuperId
            | {- 0x4C -} CallPropLex Abc.MultinameIdx Abc.U30 (Maybe B.ByteString)
            | {- 0x4D -} CallInterface
            | {- 0x4E -} CallSuperVoid Abc.MultinameIdx Abc.U30 (Maybe B.ByteString)
            | {- 0x4F -} CallPropVoid Abc.MultinameIdx Abc.U30 (Maybe B.ByteString)
              {- 0x50 -} {-Sign1-}
              {- 0x51 -} {-Sign8-}
              {- 0x52 -} {-Sign16-}
            | {- 0x53 -} ApplyType
              {- 0x54 -}
            | {- 0x55 -} NewObject Abc.U30
            | {- 0x56 -} NewArray Abc.U30
            | {- 0x57 -} NewActivation
            | {- 0x58 -} NewClass Abc.ClassInfoIdx
            | {- 0x59 -} GetDescendants Abc.MultinameIdx (Maybe B.ByteString)
            | {- 0x5A -} NewCatch Abc.ExceptionIdx
            | {- 0x5B -} FindPropGlobalStrict
            | {- 0x5C -} FindPropGlobal
            | {- 0x5D -} FindPropStrict Abc.MultinameIdx (Maybe B.ByteString)
            | {- 0x5E -} FindProperty Abc.MultinameIdx (Maybe B.ByteString)
            | {- 0x5F -} FindDef
              {- 0x60 -} {-GetLex-} -- expanded into FindPropStrict idx + GetProperty idx
            | {- 0x61 -} SetProperty Abc.MultinameIdx (Maybe B.ByteString)
            | {- 0x62 -} GetLocal Abc.U30
            | {- 0x63 -} SetLocal Abc.U30
            | {- 0x64 -} GetGlobalScope
            | {- 0x65 -} GetScopeObject Abc.U8
            | {- 0x66 -} GetProperty Abc.MultinameIdx (Maybe B.ByteString)
            | {- 0x67 -} GetPropertyLate
            | {- 0x68 -} InitProperty Abc.MultinameIdx (Maybe B.ByteString)
            | {- 0x69 -} SetPropertyLate
            | {- 0x6A -} DeleteProperty Abc.MultinameIdx (Maybe B.ByteString)
            | {- 0x6B -} DeletePropertyLate
            | {- 0x6C -} GetSlot Abc.U30
            | {- 0x6D -} SetSlot Abc.U30
            | {- 0x6E -} GetGlobalSlot Abc.U30
            | {- 0x6F -} SetGlobalSlot Abc.U30
            | {- 0x70 -} ConvertString
            | {- 0x71 -} EscXmlElem
            | {- 0x72 -} EscXmlAttr
            | {- 0x73 -} ConvertInt
            | {- 0x74 -} ConvertUInt
            | {- 0x75 -} ConvertDouble
            | {- 0x76 -} ConvertBoolean
            | {- 0x77 -} ConvertObject
            | {- 0x78 -} CheckFilter
              {- 0x79 -} {-convert_m-}
              {- 0x7A -} {-convert_m_p-}
              {- 0x7B -}
              {- 0x7C -}
              {- 0x7D -}
              {- 0x7E -}
              {- 0x7F -}
            | {- 0x80 -} Coerce Abc.MultinameIdx (Maybe B.ByteString)
            | {- 0x81 -} CoerceBoolean
            | {- 0x82 -} CoerceAny
            | {- 0x83 -} CoerceInt
            | {- 0x84 -} CoerceDouble
            | {- 0x85 -} CoerceString
            | {- 0x86 -} AsType Abc.MultinameIdx (Maybe B.ByteString)
            | {- 0x87 -} AsTypeLate
            | {- 0x88 -} CoerceUInt
            | {- 0x89 -} CoerceObject
              {- 0x8A -}
              {- 0x8B -}
              {- 0x8C -}
              {- 0x8D -}
              {- 0x8E -}
              {- 0x8F -} {-negate_p-}
            | {- 0x90 -} Negate
            | {- 0x91 -} Increment
            | {- 0x92 -} IncLocal
            | {- 0x93 -} Decrement
            | {- 0x94 -} DecLocal Abc.U30
            | {- 0x95 -} TypeOf
            | {- 0x96 -} Not
            | {- 0x97 -} BitNot
              {- 0x98 -}
              {- 0x99 -}
            | {- 0x9A -} Concat
            | {- 0x9B -} AddDouble
              {- 0x9C -} {-increment_p-}
              {- 0x9D -} {-inclocal_p-}
              {- 0x9E -} {-decrement_p-}
              {- 0x9F -} {-declocal_p-}
            | {- 0xA0 -} Add
            | {- 0xA1 -} Subtract
            | {- 0xA2 -} Multiply
            | {- 0xA3 -} Divide
            | {- 0xA4 -} Modulo
            | {- 0xA5 -} ShiftLeft
            | {- 0xA6 -} ShiftRight
            | {- 0xA7 -} ShiftRightUnsigned
            | {- 0xA8 -} BitAnd
            | {- 0xA9 -} BitOr
            | {- 0xAA -} BitXor
            | {- 0xAB -} Equals
            | {- 0xAC -} StrictEquals
            | {- 0xAD -} LessThan
            | {- 0xAE -} LessEquals
            | {- 0xAF -} GreaterThan
            | {- 0xB0 -} GreaterEquals
            | {- 0xB1 -} InstanceOf
            | {- 0xB2 -} IsType Abc.MultinameIdx (Maybe B.ByteString)
            | {- 0xB3 -} IsTypeLate
            | {- 0xB4 -} In
              {- 0xB5 -}
              {- 0xB6 -}
              {- 0xB7 -}
              {- 0xB8 -}
              {- 0xB9 -}
              {- 0xBA -}
              {- 0xBB -}
              {- 0xBC -}
              {- 0xBD -}
              {- 0xBE -}
              {- 0xBF -}
            | {- 0xC0 -} IncrementInt
            | {- 0xC1 -} DecrementInt
            | {- 0xC2 -} IncLocalInt Abc.U30
            | {- 0xC3 -} DecLocalInt Abc.U30
            | {- 0xC4 -} NegateInt
            | {- 0xC5 -} AddInt
            | {- 0xC6 -} SubtractInt
            | {- 0xC7 -} MultiplyInt
              {- 0xC8 -}
              {- 0xC9 -}
              {- 0xCA -}
              {- 0xCB -}
              {- 0xCC -}
              {- 0xCD -}
              {- 0xCE -}
              {- 0xCF -}
            | {- 0xD0 -} GetLocal0
            | {- 0xD1 -} GetLocal1
            | {- 0xD2 -} GetLocal2
            | {- 0xD3 -} GetLocal3
            | {- 0xD4 -} SetLocal0
            | {- 0xD5 -} SetLocal1
            | {- 0xD6 -} SetLocal2
            | {- 0xD7 -} SetLocal3
              {- 0xD8 -}
              {- 0xD9 -}
              {- 0xDA -}
              {- 0xDB -}
              {- 0xDC -}
              {- 0xDD -}
              {- 0xDE -}
              {- 0xDF -}
              {- 0xE0 -}
              {- 0xE1 -}
              {- 0xE2 -}
              {- 0xE3 -}
              {- 0xE4 -}
              {- 0xE5 -}
              {- 0xE6 -}
              {- 0xE7 -}
              {- 0xE8 -}
              {- 0xE9 -}
              {- 0xEA -}
              {- 0xEB -}
              {- 0xEC -}
              {- 0xED -}
              {- 0xEE -} {-abs_jump-}
            | {- 0xEF -} Debug Abc.U8 Abc.StringIdx Abc.U8 Abc.U30
            | {- 0xF0 -} DebugLine Abc.U30
            | {- 0xF1 -} DebugFile Abc.StringIdx
            | {- 0xF2 -} BreakpointLine
              {- 0xF3 -} {-timestamp-}
              {- 0xF4 -}
              {- 0xF5 -} {-verifypass-}
              {- 0xF6 -} {-alloc-}
              {- 0xF7 -} {-mark-}
              {- 0xF8 -} {-wb-}
              {- 0xF9 -} {-prologue-}
              {- 0xFA -} {-sendenter-}
              {- 0xFB -} {-doubletoatom-}
              {- 0xFC -} {-sweep-}
              {- 0xFD -} {-codegenop-}
              {- 0xFE -} {-verifyop-}
              {- 0xFF -} {-decode-}
            deriving (Show, Eq)

toBytes :: OpCode -> Int
toBytes {- 0x01 -} (Breakpoint) = 1
toBytes {- 0x02 -} (Nop) = 1
toBytes {- 0x03 -} (Throw) = 1
toBytes {- 0x04 -} (GetSuper u30 _) = 1 + u30Bytes u30
toBytes {- 0x05 -} (SetSuper u30 _) = 1 + u30Bytes u30
toBytes {- 0x06 -} (DefaultXmlNamespace u30) = 1 + u30Bytes u30
toBytes {- 0x07 -} (DefaultXmlNamespaceL) = 1
toBytes {- 0x08 -} (Kill u30) = 1 + u30Bytes u30
toBytes {- 0x09 -} (Label) = 1
--toBytes   {- 0x0A -}
--toBytes   {- 0x0B -}
toBytes {- 0x0C -} (IfNotLessThan s24) = 1 + 3
toBytes {- 0x0D -} (IfNotLessEqual s24) = 1 + 3
toBytes {- 0x0E -} (IfNotGreaterThan s24) = 1 + 3
toBytes {- 0x0F -} (IfNotGreaterEqual s24) = 1 + 3
toBytes {- 0x10 -} (Jump s24) = 1 + 3
toBytes {- 0x11 -} (IfTrue s24) = 1 + 3
toBytes {- 0x12 -} (IfFalse s24) = 1 + 3
toBytes {- 0x13 -} (IfEqual s24) = 1 + 3
toBytes {- 0x14 -} (IfNotEqual s24) = 1 + 3
toBytes {- 0x15 -} (IfLessThan s24) = 1 + 3
toBytes {- 0x16 -} (IfLessEqual s24) = 1 + 3
toBytes {- 0x17 -} (IfGreaterThan s24) = 1 + 3
toBytes {- 0x18 -} (IfGreaterEqual s24) = 1 + 3
toBytes {- 0x19 -} (IfStrictEqual s24) = 1 + 3
toBytes {- 0x1A -} (IfStrictNotEqual s24) = 1 + 3
toBytes {- 0x1B -} (LookupSwitch s24 s24s) = 1 + 3 + (Prelude.length s24s * 3)
toBytes {- 0x1C -} (PushWith) = 1
toBytes {- 0x1D -} (PopScope) = 1
toBytes {- 0x1E -} (NextName) = 1
toBytes {- 0x1F -} (HasNext) = 1
toBytes {- 0x20 -} (PushNull) = 1
toBytes {- 0x21 -} (PushUndefined) = 1
toBytes {- 0x22 -} (PushConstant) = 1
toBytes {- 0x23 -} (NextValue) = 1
toBytes {- 0x24 -} (PushByte u8) = 2
toBytes {- 0x25 -} (PushShort u30) = 1 + u30Bytes u30
toBytes {- 0x26 -} (PushTrue) = 1
toBytes {- 0x27 -} (PushFalse) = 1
toBytes {- 0x28 -} (PushNaN) = 1
toBytes {- 0x29 -} (Pop) = 1
toBytes {- 0x2A -} (Dup) = 1
toBytes {- 0x2B -} (Swap) = 1
toBytes {- 0x2C -} (PushString u30 _) = 1 + u30Bytes u30
toBytes {- 0x2D -} (PushInt u30 _) = 1 + u30Bytes u30
toBytes {- 0x2E -} (PushUInt u30 _) = 1 + u30Bytes u30
toBytes {- 0x2F -} (PushDouble u30 _) = 1 + u30Bytes u30
toBytes {- 0x30 -} (PushScope) = 1
toBytes {- 0x31 -} (PushNamespace u30) = 1 + u30Bytes u30
toBytes {- 0x32 -} (HasNext2 w32 w32_2) = 1 + 4 + 4
toBytes {- 0x33 -} (PushDecimal) = 1
toBytes {- 0x34 -} (PushDNaN) = 1
--toBytes   {- 0x35 -} {-GetByte-}
--toBytes   {- 0x36 -} {-GetShort-}
--toBytes   {- 0x37 -} {-GetInt-}
--toBytes   {- 0x38 -} {-GetFloat-}
--toBytes   {- 0x39 -} {-GetDouble-}
--toBytes   {- 0x3A -} {-SetByte-}
--toBytes   {- 0x3B -} {-SetShort-}
--toBytes   {- 0x3C -} {-SetInt-}
--toBytes   {- 0x3D -} {-SetFloat-}
--toBytes   {- 0x3E -} {-SetDouble-}
--toBytes   {- 0x3F -}
toBytes {- 0x40 -} (NewFunction u30) = 1 + u30Bytes u30
toBytes {- 0x41 -} (Call u30) = 1 + u30Bytes u30
toBytes {- 0x42 -} (Construct u30) = 1 + u30Bytes u30
toBytes {- 0x43 -} (CallMethod u30_1 u30_2 _) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x44 -} (CallStatic u30_1 u30_2 _) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x45 -} (CallSuper u30_1 u30_2 _) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x46 -} (CallProperty u30_1 u30_2 _) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x47 -} (ReturnVoid) = 1
toBytes {- 0x48 -} (ReturnValue) = 1
toBytes {- 0x49 -} (ConstructSuper u30) = 1 + u30Bytes u30
toBytes {- 0x4A -} (ConstructProp u30_1 u30_2 _) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x4B -} (CallSuperId) = 1
toBytes {- 0x4C -} (CallPropLex u30_1 u30_2 _) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x4D -} (CallInterface) = 1
toBytes {- 0x4E -} (CallSuperVoid u30_1 u30_2 _) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x4F -} (CallPropVoid u30_1 u30_2 _) = 1 + u30Bytes u30_1 + u30Bytes u30_2
--toBytes   {- 0x50 -} {-Sign1-}
--toBytes   {- 0x51 -} {-Sign8-}
--toBytes   {- 0x52 -} {-Sign16-}
toBytes {- 0x53 -} (ApplyType) = 1
toBytes {- 0x55 -} (NewObject u30) = 1 + u30Bytes u30
toBytes {- 0x56 -} (NewArray u30) = 1 + u30Bytes u30
toBytes {- 0x57 -} (NewActivation) = 1
toBytes {- 0x58 -} (NewClass u30) = 1 + u30Bytes u30
toBytes {- 0x59 -} (GetDescendants u30 _) = 1 + u30Bytes u30
toBytes {- 0x5A -} (NewCatch u30) = 1 + u30Bytes u30
toBytes {- 0x5B -} (FindPropGlobalStrict) = 1
toBytes {- 0x5C -} (FindPropGlobal) = 1
toBytes {- 0x5D -} (FindPropStrict u30 _) = 1 + u30Bytes u30
toBytes {- 0x5E -} (FindProperty u30 _) = 1 + u30Bytes u30
toBytes {- 0x5F -} (FindDef) = 1
--toBytes   {- 0x60 -} {-GetLex-}
toBytes {- 0x61 -} (SetProperty u30 _) = 1 + u30Bytes u30
toBytes {- 0x62 -} (GetLocal u30) = 1 + u30Bytes u30
toBytes {- 0x63 -} (SetLocal u30) = 1 + u30Bytes u30
toBytes {- 0x64 -} (GetGlobalScope) = 1
toBytes {- 0x65 -} (GetScopeObject u8) = 1
toBytes {- 0x66 -} (GetProperty u30 _) = 1 + u30Bytes u30
toBytes {- 0x67 -} (GetPropertyLate) = 1
toBytes {- 0x68 -} (InitProperty u30 _) = 1 + u30Bytes u30
toBytes {- 0x69 -} (SetPropertyLate) = 1
toBytes {- 0x6A -} (DeleteProperty u30 _) = 1 + u30Bytes u30
toBytes {- 0x6B -} (DeletePropertyLate) = 1
toBytes {- 0x6C -} (GetSlot u30) = 1 + u30Bytes u30
toBytes {- 0x6D -} (SetSlot u30) = 1 + u30Bytes u30
toBytes {- 0x6E -} (GetGlobalSlot u30) = 1 + u30Bytes u30
toBytes {- 0x6F -} (SetGlobalSlot u30) = 1 + u30Bytes u30
toBytes {- 0x70 -} (ConvertString) = 1
toBytes {- 0x71 -} (EscXmlElem) = 1
toBytes {- 0x72 -} (EscXmlAttr) = 1
toBytes {- 0x73 -} (ConvertInt) = 1
toBytes {- 0x74 -} (ConvertUInt) = 1
toBytes {- 0x75 -} (ConvertDouble) = 1
toBytes {- 0x76 -} (ConvertBoolean) = 1
toBytes {- 0x77 -} (ConvertObject) = 1
toBytes {- 0x78 -} (CheckFilter) = 1
--toBytes   {- 0x79 -} {-convert_m-}
--toBytes   {- 0x7A -} {-convert_m_p-}
--toBytes   {- 0x7B -}
--toBytes   {- 0x7C -}
--toBytes   {- 0x7D -}
--toBytes   {- 0x7E -}
--toBytes   {- 0x7F -}
toBytes {- 0x80 -} (Coerce u30 _) = 1 + u30Bytes u30
toBytes {- 0x81 -} (CoerceBoolean) = 1
toBytes {- 0x82 -} (CoerceAny) = 1
toBytes {- 0x83 -} (CoerceInt) = 1
toBytes {- 0x84 -} (CoerceDouble) = 1
toBytes {- 0x85 -} (CoerceString) = 1
toBytes {- 0x86 -} (AsType u30 _) = 1 + u30Bytes u30
toBytes {- 0x87 -} (AsTypeLate) = 1
toBytes {- 0x88 -} (CoerceUInt) = 1
toBytes {- 0x89 -} (CoerceObject) = 1
--toBytes   {- 0x8A -}
--toBytes   {- 0x8B -}
--toBytes   {- 0x8C -}
--toBytes   {- 0x8D -}
--toBytes   {- 0x8E -}
--toBytes   {- 0x8F -} {-negate_p-}
toBytes {- 0x90 -} (Negate) = 1
toBytes {- 0x91 -} (Increment) = 1
toBytes {- 0x92 -} (IncLocal) = 1
toBytes {- 0x93 -} (Decrement) = 1
toBytes {- 0x94 -} (DecLocal u30) = 1 + u30Bytes u30
toBytes {- 0x95 -} (TypeOf) = 1
toBytes {- 0x96 -} (Not) = 1
toBytes {- 0x97 -} (BitNot) = 1
--toBytes   {- 0x98 -}
--toBytes   {- 0x99 -}
toBytes {- 0x9A -} (Concat) = 1
toBytes {- 0x9B -} (AddDouble) = 1
--toBytes   {- 0x9C -} {-increment_p-}
--toBytes   {- 0x9D -} {-inclocal_p-}
--toBytes   {- 0x9E -} {-decrement_p-}
--toBytes   {- 0x9F -} {-declocal_p-}
toBytes {- 0xA0 -} (Add) = 1
toBytes {- 0xA1 -} (Subtract) = 1
toBytes {- 0xA2 -} (Multiply) = 1
toBytes {- 0xA3 -} (Divide) = 1
toBytes {- 0xA4 -} (Modulo) = 1
toBytes {- 0xA5 -} (ShiftLeft) = 1
toBytes {- 0xA6 -} (ShiftRight) = 1
toBytes {- 0xA7 -} (ShiftRightUnsigned) = 1
toBytes {- 0xA8 -} (BitAnd) = 1
toBytes {- 0xA9 -} (BitOr) = 1
toBytes {- 0xAA -} (BitXor) = 1
toBytes {- 0xAB -} (Equals) = 1
toBytes {- 0xAC -} (StrictEquals) = 1
toBytes {- 0xAD -} (LessThan) = 1
toBytes {- 0xAE -} (LessEquals) = 1
toBytes {- 0xAF -} (GreaterThan) = 1
toBytes {- 0xB0 -} (GreaterEquals) = 1
toBytes {- 0xB1 -} (InstanceOf) = 1
toBytes {- 0xB2 -} (IsType u30 _) = 1 + u30Bytes u30
toBytes {- 0xB3 -} (IsTypeLate) = 1
toBytes {- 0xB4 -} (In) = 1
--toBytes   {- 0xB5 -}
--toBytes   {- 0xB6 -}
--toBytes   {- 0xB7 -}
--toBytes   {- 0xB8 -}
--toBytes   {- 0xB9 -}
--toBytes   {- 0xBA -}
--toBytes   {- 0xBB -}
--toBytes   {- 0xBC -}
--toBytes   {- 0xBD -}
--toBytes   {- 0xBE -}
--toBytes   {- 0xBF -}
toBytes {- 0xC0 -} (IncrementInt) = 1
toBytes {- 0xC1 -} (DecrementInt) = 1
toBytes {- 0xC2 -} (IncLocalInt u30) = 1 + u30Bytes u30
toBytes {- 0xC3 -} (DecLocalInt u30) = 1 + u30Bytes u30
toBytes {- 0xC4 -} (NegateInt) = 1
toBytes {- 0xC5 -} (AddInt) = 1
toBytes {- 0xC6 -} (SubtractInt) = 1
toBytes {- 0xC7 -} (MultiplyInt) = 1
--toBytes   {- 0xC8 -}
--toBytes   {- 0xC9 -}
--toBytes   {- 0xCA -}
--toBytes   {- 0xCB -}
--toBytes   {- 0xCC -}
--toBytes   {- 0xCD -}
--toBytes   {- 0xCE -}
--toBytes   {- 0xCF -}
toBytes {- 0xD0 -} (GetLocal0) = 1
toBytes {- 0xD1 -} (GetLocal1) = 1
toBytes {- 0xD2 -} (GetLocal2) = 1
toBytes {- 0xD3 -} (GetLocal3) = 1
toBytes {- 0xD4 -} (SetLocal0) = 1
toBytes {- 0xD5 -} (SetLocal1) = 1
toBytes {- 0xD6 -} (SetLocal2) = 1
toBytes {- 0xD7 -} (SetLocal3) = 1
--toBytes   {- 0xD8 -}
--toBytes   {- 0xD9 -}
--toBytes   {- 0xDA -}
--toBytes   {- 0xDB -}
--toBytes   {- 0xDC -}
--toBytes   {- 0xDD -}
--toBytes   {- 0xDE -}
--toBytes   {- 0xDF -}
--toBytes   {- 0xE0 -}
--toBytes   {- 0xE1 -}
--toBytes   {- 0xE2 -}
--toBytes   {- 0xE3 -}
--toBytes   {- 0xE4 -}
--toBytes   {- 0xE5 -}
--toBytes   {- 0xE6 -}
--toBytes   {- 0xE7 -}
--toBytes   {- 0xE8 -}
--toBytes   {- 0xE9 -}
--toBytes   {- 0xEA -}
--toBytes   {- 0xEB -}
--toBytes   {- 0xEC -}
--toBytes   {- 0xED -}
--toBytes   {- 0xEE -} {-abs_jump-}
toBytes {- 0xEF -} (Debug u8_1 u30_1 u8_2 u30_2) = 3 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0xF0 -} (DebugLine u30) = 1 + u30Bytes u30
toBytes {- 0xF1 -} (DebugFile u30) = 1 + u30Bytes u30
toBytes {- 0xF2 -} (BreakpointLine) = 1
--toBytes   {- 0xF3 -} {-timestamp-}
--toBytes   {- 0xF5 -} {-verifypass-}
--toBytes   {- 0xF6 -} {-alloc-}
--toBytes   {- 0xF7 -} {-mark-}
--toBytes   {- 0xF8 -} {-wb-}
--toBytes   {- 0xF9 -} {-prologue-}
--toBytes   {- 0xFA -} {-sendenter-}
--toBytes   {- 0xFB -} {-doubletoatom-}
--toBytes   {- 0xFC -} {-sweep-}
--toBytes   {- 0xFD -} {-codegenop-}
--toBytes   {- 0xFE -} {-verifyop-}
--toBytes   {- 0xFF -} {-decode-}
