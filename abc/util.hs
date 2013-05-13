module Abc.Util (
  returnJ
, toStrict
, fromU8
, fromU16
, fromU16LE
, fromU32
, fromU32LE
, fromDouble
, fromDoubleLE
, fromU32LE_vl
, fromU30LE_vl
, fromS32LE_vl
, fromS24LE
) where

import Abc.Def
import Data.Enumerator as E
import Data.Enumerator.Binary as EB
import Data.Enumerator.List as EL
import Data.Int
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Util.Words hiding
  (
    fromU8
  , fromU16
  , fromU16LE
  , fromU32
  , fromU32LE
  , fromDouble
  , fromDoubleLE
  , fromU32LE_vl
  , fromU30LE_vl
  , fromS32LE_vl
  , fromS24LE
  )
import qualified Util.Words as Util
  (
    fromU8
  , fromU16
  , fromU16LE
  , fromU32
  , fromU32LE
  , fromDouble
  , fromDoubleLE
  , fromU32LE_vl
  , fromU30LE_vl
  , fromS32LE_vl
  , fromS24LE
  )

returnJ :: Monad m => a -> Iteratee b m (Maybe a)
returnJ = return . Just

toStrict :: BL.ByteString -> Parser B.ByteString
toStrict = return . B.pack . BL.unpack
{-# INLINE toStrict #-}

fromU8 :: Parser Word8
fromU8 = take' 1 >>= return . fst . Util.fromU8
{-# INLINE fromU8 #-}

fromU16 :: Parser Word16
fromU16 = take' 2 >>= return . fst . Util.fromU16
{-# INLINE fromU16 #-}

fromU16LE :: Parser Word16
fromU16LE = take' 2 >>= return . fst . Util.fromU16LE
{-# INLINE fromU16LE #-}

fromU32 :: Parser Word32
fromU32 = take' 4 >>= return . fst . Util.fromU32
{-# INLINE fromU32 #-}

fromU32LE :: Parser Word32
fromU32LE = take' 4 >>= return . fst . Util.fromU32LE
{-# INLINE fromU32LE #-}

fromDouble :: Parser Double
fromDouble = take' 8 >>= return . fst . Util.fromDouble
{-# INLINE fromDouble #-}

fromDoubleLE :: Parser Double
fromDoubleLE = take' 8 >>= return . fst . Util.fromDoubleLE
{-# INLINE fromDoubleLE #-}

fromU32LE_vl :: Parser Word32
fromU32LE_vl = do
  ws <- EB.takeWhile hasSignalBit
  w <- EB.take 1
  return . Util.fromU32LE_vl . BL.unpack $ BL.append ws w
{-# INLINE fromU32LE_vl #-}

fromU30LE_vl :: Parser Word32
fromU30LE_vl = do
  ws <- EB.takeWhile hasSignalBit
  w <- EB.take 1
  return . Util.fromU30LE_vl . BL.unpack $ BL.append ws w
{-# INLINE fromU30LE_vl #-}

fromS32LE_vl :: Parser Int32
fromS32LE_vl = do
  ws <- EB.takeWhile hasSignalBit
  w <- EB.take 1
  return . Util.fromS32LE_vl . BL.unpack $ BL.append ws w
{-# INLINE fromS32LE_vl #-}

fromS24LE :: Parser Int32
fromS24LE = EB.take 3 >>= return . Util.fromS24LE . BL.unpack
{-# INLINE fromS24LE #-}

take' :: Integer -> Parser B.ByteString
take' i = EB.take i >>= toStrict

toBytes :: OpCode -> Int
toBytes {- 0x01 -} (Breakpoint) = 1
toBytes {- 0x02 -} (Nop) = 1
toBytes {- 0x03 -} (Throw) = 1
toBytes {- 0x04 -} (GetSuper u30) = 1 + u30Bytes u30
toBytes {- 0x05 -} (SetSuper u30) = 1 + u30Bytes u30
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
toBytes {- 0x2C -} (PushString u30) = 1 + u30Bytes u30
toBytes {- 0x2D -} (PushInt u30) = 1 + u30Bytes u30
toBytes {- 0x2E -} (PushUInt u30) = 1 + u30Bytes u30
toBytes {- 0x2F -} (PushDouble u30) = 1 + u30Bytes u30
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
toBytes {- 0x43 -} (CallMethod u30_1 u30_2) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x44 -} (CallStatic u30_1 u30_2) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x45 -} (CallSuper u30_1 u30_2) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x46 -} (CallProperty u30_1 u30_2) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x47 -} (ReturnVoid) = 1
toBytes {- 0x48 -} (ReturnValue) = 1
toBytes {- 0x49 -} (ConstructSuper u30) = 1 + u30Bytes u30
toBytes {- 0x4A -} (ConstructProp u30_1 u30_2) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x4B -} (CallSuperId) = 1
toBytes {- 0x4C -} (CallPropLex u30_1 u30_2) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x4D -} (CallInterface) = 1
toBytes {- 0x4E -} (CallSuperVoid u30_1 u30_2) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x4F -} (CallPropVoid u30_1 u30_2) = 1 + u30Bytes u30_1 + u30Bytes u30_2
--toBytes   {- 0x50 -} {-Sign1-}
--toBytes   {- 0x51 -} {-Sign8-}
--toBytes   {- 0x52 -} {-Sign16-}
toBytes {- 0x53 -} (ApplyType) = 1
toBytes {- 0x55 -} (NewObject u30) = 1 + u30Bytes u30
toBytes {- 0x56 -} (NewArray u30) = 1 + u30Bytes u30
toBytes {- 0x57 -} (NewActivation) = 1
toBytes {- 0x58 -} (NewClass u30) = 1 + u30Bytes u30
toBytes {- 0x59 -} (GetDescendants u30) = 1 + u30Bytes u30
toBytes {- 0x5A -} (NewCatch u30) = 1 + u30Bytes u30
toBytes {- 0x5B -} (FindPropGlobalStrict) = 1
toBytes {- 0x5C -} (FindPropGlobal) = 1
toBytes {- 0x5D -} (FindPropStrict u30) = 1 + u30Bytes u30
toBytes {- 0x5E -} (FindProperty u30) = 1 + u30Bytes u30
toBytes {- 0x5F -} (FindDef) = 1
--toBytes   {- 0x60 -} {-GetLex-}
toBytes {- 0x61 -} (SetProperty u30) = 1 + u30Bytes u30
toBytes {- 0x62 -} (GetLocal u30) = 1 + u30Bytes u30
toBytes {- 0x63 -} (SetLocal u30) = 1 + u30Bytes u30
toBytes {- 0x64 -} (GetGlobalScope) = 1
toBytes {- 0x65 -} (GetScopeObject u8) = 1
toBytes {- 0x66 -} (GetProperty u30) = 1 + u30Bytes u30
toBytes {- 0x67 -} (GetPropertyLate) = 1
toBytes {- 0x68 -} (InitProperty u30) = 1 + u30Bytes u30
toBytes {- 0x69 -} (SetPropertyLate) = 1
toBytes {- 0x6A -} (DeleteProperty u30) = 1 + u30Bytes u30
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
toBytes {- 0x80 -} (Coerce u30) = 1 + u30Bytes u30
toBytes {- 0x81 -} (CoerceBoolean) = 1
toBytes {- 0x82 -} (CoerceAny) = 1
toBytes {- 0x83 -} (CoerceInt) = 1
toBytes {- 0x84 -} (CoerceDouble) = 1
toBytes {- 0x85 -} (CoerceString) = 1
toBytes {- 0x86 -} (AsType u30) = 1 + u30Bytes u30
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
toBytes {- 0x92 -} (IncLocal u30) = 1 + u30Bytes u30
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
toBytes {- 0xB2 -} (IsType u30) = 1 + u30Bytes u30
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
