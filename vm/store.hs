module Vm.Store (
  build_cp
, get_int
, get_uint
, get_double
, get_string
, get_nsInfo
, get_nsSet
, get_multiname
, get_methodSig
, get_metadata
, get_instance
, get_class
, get_script
, get_methodBody
) where

import           Control.Applicative ((<$>))
import           Data.Bits
import           Data.Int
import           Data.Word
import           MonadLib hiding (get, set)
import           Util.Misc (t44)
import           Vm.Def
import qualified Abc.Def as Abc
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashTable.IO as H

key_int :: B.ByteString
key_int = BC.pack "_int"

key_uint :: B.ByteString
key_uint = BC.pack "_uint"

key_double :: B.ByteString
key_double = BC.pack "_double"

key_string :: B.ByteString
key_string = BC.pack "_string"

key_nsInfo :: B.ByteString
key_nsInfo = BC.pack "_nsInfo"

key_nsSet :: B.ByteString
key_nsSet = BC.pack "_nsSet"

key_multiname :: B.ByteString
key_multiname = BC.pack "_multiname"

key_methodSig :: B.ByteString
key_methodSig = BC.pack "_methodSig"

key_metadata :: B.ByteString
key_metadata = BC.pack "_metadata"

key_instance :: B.ByteString
key_instance = BC.pack "_instance"

key_class :: B.ByteString
key_class = BC.pack "_class"

key_script :: B.ByteString
key_script = BC.pack "_script"

key_methodBody :: B.ByteString
key_methodBody = BC.pack "_methodBody"

build_cp :: Abc.Abc -> IO ConstantPool
build_cp (Abc.Abc ints uints doubles strings nsInfo nsSet multinames methodSigs metadata instances classes scripts methodBodies) = do
  cp <- H.new
  mapM_ (\(idx, a) -> put_int cp idx a) $ zip (map fromIntegral [0..length ints]) ints
  mapM_ (\(idx, a) -> put_uint cp idx a) $ zip (map fromIntegral [0..length uints]) uints
  mapM_ (\(idx, a) -> put_double cp idx a) $ zip (map fromIntegral [0..length doubles]) doubles
  mapM_ (\(idx, a) -> put_string cp idx a) $ zip (map fromIntegral [0..length strings]) byteStrings
  mapM_ (\(idx, a) -> put_nsInfo cp idx a) $ zip (map fromIntegral [0..length nsInfo]) nsInfo
  mapM_ (\(idx, a) -> put_nsSet cp idx a) $ zip (map fromIntegral [0..length nsSet]) nsSet
  mapM_ (\(idx, a) -> put_multiname cp idx a) $ zip (map fromIntegral [0..length multinames]) multinames
  mapM_ (\(idx, a) -> put_methodSig cp idx a) $ zip (map fromIntegral [0..length methodSigs]) methodSigs
  mapM_ (\(idx, a) -> put_metadata cp idx a) $ zip (map fromIntegral [0..length metadata]) metadata
  mapM_ (\(idx, a) -> put_instance cp idx a) $ zip (map fromIntegral [0..length instances]) instances
  mapM_ (\(idx, a) -> put_class cp idx a) $ zip (map fromIntegral [0..length classes]) classes
  mapM_ (\(idx, a) -> put_script cp idx a) $ zip (map fromIntegral [0..length scripts]) scripts
  -- reverse lookup: get_methodBody expects a method signature index since a
  -- method body index doesn't exist
  mapM_ (\(idx, a) -> put_methodBody cp idx a) $ zip (map Abc.mbMethod methodBodies) methodBodiesNew
  return cp
  where
    byteStrings = xform_strings strings
    methodBodiesNew :: [MethodBody]
    methodBodiesNew = xform_methodBodies
      int_res
      uint_res
      double_res
      string_res
      (\idx -> maybeMultiname_res string_res nsinfo_res$ multinames !! fromIntegral idx)
      methodBodies

    int_res :: Abc.U30 -> Abc.S32
    int_res i = ints !! fromIntegral i

    uint_res :: Abc.U30 -> Abc.U30
    uint_res i = uints !! fromIntegral i

    double_res :: Abc.U30 -> Double
    double_res i = doubles !! fromIntegral i

    multiname_res :: Abc.U30 -> B.ByteString
    multiname_res i = multiname string_res nsinfo_res $ multinames !! fromIntegral i

    string_res :: Abc.U30 -> B.ByteString
    string_res i = BC.pack$ strings !! fromIntegral i

    nsinfo_res :: Abc.U30 -> B.ByteString
    nsinfo_res i = nsinfo_raw string_res$ nsInfo !! fromIntegral i

    {-vmMethodSigs :: [MethodSignature]
    vmMethodSigs = map toVmMethodSig methodSigs

    toVmMethodSig :: Abc.MethodSignature -> MethodSignature
    toVmMethodSig (MethodSignature ret ptypes name flags options pnames) = undefined-}

    {-vmMethodBodies :: [MethodBody]
    vmMethodBodies = undefined-}

maybeMultiname_res :: (Abc.U30 -> B.ByteString) -- string resolution
                   -> (Abc.U30 -> B.ByteString) -- nsinfo resolution
                   -> Abc.Multiname
                   -> Maybe B.ByteString
maybeMultiname_res string_res nsinfo_res (Abc.Multiname_QName a b)
  | B.null nsinfo = Just string
  | otherwise = Just$ B.append nsinfo$ B.append colons string
  where
    nsinfo = nsinfo_res a
    string = string_res b
    colons = BC.pack "::"
maybeMultiname_res string_res nsinfo_res (Abc.Multiname_QNameA a b)
  | B.null nsinfo = Just string
  | otherwise = Just$ B.append nsinfo$ B.append colons string
  where
    nsinfo = nsinfo_res a
    string = string_res b
    colons = BC.pack "::"
maybeMultiname_res _ _ (Abc.Multiname_RTQName a) = Nothing
maybeMultiname_res _ _ (Abc.Multiname_RTQNameA a) = Nothing
maybeMultiname_res _ _ (Abc.Multiname_Multiname a b) = Nothing
maybeMultiname_res _ _ (Abc.Multiname_MultinameA a b) = Nothing
maybeMultiname_res _ _ (Abc.Multiname_MultinameL a) = Nothing
maybeMultiname_res _ _ (Abc.Multiname_MultinameLA a) = Nothing
maybeMultiname_res _ _ Abc.Multiname_Any = Nothing

multiname :: (Abc.U30 -> B.ByteString) -- string resolution
          -> (Abc.U30 -> B.ByteString) -- nsinfo resolution
          -> Abc.Multiname
          -> B.ByteString
multiname string_res nsinfo_res (Abc.Multiname_QName a b)
  | B.null nsinfo = string
  | otherwise = B.append nsinfo$ B.append colons string
  where
    nsinfo = nsinfo_res a
    string = string_res b
    colons = BC.pack "::"
multiname string_res nsinfo_res (Abc.Multiname_QNameA a b)
  | B.null nsinfo = string
  | otherwise = B.append nsinfo$ B.append colons string
  where
    nsinfo = nsinfo_res a
    string = string_res b
    colons = BC.pack "::"
multiname string_res nsinfo_res m@(Abc.Multiname_RTQName a) = BC.pack$ show m
multiname string_res nsinfo_res m@(Abc.Multiname_RTQNameA a) = BC.pack$ show m
multiname string_res nsinfo_res m@(Abc.Multiname_Multiname a b) = BC.pack$ show m
multiname string_res nsinfo_res m@(Abc.Multiname_MultinameA a b) = BC.pack$ show m
multiname string_res nsinfo_res m@(Abc.Multiname_MultinameL a) = BC.pack$ show m
multiname string_res nsinfo_res m@(Abc.Multiname_MultinameLA a) = BC.pack$ show m
multiname string_res nsinfo_res Abc.Multiname_Any = BC.pack "*"

nsinfo_raw :: (Abc.U30 -> B.ByteString) -- string resolution
           -> Abc.NSInfo
           -> B.ByteString
nsinfo_raw string_res (Abc.NSInfo_Namespace a)          = string_res a
nsinfo_raw string_res (Abc.NSInfo_PackageNamespace a)   = string_res a
nsinfo_raw string_res (Abc.NSInfo_PackageInternalNs a)  = string_res a
nsinfo_raw string_res (Abc.NSInfo_ProtectedNamespace a) = string_res a
nsinfo_raw string_res (Abc.NSInfo_ExplicitNamespace a)  = string_res a
nsinfo_raw string_res (Abc.NSInfo_StaticProtectedNs a)  = string_res a
nsinfo_raw string_res (Abc.NSInfo_PrivateNs a)          = string_res a
nsinfo_raw string_res Abc.NSInfo_Any = BC.pack "*"

xform_strings :: [String] -> [B.ByteString]
xform_strings = map BC.pack

xform_methodBodies :: (Abc.IntIdx -> Abc.S32)                  -- int resolution
                   -> (Abc.UintIdx -> Abc.U32)                 -- uint resolution
                   -> (Abc.DoubleIdx -> Double)                -- double resolution
                   -> (Abc.StringIdx -> B.ByteString)          -- string resolution
                   -> (Abc.MultinameIdx -> Maybe B.ByteString) -- multiname resolution
                   -> [Abc.MethodBody]
                   -> [MethodBody]
xform_methodBodies i u d s m = map f where
  f (Abc.MethodBody ma mb mc md me code mf mg) = MethodBody ma mb mc md me newCode mf mg
    where
      newCode = concatMap (xform_opCode i u d s m) code

xform_opCode :: (Abc.IntIdx -> Abc.S32)                  -- int resolution
             -> (Abc.UintIdx -> Abc.U32)                 -- uint resolution
             -> (Abc.DoubleIdx -> Double)                -- double resolution
             -> (Abc.StringIdx -> B.ByteString)          -- string resolution
             -> (Abc.MultinameIdx -> Maybe B.ByteString) -- multiname resolution
             -> Abc.OpCode
             -> [OpCode]
xform_opCode {- 0x01 -} i u d s m (Abc.Breakpoint) = [Breakpoint]
xform_opCode {- 0x02 -} i u d s m (Abc.Nop) = [Nop]
xform_opCode {- 0x03 -} i u d s m (Abc.Throw) = [Throw]
xform_opCode {- 0x04 -} i u d s m (Abc.GetSuper u30) = [GetSuper u30 $ m u30]
xform_opCode {- 0x05 -} i u d s m (Abc.SetSuper u30) = [SetSuper u30 $ m u30]
xform_opCode {- 0x06 -} i u d s m (Abc.DefaultXmlNamespace u30) = [DefaultXmlNamespace u30]
xform_opCode {- 0x07 -} i u d s m (Abc.DefaultXmlNamespaceL) = [DefaultXmlNamespaceL]
xform_opCode {- 0x08 -} i u d s m (Abc.Kill u30) = [Kill u30]
xform_opCode {- 0x09 -} i u d s m (Abc.Label) = [Label]
xform_opCode {- 0x0C -} i u d s m (Abc.IfNotLessThan s24) = [IfNotLessThan s24]
xform_opCode {- 0x0D -} i u d s m (Abc.IfNotLessEqual s24) = [IfNotLessEqual s24]
xform_opCode {- 0x0E -} i u d s m (Abc.IfNotGreaterThan s24) = [IfNotGreaterThan s24]
xform_opCode {- 0x0F -} i u d s m (Abc.IfNotGreaterEqual s24) = [IfNotGreaterEqual s24]
xform_opCode {- 0x10 -} i u d s m (Abc.Jump s24) = [Jump s24]
xform_opCode {- 0x11 -} i u d s m (Abc.IfTrue s24) = [IfTrue s24]
xform_opCode {- 0x12 -} i u d s m (Abc.IfFalse s24) = [IfFalse s24]
xform_opCode {- 0x13 -} i u d s m (Abc.IfEqual s24) = [IfEqual s24]
xform_opCode {- 0x14 -} i u d s m (Abc.IfNotEqual s24) = [IfNotEqual s24]
xform_opCode {- 0x15 -} i u d s m (Abc.IfLessThan s24) = [IfLessThan s24]
xform_opCode {- 0x16 -} i u d s m (Abc.IfLessEqual s24) = [IfLessEqual s24]
xform_opCode {- 0x17 -} i u d s m (Abc.IfGreaterThan s24) = [IfGreaterThan s24]
xform_opCode {- 0x18 -} i u d s m (Abc.IfGreaterEqual s24) = [IfGreaterEqual s24]
xform_opCode {- 0x19 -} i u d s m (Abc.IfStrictEqual s24) = [IfStrictEqual s24]
xform_opCode {- 0x1A -} i u d s m (Abc.IfStrictNotEqual s24) = [IfStrictNotEqual s24]
xform_opCode {- 0x1B -} i u d s m (Abc.LookupSwitch s24 s24s) = [LookupSwitch s24 s24s]
xform_opCode {- 0x1C -} i u d s m (Abc.PushWith) = [PushWith]
xform_opCode {- 0x1D -} i u d s m (Abc.PopScope) = [PopScope]
xform_opCode {- 0x1E -} i u d s m (Abc.NextName) = [NextName]
xform_opCode {- 0x1F -} i u d s m (Abc.HasNext) = [HasNext]
xform_opCode {- 0x20 -} i u d s m (Abc.PushNull) = [PushNull]
xform_opCode {- 0x21 -} i u d s m (Abc.PushUndefined) = [PushUndefined]
xform_opCode {- 0x22 -} i u d s m (Abc.PushConstant) = [PushConstant]
xform_opCode {- 0x23 -} i u d s m (Abc.NextValue) = [NextValue]
xform_opCode {- 0x24 -} i u d s m (Abc.PushByte u8) = [PushByte u8]
xform_opCode {- 0x25 -} i u d s m (Abc.PushShort u30) = [PushShort u30]
xform_opCode {- 0x26 -} i u d s m (Abc.PushTrue) = [PushTrue]
xform_opCode {- 0x27 -} i u d s m (Abc.PushFalse) = [PushFalse]
xform_opCode {- 0x28 -} i u d s m (Abc.PushNaN) = [PushNaN]
xform_opCode {- 0x29 -} i u d s m (Abc.Pop) = [Pop]
xform_opCode {- 0x2A -} i u d s m (Abc.Dup) = [Dup]
xform_opCode {- 0x2B -} i u d s m (Abc.Swap) = [Swap]
xform_opCode {- 0x2C -} i u d s m (Abc.PushString u30) = [PushString u30 $ s u30]
xform_opCode {- 0x2D -} i u d s m (Abc.PushInt u30) = [PushInt u30 $ i u30]
xform_opCode {- 0x2E -} i u d s m (Abc.PushUInt u30) = [PushUInt u30 $ u u30]
xform_opCode {- 0x2F -} i u d s m (Abc.PushDouble u30) = [PushDouble u30 $ d u30]
xform_opCode {- 0x30 -} i u d s m (Abc.PushScope) = [PushScope]
xform_opCode {- 0x31 -} i u d s m (Abc.PushNamespace u30) = [PushNamespace u30]
xform_opCode {- 0x32 -} i u d s m (Abc.HasNext2 w32 w32_2) = [HasNext2 w32 w32_2]
xform_opCode {- 0x33 -} i u d s m (Abc.PushDecimal) = [PushDecimal]
xform_opCode {- 0x34 -} i u d s m (Abc.PushDNaN) = [PushDNaN]
xform_opCode {- 0x40 -} i u d s m (Abc.NewFunction u30) = [NewFunction u30]
xform_opCode {- 0x41 -} i u d s m (Abc.Call u30) = [Call u30]
xform_opCode {- 0x42 -} i u d s m (Abc.Construct u30) = [Construct u30]
xform_opCode {- 0x43 -} i u d s m (Abc.CallMethod u30_1 u30_2) = [CallMethod u30_1 u30_2 $ m u30_1]
xform_opCode {- 0x44 -} i u d s m (Abc.CallStatic u30_1 u30_2) = [CallStatic u30_1 u30_2 $ m u30_1]
xform_opCode {- 0x45 -} i u d s m (Abc.CallSuper u30_1 u30_2) = [CallSuper u30_1 u30_2 $ m u30_1]
xform_opCode {- 0x46 -} i u d s m (Abc.CallProperty u30_1 u30_2) = [CallProperty u30_1 u30_2 $ m u30_1]
xform_opCode {- 0x47 -} i u d s m (Abc.ReturnVoid) = [ReturnVoid]
xform_opCode {- 0x48 -} i u d s m (Abc.ReturnValue) = [ReturnValue]
xform_opCode {- 0x49 -} i u d s m (Abc.ConstructSuper u30) = [ConstructSuper u30]
xform_opCode {- 0x4A -} i u d s m (Abc.ConstructProp u30_1 u30_2) = [ConstructProp u30_1 u30_2 $ m u30_1]
xform_opCode {- 0x4B -} i u d s m (Abc.CallSuperId) = [CallSuperId]
xform_opCode {- 0x4C -} i u d s m (Abc.CallPropLex u30_1 u30_2) = [CallPropLex u30_1 u30_2 $ m u30_1]
xform_opCode {- 0x4D -} i u d s m (Abc.CallInterface) = [CallInterface]
xform_opCode {- 0x4E -} i u d s m (Abc.CallSuperVoid u30_1 u30_2) = [CallSuperVoid u30_1 u30_2 $ m u30_1]
xform_opCode {- 0x4F -} i u d s m (Abc.CallPropVoid u30_1 u30_2) = [CallPropVoid u30_1 u30_2 $ m u30_1]
xform_opCode {- 0x53 -} i u d s m (Abc.ApplyType) = [ApplyType]
xform_opCode {- 0x55 -} i u d s m (Abc.NewObject u30) = [NewObject u30]
xform_opCode {- 0x56 -} i u d s m (Abc.NewArray u30) = [NewArray u30]
xform_opCode {- 0x57 -} i u d s m (Abc.NewActivation) = [NewActivation]
xform_opCode {- 0x58 -} i u d s m (Abc.NewClass u30) = [NewClass u30]
xform_opCode {- 0x59 -} i u d s m (Abc.GetDescendants u30) = [GetDescendants u30 $ m u30]
xform_opCode {- 0x5A -} i u d s m (Abc.NewCatch u30) = [NewCatch u30]
xform_opCode {- 0x5B -} i u d s m (Abc.FindPropGlobalStrict) = [FindPropGlobalStrict]
xform_opCode {- 0x5C -} i u d s m (Abc.FindPropGlobal) = [FindPropGlobal]
xform_opCode {- 0x5D -} i u d s m (Abc.FindPropStrict u30) = [FindPropStrict u30 $ m u30]
xform_opCode {- 0x5E -} i u d s m (Abc.FindProperty u30) = [FindProperty u30 $ m u30]
xform_opCode {- 0x5F -} i u d s m (Abc.FindDef) = [FindDef]
xform_opCode {- 0x60 -} i u d s m (Abc.GetLex idx) = [FindPropStrict idx$ m idx, GetProperty idx$ m idx]
xform_opCode {- 0x61 -} i u d s m (Abc.SetProperty u30) = [SetProperty u30 $ m u30]
xform_opCode {- 0x62 -} i u d s m (Abc.GetLocal u30) = [GetLocal u30]
xform_opCode {- 0x63 -} i u d s m (Abc.SetLocal u30) = [SetLocal u30]
xform_opCode {- 0x64 -} i u d s m (Abc.GetGlobalScope) = [GetGlobalScope]
xform_opCode {- 0x65 -} i u d s m (Abc.GetScopeObject u8) = [GetScopeObject u8]
xform_opCode {- 0x66 -} i u d s m (Abc.GetProperty u30) = [GetProperty u30 $ m u30]
xform_opCode {- 0x67 -} i u d s m (Abc.GetPropertyLate) = [GetPropertyLate]
xform_opCode {- 0x68 -} i u d s m (Abc.InitProperty u30) = [InitProperty u30 $ m u30]
xform_opCode {- 0x69 -} i u d s m (Abc.SetPropertyLate) = [SetPropertyLate]
xform_opCode {- 0x6A -} i u d s m (Abc.DeleteProperty u30) = [DeleteProperty u30 $ m u30]
xform_opCode {- 0x6B -} i u d s m (Abc.DeletePropertyLate) = [DeletePropertyLate]
xform_opCode {- 0x6C -} i u d s m (Abc.GetSlot u30) = [GetSlot u30]
xform_opCode {- 0x6D -} i u d s m (Abc.SetSlot u30) = [SetSlot u30]
xform_opCode {- 0x6E -} i u d s m (Abc.GetGlobalSlot u30) = [GetGlobalSlot u30]
xform_opCode {- 0x6F -} i u d s m (Abc.SetGlobalSlot u30) = [SetGlobalSlot u30]
xform_opCode {- 0x70 -} i u d s m (Abc.ConvertString) = [ConvertString]
xform_opCode {- 0x71 -} i u d s m (Abc.EscXmlElem) = [EscXmlElem]
xform_opCode {- 0x72 -} i u d s m (Abc.EscXmlAttr) = [EscXmlAttr]
xform_opCode {- 0x73 -} i u d s m (Abc.ConvertInt) = [ConvertInt]
xform_opCode {- 0x74 -} i u d s m (Abc.ConvertUInt) = [ConvertUInt]
xform_opCode {- 0x75 -} i u d s m (Abc.ConvertDouble) = [ConvertDouble]
xform_opCode {- 0x76 -} i u d s m (Abc.ConvertBoolean) = [ConvertBoolean]
xform_opCode {- 0x77 -} i u d s m (Abc.ConvertObject) = [ConvertObject]
xform_opCode {- 0x78 -} i u d s m (Abc.CheckFilter) = [CheckFilter]
xform_opCode {- 0x80 -} i u d s m (Abc.Coerce u30) = [Coerce u30 $ m u30]
xform_opCode {- 0x81 -} i u d s m (Abc.CoerceBoolean) = [CoerceBoolean]
xform_opCode {- 0x82 -} i u d s m (Abc.CoerceAny) = [CoerceAny]
xform_opCode {- 0x83 -} i u d s m (Abc.CoerceInt) = [CoerceInt]
xform_opCode {- 0x84 -} i u d s m (Abc.CoerceDouble) = [CoerceDouble]
xform_opCode {- 0x85 -} i u d s m (Abc.CoerceString) = [CoerceString]
xform_opCode {- 0x86 -} i u d s m (Abc.AsType u30) = [AsType u30 $ m u30]
xform_opCode {- 0x87 -} i u d s m (Abc.AsTypeLate) = [AsTypeLate]
xform_opCode {- 0x88 -} i u d s m (Abc.CoerceUInt) = [CoerceUInt]
xform_opCode {- 0x89 -} i u d s m (Abc.CoerceObject) = [CoerceObject]
xform_opCode {- 0x90 -} i u d s m (Abc.Negate) = [Negate]
xform_opCode {- 0x91 -} i u d s m (Abc.Increment) = [Increment]
xform_opCode {- 0x92 -} i u d s m (Abc.IncLocal) = [IncLocal]
xform_opCode {- 0x93 -} i u d s m (Abc.Decrement) = [Decrement]
xform_opCode {- 0x94 -} i u d s m (Abc.DecLocal u30) = [DecLocal u30]
xform_opCode {- 0x95 -} i u d s m (Abc.TypeOf) = [TypeOf]
xform_opCode {- 0x96 -} i u d s m (Abc.Not) = [Not]
xform_opCode {- 0x97 -} i u d s m (Abc.BitNot) = [BitNot]
xform_opCode {- 0x9A -} i u d s m (Abc.Concat) = [Concat]
xform_opCode {- 0x9B -} i u d s m (Abc.AddDouble) = [AddDouble]
xform_opCode {- 0xA0 -} i u d s m (Abc.Add) = [Add]
xform_opCode {- 0xA1 -} i u d s m (Abc.Subtract) = [Subtract]
xform_opCode {- 0xA2 -} i u d s m (Abc.Multiply) = [Multiply]
xform_opCode {- 0xA3 -} i u d s m (Abc.Divide) = [Divide]
xform_opCode {- 0xA4 -} i u d s m (Abc.Modulo) = [Modulo]
xform_opCode {- 0xA5 -} i u d s m (Abc.ShiftLeft) = [ShiftLeft]
xform_opCode {- 0xA6 -} i u d s m (Abc.ShiftRight) = [ShiftRight]
xform_opCode {- 0xA7 -} i u d s m (Abc.ShiftRightUnsigned) = [ShiftRightUnsigned]
xform_opCode {- 0xA8 -} i u d s m (Abc.BitAnd) = [BitAnd]
xform_opCode {- 0xA9 -} i u d s m (Abc.BitOr) = [BitOr]
xform_opCode {- 0xAA -} i u d s m (Abc.BitXor) = [BitXor]
xform_opCode {- 0xAB -} i u d s m (Abc.Equals) = [Equals]
xform_opCode {- 0xAC -} i u d s m (Abc.StrictEquals) = [StrictEquals]
xform_opCode {- 0xAD -} i u d s m (Abc.LessThan) = [LessThan]
xform_opCode {- 0xAE -} i u d s m (Abc.LessEquals) = [LessEquals]
xform_opCode {- 0xAF -} i u d s m (Abc.GreaterThan) = [GreaterThan]
xform_opCode {- 0xB0 -} i u d s m (Abc.GreaterEquals) = [GreaterEquals]
xform_opCode {- 0xB1 -} i u d s m (Abc.InstanceOf) = [InstanceOf]
xform_opCode {- 0xB2 -} i u d s m (Abc.IsType u30) = [IsType u30 $ m u30]
xform_opCode {- 0xB3 -} i u d s m (Abc.IsTypeLate) = [IsTypeLate]
xform_opCode {- 0xB4 -} i u d s m (Abc.In) = [In]
xform_opCode {- 0xC0 -} i u d s m (Abc.IncrementInt) = [IncrementInt]
xform_opCode {- 0xC1 -} i u d s m (Abc.DecrementInt) = [DecrementInt]
xform_opCode {- 0xC2 -} i u d s m (Abc.IncLocalInt u30) = [IncLocalInt u30]
xform_opCode {- 0xC3 -} i u d s m (Abc.DecLocalInt u30) = [DecLocalInt u30]
xform_opCode {- 0xC4 -} i u d s m (Abc.NegateInt) = [NegateInt]
xform_opCode {- 0xC5 -} i u d s m (Abc.AddInt) = [AddInt]
xform_opCode {- 0xC6 -} i u d s m (Abc.SubtractInt) = [SubtractInt]
xform_opCode {- 0xC7 -} i u d s m (Abc.MultiplyInt) = [MultiplyInt]
xform_opCode {- 0xD0 -} i u d s m (Abc.GetLocal0) = [GetLocal0]
xform_opCode {- 0xD1 -} i u d s m (Abc.GetLocal1) = [GetLocal1]
xform_opCode {- 0xD2 -} i u d s m (Abc.GetLocal2) = [GetLocal2]
xform_opCode {- 0xD3 -} i u d s m (Abc.GetLocal3) = [GetLocal3]
xform_opCode {- 0xD4 -} i u d s m (Abc.SetLocal0) = [SetLocal0]
xform_opCode {- 0xD5 -} i u d s m (Abc.SetLocal1) = [SetLocal1]
xform_opCode {- 0xD6 -} i u d s m (Abc.SetLocal2) = [SetLocal2]
xform_opCode {- 0xD7 -} i u d s m (Abc.SetLocal3) = [SetLocal3]
xform_opCode {- 0xEF -} i u d s m (Abc.Debug u8_1 u30_1 u8_2 u30_2) = [Debug u8_1 u30_1 u8_2 u30_2]
xform_opCode {- 0xF0 -} i u d s m (Abc.DebugLine u30) = [DebugLine u30]
xform_opCode {- 0xF1 -} i u d s m (Abc.DebugFile u30) = [DebugFile u30]
xform_opCode {- 0xF2 -} i u d s m (Abc.BreakpointLine) = [BreakpointLine]

get_int :: ConstantPool -> Abc.U30 -> IO Abc.S32
get_int cp u30 = do VmAbc_Int a <- get_ht cp key_int u30;return a

put_int :: ConstantPool -> Abc.U30 -> Abc.S32 -> IO ()
put_int cp k v = put_ht cp key_int k $ VmAbc_Int v

get_uint :: ConstantPool -> Abc.U30 -> IO Abc.U30
get_uint cp u30 = do VmAbc_Uint a <- get_ht cp key_uint u30;return a

put_uint :: ConstantPool -> Abc.U30 -> Abc.U30 -> IO ()
put_uint cp k v = put_ht cp key_uint k $ VmAbc_Uint v

get_double :: ConstantPool -> Abc.U30 -> IO Double
get_double cp u30 = do VmAbc_Double a <- get_ht cp key_double u30;return a

put_double :: ConstantPool -> Abc.U30 -> Double -> IO ()
put_double cp k v = put_ht cp key_double k $ VmAbc_Double v

get_string :: ConstantPool -> Abc.U30 -> IO B.ByteString
get_string cp u30 = do VmAbc_String a <- get_ht cp key_string u30;return a

put_string :: ConstantPool -> Abc.U30 -> B.ByteString -> IO ()
put_string cp k v = put_ht cp key_string k $ VmAbc_String v

get_nsInfo :: ConstantPool -> Abc.U30 -> IO Abc.NSInfo
get_nsInfo cp u30 = do VmAbc_NsInfo a <- get_ht cp key_nsInfo u30;return a

put_nsInfo :: ConstantPool -> Abc.U30 -> Abc.NSInfo -> IO ()
put_nsInfo cp k v = put_ht cp key_nsInfo k $ VmAbc_NsInfo v

get_nsSet :: ConstantPool -> Abc.U30 -> IO Abc.NSSet
get_nsSet cp u30 = do VmAbc_NsSet a <- get_ht cp key_nsSet u30;return a

put_nsSet :: ConstantPool -> Abc.U30 -> Abc.NSSet -> IO ()
put_nsSet cp k v = put_ht cp key_nsSet k $ VmAbc_NsSet v

get_multiname :: ConstantPool -> Abc.U30 -> IO Abc.Multiname
get_multiname cp u30 = do VmAbc_Multiname a <- get_ht cp key_multiname u30;return a

put_multiname :: ConstantPool -> Abc.U30 -> Abc.Multiname -> IO ()
put_multiname cp k v = put_ht cp key_multiname k $ VmAbc_Multiname v

get_methodSig :: ConstantPool -> Abc.U30 -> IO Abc.MethodSignature
get_methodSig cp u30 = do VmAbc_MethodSig a <- get_ht cp key_methodSig u30;return a

put_methodSig :: ConstantPool -> Abc.U30 -> Abc.MethodSignature -> IO ()
put_methodSig cp k v = put_ht cp key_methodSig k $ VmAbc_MethodSig v

get_metadata :: ConstantPool -> Abc.U30 -> IO Abc.Metadata
get_metadata cp u30 = do VmAbc_Metadata a <- get_ht cp key_metadata u30;return a

put_metadata :: ConstantPool -> Abc.U30 -> Abc.Metadata -> IO ()
put_metadata cp k v = put_ht cp key_metadata k $ VmAbc_Metadata v

get_instance :: ConstantPool -> Abc.U30 -> IO Abc.InstanceInfo
get_instance cp u30 = do VmAbc_Instance a <- get_ht cp key_instance u30;return a

put_instance :: ConstantPool -> Abc.U30 -> Abc.InstanceInfo -> IO ()
put_instance cp k v = put_ht cp key_instance k $ VmAbc_Instance v

get_class :: ConstantPool -> Abc.U30 -> IO Abc.ClassInfo
get_class cp u30 = do VmAbc_Class a <- get_ht cp key_class u30;return a

put_class :: ConstantPool -> Abc.U30 -> Abc.ClassInfo -> IO ()
put_class cp k v = put_ht cp key_class k $ VmAbc_Class v

get_script :: ConstantPool -> Abc.U30 -> IO Abc.ScriptInfo
get_script cp u30 = do VmAbc_Script a <- get_ht cp key_script u30;return a

put_script :: ConstantPool -> Abc.U30 -> Abc.ScriptInfo -> IO ()
put_script cp k v = put_ht cp key_script k $ VmAbc_Script v

get_methodBody :: ConstantPool -> Abc.U30 -> IO MethodBody
get_methodBody cp u30 = do VmAbc_MethodBody a <- get_ht cp key_methodBody u30;return a

put_methodBody :: ConstantPool -> Abc.U30 -> MethodBody -> IO ()
put_methodBody cp k v = put_ht cp key_methodBody k $ VmAbc_MethodBody v

get_ht :: ConstantPool -> B.ByteString -> Abc.U30 -> IO VmAbc
get_ht ht prefix k = do
  --liftIO.putStrLn$ "prefix " ++ show prefix ++ show k
  m <- H.lookup ht fullKey
  case m of
    Nothing -> fail$ "get_ht - " ++ (show k ++ BC.unpack prefix)
    Just ret -> return ret
  where
    fullKey = foldr B.cons prefix$ u30ToWord8 k

put_ht :: ConstantPool -> B.ByteString -> Abc.U30 -> VmAbc -> IO ()
put_ht cp prefix k v = do
  --liftIO.putStrLn$ "prefix " ++ show prefix ++ show k
  H.insert cp fullKey v
  where
    fullKey = foldr B.cons prefix$ u30ToWord8 k

u30ToWord8 :: Abc.U30 -> [Word8]
u30ToWord8 u30 = [msb0, msb1, msb2, msb3]
  where
    msb0 = fromIntegral$ u30 `shiftR` 24 .&. 0xff
    msb1 = fromIntegral$ u30 `shiftR` 16 .&. 0xff
    msb2 = fromIntegral$ u30 `shiftR`  8 .&. 0xff
    msb3 = fromIntegral$ u30 `shiftR`  0 .&. 0xff
{-# INLINE u30ToWord8 #-}
