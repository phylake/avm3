module Abc.DeepSeq where

import Control.DeepSeq
import Abc.Def

instance NFData Abc where
  rnf (Abc a b c d e f g h i j k l m) = a
    `deepseq` b
    `deepseq` c
    `deepseq` d
    `deepseq` e
    `deepseq` f
    `deepseq` g
    `deepseq` h
    `deepseq` i
    `deepseq` j
    `deepseq` k
    `deepseq` l
    `deepseq` m
    `deepseq` ()

instance NFData NSInfo where
  rnf (NSInfo_Namespace a) = a `deepseq` ()
  rnf (NSInfo_PackageNamespace a) = a `deepseq` ()
  rnf (NSInfo_PackageInternalNs a) = a `deepseq` ()
  rnf (NSInfo_ProtectedNamespace a) = a `deepseq` ()
  rnf (NSInfo_ExplicitNamespace a) = a `deepseq` ()
  rnf (NSInfo_StaticProtectedNs a) = a `deepseq` ()
  rnf (NSInfo_PrivateNs a) = a `deepseq` ()
  rnf _ = ()

instance NFData Multiname where
  rnf (Multiname_QName a b) = a `deepseq` b `deepseq` ()
  rnf (Multiname_QNameA a b) = a `deepseq` b `deepseq` ()
  rnf (Multiname_RTQName a) = a `deepseq` ()
  rnf (Multiname_RTQNameA a) = a `deepseq` ()
  rnf (Multiname_Multiname a b) = a `deepseq` b `deepseq` ()
  rnf (Multiname_MultinameA a b) = a `deepseq` b `deepseq` ()
  rnf (Multiname_MultinameL a) = a `deepseq` ()
  rnf (Multiname_MultinameLA a) = a `deepseq` ()
  rnf Multiname_Any = ()

instance NFData MethodSignature where
  rnf (MethodSignature a b c d e f) = a
    `deepseq` b
    `deepseq` c
    `deepseq` d
    `deepseq` e
    `deepseq` f
    `deepseq` ()

instance NFData CPC where
  --rnf (CPC_Undefined) = a `deepseq` ()
  rnf (CPC_Utf8 a) = a `deepseq` ()
  rnf (CPC_Decimal a) = a `deepseq` ()
  rnf (CPC_Int a) = a `deepseq` ()
  rnf (CPC_Uint a) = a `deepseq` ()
  rnf (CPC_PrivateNamespace a) = a `deepseq` ()
  rnf (CPC_Double a) = a `deepseq` ()
  rnf (CPC_QName a) = a `deepseq` ()
  rnf (CPC_Namespace a) = a `deepseq` ()
  rnf (CPC_Multiname a) = a `deepseq` ()
  --rnf (CPC_False) = a `deepseq` ()
  --rnf (CPC_True) = a `deepseq` ()
  --rnf (CPC_Null) = a `deepseq` ()
  rnf (CPC_QNameA a) = a `deepseq` ()
  rnf (CPC_MultinameA a) = a `deepseq` ()
  rnf (CPC_RTQName a) = a `deepseq` ()
  rnf (CPC_RTQNameA a) = a `deepseq` ()
  rnf (CPC_RTQNameL a) = a `deepseq` ()
  rnf (CPC_RTQNameLA a) = a `deepseq` ()
  rnf (CPC_NameL a) = a `deepseq` ()
  rnf (CPC_NameLA a) = a `deepseq` ()
  rnf (CPC_NamespaceSet a) = a `deepseq` ()
  rnf (CPC_PackageNamespace a) = a `deepseq` ()
  rnf (CPC_PackageInternalNs a) = a `deepseq` ()
  rnf (CPC_ProtectedNamespace a) = a `deepseq` ()
  rnf (CPC_ExplicitNamespace a) = a `deepseq` ()
  rnf (CPC_StaticProtectedNs a) = a `deepseq` ()
  rnf (CPC_MultinameL a) = a `deepseq` ()
  rnf (CPC_MultinameLA a) = a `deepseq` ()
  rnf _ = ()

instance NFData Metadata where
  rnf (Metadata a b) = a `deepseq` b `deepseq` ()

instance NFData InstanceInfo where
  rnf (InstanceInfo a b c d e f g) = a
    `deepseq` b
    `deepseq` c
    `deepseq` d
    `deepseq` e
    `deepseq` f
    `deepseq` g
    `deepseq` ()

instance NFData TraitsInfo where
  rnf (TraitsInfo a b c d e) = a
    `deepseq` b
    `deepseq` c
    `deepseq` d
    `deepseq` e
    `deepseq` ()

instance NFData TraitType where
  rnf (TraitVar a b c d) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` ()
  rnf (TraitConst a b c d) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` ()
  rnf (TraitMethod a b) = a `deepseq` b `deepseq` ()
  rnf (TraitGetter a b) = a `deepseq` b `deepseq` ()
  rnf (TraitSetter a b) = a `deepseq` b `deepseq` ()
  rnf (TraitClass a b) = a `deepseq` b `deepseq` ()
  rnf (TraitFunction a b) = a `deepseq` b `deepseq` ()

instance NFData ClassInfo where
  rnf (ClassInfo a b) = a `deepseq` b `deepseq` ()

instance NFData ScriptInfo where
  rnf (ScriptInfo a b) = a `deepseq` b `deepseq` ()

instance NFData MethodBody where
  rnf (MethodBody a b c d e f g h) = a
    `deepseq` b
    `deepseq` c
    `deepseq` d
    `deepseq` e
    `deepseq` f
    `deepseq` g
    `deepseq` h
    `deepseq` ()

instance NFData Exception where
  rnf (Exception a b c d e) = a
    `deepseq` b
    `deepseq` c
    `deepseq` d
    `deepseq` e
    `deepseq` ()

instance NFData OpCode where
{- 0x01 -} {-Breakpoint-}
{- 0x02 -} {-Nop-}
{- 0x03 -} {-Throw-}
{- 0x04 -} rnf (GetSuper a) = a `deepseq` ()
{- 0x05 -} rnf (SetSuper a) = a `deepseq` ()
{- 0x06 -} rnf (DefaultXmlNamespace a) = a `deepseq` ()
{- 0x07 -} {-DefaultXmlNamespaceL-}
{- 0x08 -} rnf (Kill a) = a `deepseq` ()
{- 0x09 -} {-Label-}
{- 0x0A -}
{- 0x0B -}
{- 0x0C -} rnf (IfNotLessThan a) = a `deepseq` ()
{- 0x0D -} rnf (IfNotLessEqual a) = a `deepseq` ()
{- 0x0E -} rnf (IfNotGreaterThan a) = a `deepseq` ()
{- 0x0F -} rnf (IfNotGreaterEqual a) = a `deepseq` ()
{- 0x10 -} rnf (Jump a) = a `deepseq` ()
{- 0x11 -} rnf (IfTrue a) = a `deepseq` ()
{- 0x12 -} rnf (IfFalse a) = a `deepseq` ()
{- 0x13 -} rnf (IfEqual a) = a `deepseq` ()
{- 0x14 -} rnf (IfNotEqual a) = a `deepseq` ()
{- 0x15 -} rnf (IfLessThan a) = a `deepseq` ()
{- 0x16 -} rnf (IfLessEqual a) = a `deepseq` ()
{- 0x17 -} rnf (IfGreaterThan a) = a `deepseq` ()
{- 0x18 -} rnf (IfGreaterEqual a) = a `deepseq` ()
{- 0x19 -} rnf (IfStrictEqual a) = a `deepseq` ()
{- 0x1A -} rnf (IfStrictNotEqual a) = a `deepseq` ()
{- 0x1B -} rnf (LookupSwitch a b) = a `deepseq` b `deepseq` ()
{- 0x1C -} {-PushWith-}
{- 0x1D -} {-PopScope-}
{- 0x1E -} {-NextName-}
{- 0x1F -} {-HasNext-}
{- 0x20 -} {-PushNull-}
{- 0x21 -} {-PushUndefined-}
{- 0x22 -} {-PushConstant-}
{- 0x23 -} {-NextValue-}
{- 0x24 -} rnf (PushByte a) = a `deepseq` ()
{- 0x25 -} rnf (PushShort a) = a `deepseq` ()
{- 0x26 -} {-PushTrue-}
{- 0x27 -} {-PushFalse-}
{- 0x28 -} {-PushNaN-}
{- 0x29 -} {-Pop-}
{- 0x2A -} {-Dup-}
{- 0x2B -} {-Swap-}
{- 0x2C -} rnf (PushString a) = a `deepseq` ()
{- 0x2D -} rnf (PushInt a) = a `deepseq` ()
{- 0x2E -} rnf (PushUInt a) = a `deepseq` ()
{- 0x2F -} rnf (PushDouble a) = a `deepseq` ()
{- 0x30 -} {-PushScope-}
{- 0x31 -} rnf (PushNamespace a) = a `deepseq` ()
{- 0x32 -} rnf (HasNext2 a b) = a `deepseq` b `deepseq` ()
{- 0x33 -} {-PushDecimal-}
{- 0x34 -} {-PushDNaN-}
{- 0x35 -}
{- 0x36 -}
{- 0x37 -}
{- 0x38 -}
{- 0x39 -}
{- 0x3A -}
{- 0x3B -}
{- 0x3C -}
{- 0x3D -}
{- 0x3E -}
{- 0x3F -}
{- 0x40 -} rnf (NewFunction a) = a `deepseq` ()
{- 0x41 -} rnf (Call a) = a `deepseq` ()
{- 0x42 -} rnf (Construct a) = a `deepseq` ()
{- 0x43 -} rnf (CallMethod a b) = a `deepseq` b `deepseq` ()
{- 0x44 -} rnf (CallStatic a b) = a `deepseq` b `deepseq` ()
{- 0x45 -} rnf (CallSuper a b) = a `deepseq` b `deepseq` ()
{- 0x46 -} rnf (CallProperty a b) = a `deepseq` b `deepseq` ()
{- 0x47 -} {-ReturnVoid-}
{- 0x48 -} {-ReturnValue-}
{- 0x49 -} rnf (ConstructSuper a) = a `deepseq` ()
{- 0x4A -} rnf (ConstructProp a b) = a `deepseq` b `deepseq` ()
{- 0x4B -} {-CallSuperId-}
{- 0x4C -} rnf (CallPropLex a b) = a `deepseq` b `deepseq` ()
{- 0x4D -} {-CallInterface-}
{- 0x4E -} rnf (CallSuperVoid a b) = a `deepseq` b `deepseq` ()
{- 0x4F -} rnf (CallPropVoid a b) = a `deepseq` b `deepseq` ()
{- 0x50 -}
{- 0x51 -}
{- 0x52 -}
{- 0x53 -} {-ApplyType-}
{- 0x55 -} rnf (NewObject a) = a `deepseq` ()
{- 0x56 -} rnf (NewArray a) = a `deepseq` ()
{- 0x57 -} {-NewActivation-}
{- 0x58 -} rnf (NewClass a) = a `deepseq` ()
{- 0x59 -} rnf (GetDescendants a) = a `deepseq` ()
{- 0x5A -} rnf (NewCatch a) = a `deepseq` ()
{- 0x5B -} {-FindPropGlobalStrict-}
{- 0x5C -} {-FindPropGlobal-}
{- 0x5D -} rnf (FindPropStrict a) = a `deepseq` ()
{- 0x5E -} rnf (FindProperty a) = a `deepseq` ()
{- 0x5F -} {-FindDef-}
{- 0x60 -} rnf (GetLex a) = a `deepseq` ()
{- 0x61 -} rnf (SetProperty a) = a `deepseq` ()
{- 0x62 -} rnf (GetLocal a) = a `deepseq` ()
{- 0x63 -} rnf (SetLocal a) = a `deepseq` ()
{- 0x64 -} {-GetGlobalScope-}
{- 0x65 -} rnf (GetScopeObject a) = a `deepseq` ()
{- 0x66 -} rnf (GetProperty a) = a `deepseq` ()
{- 0x67 -} {-GetPropertyLate-}
{- 0x68 -} rnf (InitProperty a) = a `deepseq` ()
{- 0x69 -} {-SetPropertyLate-}
{- 0x6A -} rnf (DeleteProperty a) = a `deepseq` ()
{- 0x6B -} {-DeletePropertyLate-}
{- 0x6C -} rnf (GetSlot a) = a `deepseq` ()
{- 0x6D -} rnf (SetSlot a) = a `deepseq` ()
{- 0x6E -} rnf (GetGlobalSlot a) = a `deepseq` ()
{- 0x6F -} rnf (SetGlobalSlot a) = a `deepseq` ()
{- 0x70 -} {-ConvertString-}
{- 0x71 -} {-EscXmlElem-}
{- 0x72 -} {-EscXmlAttr-}
{- 0x73 -} {-ConvertInt-}
{- 0x74 -} {-ConvertUInt-}
{- 0x75 -} {-ConvertDouble-}
{- 0x76 -} {-ConvertBoolean-}
{- 0x77 -} {-ConvertObject-}
{- 0x78 -} {-CheckFilter-}
{- 0x79 -}
{- 0x7A -}
{- 0x7B -}
{- 0x7C -}
{- 0x7D -}
{- 0x7E -}
{- 0x7F -}
{- 0x80 -} rnf (Coerce a) = a `deepseq` ()
{- 0x81 -} {-CoerceBoolean-}
{- 0x82 -} {-CoerceAny-}
{- 0x83 -} {-CoerceInt-}
{- 0x84 -} {-CoerceDouble-}
{- 0x85 -} {-CoerceString-}
{- 0x86 -} rnf (AsType a) = a `deepseq` ()
{- 0x87 -} {-AsTypeLate-}
{- 0x88 -} {-CoerceUInt-}
{- 0x89 -} {-CoerceObject-}
{- 0x8A -}
{- 0x8B -}
{- 0x8C -}
{- 0x8D -}
{- 0x8E -}
{- 0x8F -}
{- 0x90 -} {-Negate-}
{- 0x91 -} {-Increment-}
{- 0x92 -} {-IncLocal-}
{- 0x93 -} {-Decrement-}
{- 0x94 -} rnf (DecLocal a) = a `deepseq` ()
{- 0x95 -} {-TypeOf-}
{- 0x96 -} {-Not-}
{- 0x97 -} {-BitNot-}
{- 0x98 -}
{- 0x99 -}
{- 0x9A -} {-Concat-}
{- 0x9B -} {-AddDouble-}
{- 0x9C -}
{- 0x9D -}
{- 0x9E -}
{- 0x9F -}
{- 0xA0 -} {-Add-}
{- 0xA1 -} {-Subtract-}
{- 0xA2 -} {-Multiply-}
{- 0xA3 -} {-Divide-}
{- 0xA4 -} {-Modulo-}
{- 0xA5 -} {-ShiftLeft-}
{- 0xA6 -} {-ShiftRight-}
{- 0xA7 -} {-ShiftRightUnsigned-}
{- 0xA8 -} {-BitAnd-}
{- 0xA9 -} {-BitOr-}
{- 0xAA -} {-BitXor-}
{- 0xAB -} {-Equals-}
{- 0xAC -} {-StrictEquals-}
{- 0xAD -} {-LessThan-}
{- 0xAE -} {-LessEquals-}
{- 0xAF -} {-GreaterThan-}
{- 0xB0 -} {-GreaterEquals-}
{- 0xB1 -} {-InstanceOf-}
{- 0xB2 -} rnf (IsType a) = a `deepseq` ()
{- 0xB3 -} {-IsTypeLate-}
{- 0xB4 -} {-In-}
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
{- 0xC0 -} {-IncrementInt-}
{- 0xC1 -} {-DecrementInt-}
{- 0xC2 -} rnf (IncLocalInt a) = a `deepseq` ()
{- 0xC3 -} rnf (DecLocalInt a) = a `deepseq` ()
{- 0xC4 -} {-NegateInt-}
{- 0xC5 -} {-AddInt-}
{- 0xC6 -} {-SubtractInt-}
{- 0xC7 -} {-MultiplyInt-}
{- 0xC8 -}
{- 0xC9 -}
{- 0xCA -}
{- 0xCB -}
{- 0xCC -}
{- 0xCD -}
{- 0xCE -}
{- 0xCF -}
{- 0xD0 -} {-GetLocal0-}
{- 0xD1 -} {-GetLocal1-}
{- 0xD2 -} {-GetLocal2-}
{- 0xD3 -} {-GetLocal3-}
{- 0xD4 -} {-SetLocal0-}
{- 0xD5 -} {-SetLocal1-}
{- 0xD6 -} {-SetLocal2-}
{- 0xD7 -} {-SetLocal3-}
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
{- 0xEE -}
{- 0xEF -} rnf (Debug a b c d) = a
                `deepseq` b
                `deepseq` c
                `deepseq` d
                `deepseq` ()
{- 0xF0 -} rnf (DebugLine a) = a `deepseq` ()
{- 0xF1 -} rnf (DebugFile a) = a `deepseq` ()
{- 0xF2 -} {-BreakpointLine-}
{- 0xF3 -}
{- 0xF5 -}
{- 0xF6 -}
{- 0xF7 -}
{- 0xF8 -}
{- 0xF9 -}
{- 0xFA -}
{- 0xFB -}
{- 0xFC -}
{- 0xFD -}
{- 0xFE -}
{- 0xFF -}
           rnf _ = ()
