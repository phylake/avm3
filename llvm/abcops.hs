module LLVM.AbcOps where

import           Data.Word
import           LLVM.Lang
import qualified Abc.Def as Abc
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

data OpCodeBranch = BranchIdx Abc.S24 | BranchLabel Label

{-
A superset of Abc.OpCode
-}
data OpCode = {- 0x01 -} Breakpoint
            | {- 0x02 -} Nop
            | {- 0x03 -} Throw
            | {- 0x04 -} GetSuper Abc.MultinameIdx (Maybe String)
            | {- 0x05 -} SetSuper Abc.MultinameIdx (Maybe String)
            | {- 0x06 -} DefaultXmlNamespace Abc.U30
            | {- 0x07 -} DefaultXmlNamespaceL
            | {- 0x08 -} Kill Abc.U30
            | {- 0x09 -} Label
              {- 0x0A -}
              {- 0x0B -}
            | {- 0x0C -} IfNotLessThan Abc.S24 (Maybe Label) (Maybe Label)
            | {- 0x0D -} IfNotLessEqual Abc.S24 (Maybe Label) (Maybe Label)
            | {- 0x0E -} IfNotGreaterThan Abc.S24 (Maybe Label) (Maybe Label)
            | {- 0x0F -} IfNotGreaterEqual Abc.S24 (Maybe Label) (Maybe Label)
            | {- 0x10 -} Jump Abc.S24 (Maybe Label)
            | {- 0x11 -} IfTrue Abc.S24 (Maybe Label) (Maybe Label)
            | {- 0x12 -} IfFalse Abc.S24 (Maybe Label) (Maybe Label)
            | {- 0x13 -} IfEqual Abc.S24 (Maybe Label) (Maybe Label)
            | {- 0x14 -} IfNotEqual Abc.S24 (Maybe Label) (Maybe Label)
            | {- 0x15 -} IfLessThan Abc.S24 (Maybe Label) (Maybe Label)
            | {- 0x16 -} IfLessEqual Abc.S24 (Maybe Label) (Maybe Label)
            | {- 0x17 -} IfGreaterThan Abc.S24 (Maybe Label) (Maybe Label)
            | {- 0x18 -} IfGreaterEqual Abc.S24 (Maybe Label) (Maybe Label)
            | {- 0x19 -} IfStrictEqual Abc.S24 (Maybe Label) (Maybe Label)
            | {- 0x1A -} IfStrictNotEqual Abc.S24 (Maybe Label) (Maybe Label)
            | {- 0x1B -} LookupSwitch Abc.S24 (Maybe Label) [Abc.S24] [Maybe Label] {- default offset, case offsets -}
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
            | {- 0x2C -} PushString Abc.StringIdx String
            | {- 0x2D -} PushInt Abc.IntIdx Abc.S32
            | {- 0x2E -} PushUInt Abc.UintIdx Abc.U32
            | {- 0x2F -} PushDouble Abc.DoubleIdx Double
            | {- 0x30 -} PushScope
            | {- 0x31 -} PushNamespace Abc.NSInfoIdx
            | {- 0x32 -} HasNext2 Word32 Word32
            | {- 0x33 -} PushDecimal    {-NEW: PushDecimal according to FlexSDK, lix8 according to Tamarin-}
            | {- 0x34 -} PushDNaN       {-NEW: PushDNaN according to Flex SDK, lix16 according to Tamarin-}
              {- 0x35 -} {-GetByte-}   {- Alchemy -}
              {- 0x36 -} {-GetShort-}  {- Alchemy -}
              {- 0x37 -} {-GetInt-}    {- Alchemy -}
              {- 0x38 -} {-GetFloat-}  {- Alchemy -}
              {- 0x39 -} {-GetDouble-} {- Alchemy -}
              {- 0x3A -} {-SetByte-}   {- Alchemy -}
              {- 0x3B -} {-SetShort-}  {- Alchemy -}
              {- 0x3C -} {-SetInt-}    {- Alchemy -}
              {- 0x3D -} {-SetFloat-}  {- Alchemy -}
              {- 0x3E -} {-SetDouble-} {- Alchemy -}
              {- 0x3F -}
            | {- 0x40 -} NewFunction Abc.U30
            | {- 0x41 -} Call Abc.U30
            | {- 0x42 -} Construct Abc.U30
            | {- 0x43 -} CallMethod Abc.MultinameIdx Abc.U30 (Maybe String)
            | {- 0x44 -} CallStatic Abc.MultinameIdx Abc.U30 (Maybe String)
            | {- 0x45 -} CallSuper Abc.MultinameIdx Abc.U30 (Maybe String)
            | {- 0x46 -} CallProperty Abc.MultinameIdx Abc.U30 (Maybe String)
            | {- 0x47 -} ReturnVoid
            | {- 0x48 -} ReturnValue
            | {- 0x49 -} ConstructSuper Abc.U30
            | {- 0x4A -} ConstructProp Abc.MultinameIdx Abc.U30 (Maybe String)
            | {- 0x4B -} CallSuperId    {-NOT HANDLED-}
            | {- 0x4C -} CallPropLex Abc.MultinameIdx Abc.U30 (Maybe String)
            | {- 0x4D -} CallInterface  {-NOT HANDLED-}
            | {- 0x4E -} CallSuperVoid Abc.MultinameIdx Abc.U30 (Maybe String)
            | {- 0x4F -} CallPropVoid Abc.MultinameIdx Abc.U30 (Maybe String)
              {- 0x50 -} {-Sign1-}  {- Alchemy -}
              {- 0x51 -} {-Sign8-}  {- Alchemy -}
              {- 0x52 -} {-Sign16-} {- Alchemy -}
            | {- 0x53 -} ApplyType
              {- 0x53 -}
            | {- 0x55 -} NewObject Abc.U30
            | {- 0x56 -} NewArray Abc.U30
            | {- 0x57 -} NewActivation
            | {- 0x58 -} NewClass Abc.ClassInfoIdx
            | {- 0x59 -} GetDescendants Abc.MultinameIdx (Maybe String)
            | {- 0x5A -} NewCatch Abc.ExceptionIdx
            | {- 0x5B -} FindPropGlobalStrict   {-NEW from Tamarin (internal)-}
            | {- 0x5C -} FindPropGlobal         {-NEW from Tamarin (internal)-}
            | {- 0x5D -} FindPropStrict Abc.MultinameIdx (Maybe String)
            | {- 0x5E -} FindProperty Abc.MultinameIdx (Maybe String)
            | {- 0x5F -} FindDef        {-NOT HANDLED-}
            | {- 0x60 -} GetLex Abc.MultinameIdx (Maybe String)
            | {- 0x61 -} SetProperty Abc.MultinameIdx (Maybe String)
            | {- 0x62 -} GetLocal Abc.U30
            | {- 0x63 -} SetLocal Abc.U30
            | {- 0x64 -} GetGlobalScope
            | {- 0x65 -} GetScopeObject Abc.U8
            | {- 0x66 -} GetProperty Abc.MultinameIdx (Maybe String)
            | {- 0x67 -} GetPropertyLate
            | {- 0x68 -} InitProperty Abc.MultinameIdx (Maybe String)
            | {- 0x69 -} SetPropertyLate
            | {- 0x6A -} DeleteProperty Abc.MultinameIdx (Maybe String)
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
            | {- 0x80 -} Coerce Abc.MultinameIdx (Maybe String)
            | {- 0x81 -} CoerceBoolean
            | {- 0x82 -} CoerceAny
            | {- 0x83 -} CoerceInt
            | {- 0x84 -} CoerceDouble
            | {- 0x85 -} CoerceString
            | {- 0x86 -} AsType Abc.MultinameIdx (Maybe String)
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
            | {- 0x92 -} IncLocal Abc.U30
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
            | {- 0xB2 -} IsType Abc.MultinameIdx (Maybe String)
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

abcToLLVM :: (Abc.IntIdx -> Abc.S32)            -- int resolution
          -> (Abc.UintIdx -> Abc.U32)           -- uint resolution
          -> (Abc.DoubleIdx -> Double)          -- double resolution
          -> (Abc.StringIdx -> String)          -- string resolution
          -> (Abc.MultinameIdx -> Maybe String) -- multiname resolution
          -> (Abc.U30 -> Abc.TraitsInfo)        -- trait slot resolution
          -> Abc.OpCode
          -> [OpCode]
abcToLLVM {- 0x01 -} i u d s m t (Abc.Breakpoint) = [Breakpoint]
abcToLLVM {- 0x02 -} i u d s m t (Abc.Nop) = [Nop]
abcToLLVM {- 0x03 -} i u d s m t (Abc.Throw) = [Throw]
abcToLLVM {- 0x04 -} i u d s m t (Abc.GetSuper u30) = [GetSuper u30 $ m u30]
abcToLLVM {- 0x05 -} i u d s m t (Abc.SetSuper u30) = [SetSuper u30 $ m u30]
abcToLLVM {- 0x06 -} i u d s m t (Abc.DefaultXmlNamespace u30) = [DefaultXmlNamespace u30]
abcToLLVM {- 0x07 -} i u d s m t (Abc.DefaultXmlNamespaceL) = [DefaultXmlNamespaceL]
abcToLLVM {- 0x08 -} i u d s m t (Abc.Kill u30) = [Kill u30]
abcToLLVM {- 0x09 -} i u d s m t (Abc.Label) = [Label]
abcToLLVM {- 0x0C -} i u d s m t (Abc.IfNotLessThan s24) = [IfNotLessThan s24 Nothing Nothing]
abcToLLVM {- 0x0D -} i u d s m t (Abc.IfNotLessEqual s24) = [IfNotLessEqual s24 Nothing Nothing]
abcToLLVM {- 0x0E -} i u d s m t (Abc.IfNotGreaterThan s24) = [IfNotGreaterThan s24 Nothing Nothing]
abcToLLVM {- 0x0F -} i u d s m t (Abc.IfNotGreaterEqual s24) = [IfNotGreaterEqual s24 Nothing Nothing]
abcToLLVM {- 0x10 -} i u d s m t (Abc.Jump s24) = [Jump s24 Nothing]
abcToLLVM {- 0x11 -} i u d s m t (Abc.IfTrue s24) = [IfTrue s24 Nothing Nothing]
abcToLLVM {- 0x12 -} i u d s m t (Abc.IfFalse s24) = [IfFalse s24 Nothing Nothing]
abcToLLVM {- 0x13 -} i u d s m t (Abc.IfEqual s24) = [IfEqual s24 Nothing Nothing]
abcToLLVM {- 0x14 -} i u d s m t (Abc.IfNotEqual s24) = [IfNotEqual s24 Nothing Nothing]
abcToLLVM {- 0x15 -} i u d s m t (Abc.IfLessThan s24) = [IfLessThan s24 Nothing Nothing]
abcToLLVM {- 0x16 -} i u d s m t (Abc.IfLessEqual s24) = [IfLessEqual s24 Nothing Nothing]
abcToLLVM {- 0x17 -} i u d s m t (Abc.IfGreaterThan s24) = [IfGreaterThan s24 Nothing Nothing]
abcToLLVM {- 0x18 -} i u d s m t (Abc.IfGreaterEqual s24) = [IfGreaterEqual s24 Nothing Nothing]
abcToLLVM {- 0x19 -} i u d s m t (Abc.IfStrictEqual s24) = [IfStrictEqual s24 Nothing Nothing]
abcToLLVM {- 0x1A -} i u d s m t (Abc.IfStrictNotEqual s24) = [IfStrictNotEqual s24 Nothing Nothing]
abcToLLVM {- 0x1B -} i u d s m t (Abc.LookupSwitch s24 s24s) = [LookupSwitch s24 Nothing s24s []]
abcToLLVM {- 0x1C -} i u d s m t (Abc.PushWith) = [PushWith]
abcToLLVM {- 0x1D -} i u d s m t (Abc.PopScope) = [PopScope]
abcToLLVM {- 0x1E -} i u d s m t (Abc.NextName) = [NextName]
abcToLLVM {- 0x1F -} i u d s m t (Abc.HasNext) = [HasNext]
abcToLLVM {- 0x20 -} i u d s m t (Abc.PushNull) = [PushNull]
abcToLLVM {- 0x21 -} i u d s m t (Abc.PushUndefined) = [PushUndefined]
abcToLLVM {- 0x22 -} i u d s m t (Abc.PushConstant) = [PushConstant]
abcToLLVM {- 0x23 -} i u d s m t (Abc.NextValue) = [NextValue]
abcToLLVM {- 0x24 -} i u d s m t (Abc.PushByte u8) = [PushByte u8]
abcToLLVM {- 0x25 -} i u d s m t (Abc.PushShort u30) = [PushShort u30]
abcToLLVM {- 0x26 -} i u d s m t (Abc.PushTrue) = [PushTrue]
abcToLLVM {- 0x27 -} i u d s m t (Abc.PushFalse) = [PushFalse]
abcToLLVM {- 0x28 -} i u d s m t (Abc.PushNaN) = [PushNaN]
abcToLLVM {- 0x29 -} i u d s m t (Abc.Pop) = [Pop]
abcToLLVM {- 0x2A -} i u d s m t (Abc.Dup) = [Dup]
abcToLLVM {- 0x2B -} i u d s m t (Abc.Swap) = [Swap]
abcToLLVM {- 0x2C -} i u d s m t (Abc.PushString u30) = [PushString u30 $ s u30]
abcToLLVM {- 0x2D -} i u d s m t (Abc.PushInt u30) = [PushInt u30 $ i u30]
abcToLLVM {- 0x2E -} i u d s m t (Abc.PushUInt u30) = [PushUInt u30 $ u u30]
abcToLLVM {- 0x2F -} i u d s m t (Abc.PushDouble u30) = [PushDouble u30 $ d u30]
abcToLLVM {- 0x30 -} i u d s m t (Abc.PushScope) = [PushScope]
abcToLLVM {- 0x31 -} i u d s m t (Abc.PushNamespace u30) = [PushNamespace u30]
abcToLLVM {- 0x32 -} i u d s m t (Abc.HasNext2 w32 w32_2) = [HasNext2 w32 w32_2]
abcToLLVM {- 0x33 -} i u d s m t (Abc.PushDecimal) = [PushDecimal]
abcToLLVM {- 0x34 -} i u d s m t (Abc.PushDNaN) = [PushDNaN]
abcToLLVM {- 0x40 -} i u d s m t (Abc.NewFunction u30) = [NewFunction u30]
abcToLLVM {- 0x41 -} i u d s m t (Abc.Call u30) = [Call u30]
abcToLLVM {- 0x42 -} i u d s m t (Abc.Construct u30) = [Construct u30]
abcToLLVM {- 0x43 -} i u d s m t (Abc.CallMethod u30_1 u30_2) = [CallMethod u30_1 u30_2 $ m u30_1]
abcToLLVM {- 0x44 -} i u d s m t (Abc.CallStatic u30_1 u30_2) = [CallStatic u30_1 u30_2 $ m u30_1]
abcToLLVM {- 0x45 -} i u d s m t (Abc.CallSuper u30_1 u30_2) = [CallSuper u30_1 u30_2 $ m u30_1]
abcToLLVM {- 0x46 -} i u d s m t (Abc.CallProperty u30_1 u30_2) = [CallProperty u30_1 u30_2 $ m u30_1]
abcToLLVM {- 0x47 -} i u d s m t (Abc.ReturnVoid) = [ReturnVoid]
abcToLLVM {- 0x48 -} i u d s m t (Abc.ReturnValue) = [ReturnValue]
abcToLLVM {- 0x49 -} i u d s m t (Abc.ConstructSuper u30) = [ConstructSuper u30]
abcToLLVM {- 0x4A -} i u d s m t (Abc.ConstructProp u30_1 u30_2) = [ConstructProp u30_1 u30_2 $ m u30_1]
abcToLLVM {- 0x4B -} i u d s m t (Abc.CallSuperId) = [CallSuperId]
abcToLLVM {- 0x4C -} i u d s m t (Abc.CallPropLex u30_1 u30_2) = [CallPropLex u30_1 u30_2 $ m u30_1]
abcToLLVM {- 0x4D -} i u d s m t (Abc.CallInterface) = [CallInterface]
abcToLLVM {- 0x4E -} i u d s m t (Abc.CallSuperVoid u30_1 u30_2) = [CallSuperVoid u30_1 u30_2 $ m u30_1]
abcToLLVM {- 0x4F -} i u d s m t (Abc.CallPropVoid u30_1 u30_2) = [CallPropVoid u30_1 u30_2 $ m u30_1]
abcToLLVM {- 0x53 -} i u d s m t (Abc.ApplyType) = [ApplyType]
abcToLLVM {- 0x55 -} i u d s m t (Abc.NewObject u30) = [NewObject u30]
abcToLLVM {- 0x56 -} i u d s m t (Abc.NewArray u30) = [NewArray u30]
abcToLLVM {- 0x57 -} i u d s m t (Abc.NewActivation) = [NewActivation]
abcToLLVM {- 0x58 -} i u d s m t (Abc.NewClass u30) = [NewClass u30]
abcToLLVM {- 0x59 -} i u d s m t (Abc.GetDescendants u30) = [GetDescendants u30 $ m u30]
abcToLLVM {- 0x5A -} i u d s m t (Abc.NewCatch u30) = [NewCatch u30]
abcToLLVM {- 0x5B -} i u d s m t (Abc.FindPropGlobalStrict) = [FindPropGlobalStrict]
abcToLLVM {- 0x5C -} i u d s m t (Abc.FindPropGlobal) = [FindPropGlobal]
abcToLLVM {- 0x5D -} i u d s m t (Abc.FindPropStrict u30) = [FindPropStrict u30 $ m u30]
abcToLLVM {- 0x5E -} i u d s m t (Abc.FindProperty u30) = [FindProperty u30 $ m u30]
abcToLLVM {- 0x5F -} i u d s m t (Abc.FindDef) = [FindDef]
abcToLLVM {- 0x60 -} i u d s m t (Abc.GetLex idx) = [FindPropStrict idx$ m idx, GetProperty idx$ m idx]
abcToLLVM {- 0x61 -} i u d s m t (Abc.SetProperty u30) = [SetProperty u30 $ m u30]
abcToLLVM {- 0x62 -} i u d s m t (Abc.GetLocal u30) = [GetLocal u30]
abcToLLVM {- 0x63 -} i u d s m t (Abc.SetLocal u30) = [SetLocal u30]
abcToLLVM {- 0x64 -} i u d s m t (Abc.GetGlobalScope) = [GetGlobalScope]
abcToLLVM {- 0x65 -} i u d s m t (Abc.GetScopeObject u8) = [GetScopeObject u8]
abcToLLVM {- 0x66 -} i u d s m t (Abc.GetProperty u30) = [GetProperty u30 $ m u30]
abcToLLVM {- 0x67 -} i u d s m t (Abc.GetPropertyLate) = [GetPropertyLate]
abcToLLVM {- 0x68 -} i u d s m t (Abc.InitProperty u30) = [InitProperty u30 $ m u30]
abcToLLVM {- 0x69 -} i u d s m t (Abc.SetPropertyLate) = [SetPropertyLate]
abcToLLVM {- 0x6A -} i u d s m t (Abc.DeleteProperty u30) = [DeleteProperty u30 $ m u30]
abcToLLVM {- 0x6B -} i u d s m t (Abc.DeletePropertyLate) = [DeletePropertyLate]
abcToLLVM {- 0x6C -} i u d s m t (Abc.GetSlot u30) = [GetSlot u30{- $ t u30-}]
abcToLLVM {- 0x6D -} i u d s m t (Abc.SetSlot u30) = [SetSlot u30{- $ t u30-}]
abcToLLVM {- 0x6E -} i u d s m t (Abc.GetGlobalSlot u30) = [GetGlobalSlot u30]
abcToLLVM {- 0x6F -} i u d s m t (Abc.SetGlobalSlot u30) = [SetGlobalSlot u30]
abcToLLVM {- 0x70 -} i u d s m t (Abc.ConvertString) = [ConvertString]
abcToLLVM {- 0x71 -} i u d s m t (Abc.EscXmlElem) = [EscXmlElem]
abcToLLVM {- 0x72 -} i u d s m t (Abc.EscXmlAttr) = [EscXmlAttr]
abcToLLVM {- 0x73 -} i u d s m t (Abc.ConvertInt) = [ConvertInt]
abcToLLVM {- 0x74 -} i u d s m t (Abc.ConvertUInt) = [ConvertUInt]
abcToLLVM {- 0x75 -} i u d s m t (Abc.ConvertDouble) = [ConvertDouble]
abcToLLVM {- 0x76 -} i u d s m t (Abc.ConvertBoolean) = [ConvertBoolean]
abcToLLVM {- 0x77 -} i u d s m t (Abc.ConvertObject) = [ConvertObject]
abcToLLVM {- 0x78 -} i u d s m t (Abc.CheckFilter) = [CheckFilter]
abcToLLVM {- 0x80 -} i u d s m t (Abc.Coerce u30) = [Coerce u30 $ m u30]
abcToLLVM {- 0x81 -} i u d s m t (Abc.CoerceBoolean) = [CoerceBoolean]
abcToLLVM {- 0x82 -} i u d s m t (Abc.CoerceAny) = [CoerceAny]
abcToLLVM {- 0x83 -} i u d s m t (Abc.CoerceInt) = [CoerceInt]
abcToLLVM {- 0x84 -} i u d s m t (Abc.CoerceDouble) = [CoerceDouble]
abcToLLVM {- 0x85 -} i u d s m t (Abc.CoerceString) = [CoerceString]
abcToLLVM {- 0x86 -} i u d s m t (Abc.AsType u30) = [AsType u30 $ m u30]
abcToLLVM {- 0x87 -} i u d s m t (Abc.AsTypeLate) = [AsTypeLate]
abcToLLVM {- 0x88 -} i u d s m t (Abc.CoerceUInt) = [CoerceUInt]
abcToLLVM {- 0x89 -} i u d s m t (Abc.CoerceObject) = [CoerceObject]
abcToLLVM {- 0x90 -} i u d s m t (Abc.Negate) = [Negate]
abcToLLVM {- 0x91 -} i u d s m t (Abc.Increment) = [Increment]
abcToLLVM {- 0x92 -} i u d s m t (Abc.IncLocal u30) = [IncLocal u30]
abcToLLVM {- 0x93 -} i u d s m t (Abc.Decrement) = [Decrement]
abcToLLVM {- 0x94 -} i u d s m t (Abc.DecLocal u30) = [DecLocal u30]
abcToLLVM {- 0x95 -} i u d s m t (Abc.TypeOf) = [TypeOf]
abcToLLVM {- 0x96 -} i u d s m t (Abc.Not) = [Not]
abcToLLVM {- 0x97 -} i u d s m t (Abc.BitNot) = [BitNot]
abcToLLVM {- 0x9A -} i u d s m t (Abc.Concat) = [Concat]
abcToLLVM {- 0x9B -} i u d s m t (Abc.AddDouble) = [AddDouble]
abcToLLVM {- 0xA0 -} i u d s m t (Abc.Add) = [Add]
abcToLLVM {- 0xA1 -} i u d s m t (Abc.Subtract) = [Subtract]
abcToLLVM {- 0xA2 -} i u d s m t (Abc.Multiply) = [Multiply]
abcToLLVM {- 0xA3 -} i u d s m t (Abc.Divide) = [Divide]
abcToLLVM {- 0xA4 -} i u d s m t (Abc.Modulo) = [Modulo]
abcToLLVM {- 0xA5 -} i u d s m t (Abc.ShiftLeft) = [ShiftLeft]
abcToLLVM {- 0xA6 -} i u d s m t (Abc.ShiftRight) = [ShiftRight]
abcToLLVM {- 0xA7 -} i u d s m t (Abc.ShiftRightUnsigned) = [ShiftRightUnsigned]
abcToLLVM {- 0xA8 -} i u d s m t (Abc.BitAnd) = [BitAnd]
abcToLLVM {- 0xA9 -} i u d s m t (Abc.BitOr) = [BitOr]
abcToLLVM {- 0xAA -} i u d s m t (Abc.BitXor) = [BitXor]
abcToLLVM {- 0xAB -} i u d s m t (Abc.Equals) = [Equals]
abcToLLVM {- 0xAC -} i u d s m t (Abc.StrictEquals) = [StrictEquals]
abcToLLVM {- 0xAD -} i u d s m t (Abc.LessThan) = [LessThan]
abcToLLVM {- 0xAE -} i u d s m t (Abc.LessEquals) = [LessEquals]
abcToLLVM {- 0xAF -} i u d s m t (Abc.GreaterThan) = [GreaterThan]
abcToLLVM {- 0xB0 -} i u d s m t (Abc.GreaterEquals) = [GreaterEquals]
abcToLLVM {- 0xB1 -} i u d s m t (Abc.InstanceOf) = [InstanceOf]
abcToLLVM {- 0xB2 -} i u d s m t (Abc.IsType u30) = [IsType u30 $ m u30]
abcToLLVM {- 0xB3 -} i u d s m t (Abc.IsTypeLate) = [IsTypeLate]
abcToLLVM {- 0xB4 -} i u d s m t (Abc.In) = [In]
abcToLLVM {- 0xC0 -} i u d s m t (Abc.IncrementInt) = [IncrementInt]
abcToLLVM {- 0xC1 -} i u d s m t (Abc.DecrementInt) = [DecrementInt]
abcToLLVM {- 0xC2 -} i u d s m t (Abc.IncLocalInt u30) = [IncLocalInt u30]
abcToLLVM {- 0xC3 -} i u d s m t (Abc.DecLocalInt u30) = [DecLocalInt u30]
abcToLLVM {- 0xC4 -} i u d s m t (Abc.NegateInt) = [NegateInt]
abcToLLVM {- 0xC5 -} i u d s m t (Abc.AddInt) = [AddInt]
abcToLLVM {- 0xC6 -} i u d s m t (Abc.SubtractInt) = [SubtractInt]
abcToLLVM {- 0xC7 -} i u d s m t (Abc.MultiplyInt) = [MultiplyInt]
abcToLLVM {- 0xD0 -} i u d s m t (Abc.GetLocal0) = [GetLocal 0]
abcToLLVM {- 0xD1 -} i u d s m t (Abc.GetLocal1) = [GetLocal 1]
abcToLLVM {- 0xD2 -} i u d s m t (Abc.GetLocal2) = [GetLocal 2]
abcToLLVM {- 0xD3 -} i u d s m t (Abc.GetLocal3) = [GetLocal 3]
abcToLLVM {- 0xD4 -} i u d s m t (Abc.SetLocal0) = [SetLocal 0]
abcToLLVM {- 0xD5 -} i u d s m t (Abc.SetLocal1) = [SetLocal 1]
abcToLLVM {- 0xD6 -} i u d s m t (Abc.SetLocal2) = [SetLocal 2]
abcToLLVM {- 0xD7 -} i u d s m t (Abc.SetLocal3) = [SetLocal 3]
abcToLLVM {- 0xEF -} i u d s m t (Abc.Debug u8_1 u30_1 u8_2 u30_2) = [Debug u8_1 u30_1 u8_2 u30_2]
abcToLLVM {- 0xF0 -} i u d s m t (Abc.DebugLine u30) = [DebugLine u30]
abcToLLVM {- 0xF1 -} i u d s m t (Abc.DebugFile u30) = [DebugFile u30]
abcToLLVM {- 0xF2 -} i u d s m t (Abc.BreakpointLine) = [BreakpointLine]
abcToLLVM            i u d s m t _ = []
