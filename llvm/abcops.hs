module LLVM.AbcOps (OpCode(..)) where

import           Data.Word
import           LLVM.Lang
import qualified Abc.Def as Abc

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
            | {- 0x0C -} IfNotLessThan Label Label
            | {- 0x0D -} IfNotLessEqual Label Label
            | {- 0x0E -} IfNotGreaterThan Label Label
            | {- 0x0F -} IfNotGreaterEqual Label Label
            | {- 0x10 -} Jump Label
            | {- 0x11 -} IfTrue Label Label
            | {- 0x12 -} IfFalse Label Label
            | {- 0x13 -} IfEqual Label Label
            | {- 0x14 -} IfNotEqual Label Label
            | {- 0x15 -} IfLessThan Label Label
            | {- 0x16 -} IfLessEqual Label Label
            | {- 0x17 -} IfGreaterThan Label Label
            | {- 0x18 -} IfGreaterEqual Label Label
            | {- 0x19 -} IfStrictEqual Label Label
            | {- 0x1A -} IfStrictNotEqual Label Label
            | {- 0x1B -} LookupSwitch Label [Label] {- default offset, case offsets -}
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
            | {- 0x2C -} PushString String
            | {- 0x2D -} PushInt Abc.S32
            | {- 0x2E -} PushUInt Abc.U32
            | {- 0x2F -} PushDouble Double
            | {- 0x30 -} PushScope
            | {- 0x31 -} PushNamespace Abc.NSInfoIdx
            | {- 0x32 -} HasNext2 Word32 Word32
            | {- 0x33 -} PushDecimal
            | {- 0x34 -} PushDNaN
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
            | {- 0x4B -} CallSuperId
            | {- 0x4C -} CallPropLex Abc.MultinameIdx Abc.U30 (Maybe String)
            | {- 0x4D -} CallInterface
            | {- 0x4E -} CallSuperVoid Abc.MultinameIdx Abc.U30 (Maybe String)
            | {- 0x4F -} CallPropVoid Abc.MultinameIdx Abc.U30 (Maybe String)
              {- 0x50 -}
              {- 0x51 -}
              {- 0x52 -}
            | {- 0x53 -} ApplyType
              {- 0x53 -}
            | {- 0x55 -} NewObject Abc.U30
            | {- 0x56 -} NewArray Abc.U30
            | {- 0x57 -} NewActivation
            | {- 0x58 -} NewClass Abc.ClassInfoIdx
            | {- 0x59 -} GetDescendants Abc.MultinameIdx (Maybe String)
            | {- 0x5A -} NewCatch Abc.ExceptionIdx
            | {- 0x5B -} FindPropGlobalStrict
            | {- 0x5C -} FindPropGlobal
            | {- 0x5D -} FindPropStrict Abc.MultinameIdx (Maybe String)
            | {- 0x5E -} FindProperty Abc.MultinameIdx (Maybe String)
            | {- 0x5F -} FindDef
            | {- 0x60 -} GetLex Abc.MultinameIdx (Maybe String)
            | {- 0x61 -} SetProperty Abc.MultinameIdx (Maybe String)
            | {- 0x62 -} GetLocal Int
            | {- 0x63 -} SetLocal Int
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
              {- 0x79 -}
              {- 0x7A -}
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
              {- 0x8F -}
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
              {- 0x9C -}
              {- 0x9D -}
              {- 0x9E -}
              {- 0x9F -}
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
              {- 0xC2 -}
              {- 0xC3 -}
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
              {- 0xD0 -}
              {- 0xD1 -}
              {- 0xD2 -}
              {- 0xD3 -}
              {- 0xD4 -}
              {- 0xD5 -}
              {- 0xD6 -}
              {- 0xD7 -}
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
