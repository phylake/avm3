module Abc.Def where

import           Data.Enumerator as E
import           Data.Enumerator.Binary as EB
import           Data.Enumerator.List as EL
import           Data.Int
import           Data.Word
import           Util.Words (u30Bytes)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

type Parser = Iteratee B.ByteString IO

type U8 = Word8
type U30 = Word32
type U32 = Word32
type S24 = Int32

type IntIdx = U30
type UintIdx = U30
type DoubleIdx = U30
type StringIdx = U30
type NSInfoIdx = U30
type NSSetIdx = U30
type MultinameIdx = U30
type MethodSignatureIdx = U30
type MetadataIdx = U30
type InstanceInfoIdx = U30
type ClassInfoIdx = U30
type ScriptInfoIdx = U30
type MethodBodyIdx = U30

{-
    4.2
    Abc
-}
data Abc = Abc {
                 abcInts :: [Int32]
               , abcUints :: [U30]
               , abcDoubles :: [Double]
               , abcStrings :: [String]
               , abcNsInfo :: [NSInfo]
               , abcNsSet :: [NSSet]
               , abcMultinames :: [Multiname]
               , abcMethodSigs :: [MethodSignature]
               , abcMetadata :: [Metadata]
               , abcInstances :: [InstanceInfo]
               , abcClasses :: [ClassInfo]
               , abcScripts :: [ScriptInfo]
               , abcMethodBodies :: [MethodBody]
               }
               deriving (Show)

{-
    4.4.1
    Namespace
-}
data NSInfo = {- 0x08 -} NSInfo_Namespace StringIdx
            | {- 0x16 -} NSInfo_PackageNamespace StringIdx
            | {- 0x17 -} NSInfo_PackageInternalNs StringIdx
            | {- 0x18 -} NSInfo_ProtectedNamespace StringIdx
            | {- 0x19 -} NSInfo_ExplicitNamespace StringIdx
            | {- 0x1A -} NSInfo_StaticProtectedNs StringIdx
            | {- 0x05 -} NSInfo_PrivateNs StringIdx
            |            NSInfo_Any {- '*' see 4.4.4 QName -}
            deriving (Show)

{-
    4.4.2
    Namespace set
-}
type NSSet = [NSInfoIdx]

{-
    4.4.3
    Multiname
-}
data Multiname = {- 0x07 -} Multiname_QName NSInfoIdx StringIdx
               | {- 0x0D -} Multiname_QNameA NSInfoIdx StringIdx
               | {- 0x0F -} Multiname_RTQName StringIdx
               | {- 0x10 -} Multiname_RTQNameA StringIdx
               | {- 0x11 -} Multiname_RTQNameL
               | {- 0x12 -} Multiname_RTQNameLA
               | {- 0x09 -} Multiname_Multiname StringIdx NSSetIdx
               | {- 0x0E -} Multiname_MultinameA StringIdx NSSetIdx
               | {- 0x1B -} Multiname_MultinameL NSSetIdx
               | {- 0x1C -} Multiname_MultinameLA NSSetIdx
               |            Multiname_Any
               deriving (Show)

{-
    4.5
    Method signature
-}
data MethodSignature = MethodSignature {
                                         msReturnType :: MultinameIdx
                                       , msParamTypes :: [MultinameIdx]
                                       , msMethodName :: StringIdx -- debug
                                       , msFlags :: Word8
                                       , msOptionInfo :: Maybe [CPC]
                                       , msParamNames :: Maybe [StringIdx] -- debug
                                       }
                                       deriving (Show)

msflag_NEED_ARGUMENTS :: Word8
msflag_NEED_ARGUMENTS = 0x01

msflag_NEED_ACTIVATION :: Word8
msflag_NEED_ACTIVATION = 0x02

msflag_NEED_REST :: Word8
msflag_NEED_REST = 0x04

msflag_HAS_OPTIONAL :: Word8
msflag_HAS_OPTIONAL = 0x08

msflag_IGNORE_REST :: Word8
msflag_IGNORE_REST = 0x10

msflag_NATIVE :: Word8
msflag_NATIVE = 0x20

msflag_SET_DNX :: Word8
msflag_SET_DNX = 0x40

msflag_HAS_PARAM_NAMES :: Word8
msflag_HAS_PARAM_NAMES = 0x80

{-
    4.5.1
    Optional parameters
-}
cpc_Undefined :: Word8
cpc_Undefined = 0x00

cpc_Utf8 :: Word8
cpc_Utf8 = 0x01

cpc_Decimal :: Word8
cpc_Decimal = 0x02

cpc_Int :: Word8
cpc_Int = 0x03

cpc_Uint :: Word8
cpc_Uint = 0x04

cpc_PrivateNamespace :: Word8
cpc_PrivateNamespace = 0x05

cpc_Double :: Word8
cpc_Double = 0x06

cpc_QName :: Word8
cpc_QName = 0x07

cpc_Namespace :: Word8
cpc_Namespace = 0x08

cpc_Multiname :: Word8
cpc_Multiname = 0x09

cpc_False :: Word8
cpc_False = 0x0A

cpc_True :: Word8
cpc_True = 0x0B

cpc_Null :: Word8
cpc_Null = 0x0C

cpc_QNameA :: Word8
cpc_QNameA = 0x0D

cpc_MultinameA :: Word8
cpc_MultinameA = 0x0E

cpc_RTQName :: Word8
cpc_RTQName = 0x0F

cpc_RTQNameA :: Word8
cpc_RTQNameA = 0x10

cpc_RTQNameL :: Word8
cpc_RTQNameL = 0x11

cpc_RTQNameLA :: Word8
cpc_RTQNameLA = 0x12

cpc_NameL :: Word8
cpc_NameL = 0x13

cpc_NameLA :: Word8
cpc_NameLA = 0x14

cpc_NamespaceSet :: Word8
cpc_NamespaceSet = 0x15

cpc_PackageNamespace :: Word8
cpc_PackageNamespace = 0x16

cpc_PackageInternalNs :: Word8
cpc_PackageInternalNs = 0x17

cpc_ProtectedNamespace :: Word8
cpc_ProtectedNamespace = 0x18

cpc_ExplicitNamespace :: Word8
cpc_ExplicitNamespace = 0x19

cpc_StaticProtectedNs :: Word8
cpc_StaticProtectedNs = 0x1A

cpc_MultinameL :: Word8
cpc_MultinameL = 0x1B

cpc_MultinameLA :: Word8
cpc_MultinameLA = 0x1C

data CPC = {- 0x00 -} CPC_Undefined
         | {- 0x01 -} CPC_Utf8 U30
         | {- 0x02 -} CPC_Decimal U30
         | {- 0x03 -} CPC_Int U30
         | {- 0x04 -} CPC_Uint U30
         | {- 0x05 -} CPC_PrivateNamespace U30
         | {- 0x06 -} CPC_Double U30
         | {- 0x07 -} CPC_QName U30
         | {- 0x08 -} CPC_Namespace U30
         | {- 0x09 -} CPC_Multiname U30
         | {- 0x0A -} CPC_False
         | {- 0x0B -} CPC_True
         | {- 0x0C -} CPC_Null
         | {- 0x0D -} CPC_QNameA U30
         | {- 0x0E -} CPC_MultinameA U30
         | {- 0x0F -} CPC_RTQName U30
         | {- 0x10 -} CPC_RTQNameA U30
         | {- 0x11 -} CPC_RTQNameL U30
         | {- 0x12 -} CPC_RTQNameLA U30
         | {- 0x13 -} CPC_NameL U30
         | {- 0x14 -} CPC_NameLA U30
         | {- 0x15 -} CPC_NamespaceSet U30
         | {- 0x16 -} CPC_PackageNamespace U30
         | {- 0x17 -} CPC_PackageInternalNs U30
         | {- 0x18 -} CPC_ProtectedNamespace U30
         | {- 0x19 -} CPC_ExplicitNamespace U30
         | {- 0x1A -} CPC_StaticProtectedNs U30
         | {- 0x1B -} CPC_MultinameL U30
         | {- 0x1C -} CPC_MultinameLA U30
         deriving (Show)

{-
    4.6
    metadata
-}
data Metadata = Metadata {
                           metaName :: StringIdx
                         , kvps :: [(StringIdx, StringIdx)]
                         }
                         deriving (Show)

{-
    4.7
    instance
-}

data InstanceInfo = InstanceInfo {
                                   instName :: MultinameIdx
                                 , instSuperName :: MultinameIdx
                                 , instFlags :: Word8
                                 , instNs :: Maybe NSInfoIdx
                                 , instInterface :: [MultinameIdx]
                                 , instInit :: U30
                                 , instTraits :: [TraitsInfo]
                                 }
                                 deriving (Show)

instf_CLASS_SEALED :: Word8
instf_CLASS_SEALED = 0x01

instf_CLASS_FINAL :: Word8
instf_CLASS_FINAL = 0x02

instf_CLASS_INTERFACE :: Word8
instf_CLASS_INTERFACE = 0x04

instf_CLASS_PROTECTEDNS :: Word8
instf_CLASS_PROTECTEDNS = 0x08

instf_CLASS_NON_NULLABLE :: Word8
instf_CLASS_NON_NULLABLE = 0x10

{-
    4.8
    traits info
-}

data TraitsInfo = TraitsInfo {
                               tiName :: MultinameIdx
                             , tiFinal :: Bool
                             , tiOverride :: Bool
                             , tiType :: TraitType
                             , tiMeta :: Maybe [MetadataIdx]
                             }
                             deriving (Show)

{-
    4.8.1
    trait type
-}

trait_var :: Word8
trait_var = 0x00

trait_method :: Word8
trait_method = 0x01

trait_getter :: Word8
trait_getter = 0x02

trait_setter :: Word8
trait_setter = 0x03

trait_class :: Word8
trait_class = 0x04

trait_function :: Word8
trait_function = 0x05

trait_const :: Word8
trait_const = 0x06

data TraitType = {- 0 -} TT_Var TraitVar
               | {- 1 -} TT_Method TraitMethod
               | {- 2 -} TT_Getter TraitMethod
               | {- 3 -} TT_Setter TraitMethod
               | {- 4 -} TT_Class TraitClass
               | {- 5 -} TT_Function TraitFunction
               | {- 6 -} TT_Const TraitVar
               deriving (Show)

{-
    4.8.2
    trait slot
-}

data TraitVar = TraitVar {
                           tsId :: U30
                         , tsName :: MultinameIdx
                         , tsVindex :: U30
                         , tsVkind :: Maybe Word8
                         }
                         deriving (Show)

{-
    4.8.3
    trait class
-}

data TraitClass = TraitClass {
                               tcId :: U30
                             , tcInit :: ClassInfoIdx
                             }
                             deriving (Show)

{-
    4.8.4
    trait function
-}

data TraitFunction = TraitFunction {
                                     tfId :: U30
                                   , tfFunc :: MethodSignatureIdx
                                   }
                                   deriving (Show)

{-
    4.8.5
    trait method
-}

data TraitMethod = TraitMethod {
                                 tmDispId :: U30
                               , tmMethod :: MethodSignatureIdx
                               }
                               deriving (Show)

{-
    4.9
    class info
-}

data ClassInfo = ClassInfo {
                             ciInit :: MethodSignatureIdx
                           , ciTraits :: [TraitsInfo]
                           }
                           deriving (Show)

{-
    4.10
    script
-}

data ScriptInfo = ScriptInfo {
                               siInit :: MethodSignatureIdx
                             , siTraits :: [TraitsInfo]
                             }
                             deriving (Show)

{-
    4.11
    method body
-}

data MethodBody = MethodBody {
                               mbMethod :: MethodSignatureIdx
                             , mbMaxStack :: U30
                             , mbLocalCount :: U30
                             , mbInitScopeDepth :: U30
                             , mbMaxScopeDepth :: U30
                             , mbCode :: [OpCode]
                             , mbExceptions :: [Exception]
                             , mbTraits :: [TraitsInfo]
                             }
                             deriving (Show)

{-
    4.12
    exception
-}

data Exception = Exception {
                             exFrom :: U30
                           , exTo :: U30
                           , exTarget :: U30
                           , exType :: StringIdx
                           , exVarname :: StringIdx
                           }
                           deriving (Show)

data OpCode = {- 0x01 -} Breakpoint
            | {- 0x02 -} Nop
            | {- 0x03 -} Throw
            | {- 0x04 -} GetSuper MultinameIdx
            | {- 0x05 -} SetSuper MultinameIdx
            | {- 0x06 -} DefaultXmlNamespace U30
            | {- 0x07 -} DefaultXmlNamespaceL
            | {- 0x08 -} Kill U30
            | {- 0x09 -} Label
              {- 0x0A -}
              {- 0x0B -}
            | {- 0x0C -} IfNotLessThan S24
            | {- 0x0D -} IfNotLessEqual S24
            | {- 0x0E -} IfNotGreaterThan S24
            | {- 0x0F -} IfNotGreaterEqual S24
            | {- 0x10 -} Jump S24
            | {- 0x11 -} IfTrue S24
            | {- 0x12 -} IfFalse S24
            | {- 0x13 -} IfEqual S24
            | {- 0x14 -} IfNotEqual S24
            | {- 0x15 -} IfLessThan S24
            | {- 0x16 -} IfLessEqual S24
            | {- 0x17 -} IfGreaterThan S24
            | {- 0x18 -} IfGreaterEqual S24
            | {- 0x19 -} IfStrictEqual S24
            | {- 0x1A -} IfStrictNotEqual S24
            | {- 0x1B -} LookupSwitch S24 [S24] {- default offset, case offsets -}
            | {- 0x1C -} PushWith
            | {- 0x1D -} PopScope
            | {- 0x1E -} NextName
            | {- 0x1F -} HasNext
            | {- 0x20 -} PushNull
            | {- 0x21 -} PushUndefined
            | {- 0x22 -} PushConstant
            | {- 0x23 -} NextValue
            | {- 0x24 -} PushByte U8
            | {- 0x25 -} PushShort U30
            | {- 0x26 -} PushTrue
            | {- 0x27 -} PushFalse
            | {- 0x28 -} PushNaN
            | {- 0x29 -} Pop
            | {- 0x2A -} Dup
            | {- 0x2B -} Swap
            | {- 0x2C -} PushString StringIdx
            | {- 0x2D -} PushInt IntIdx
            | {- 0x2E -} PushUInt UintIdx
            | {- 0x2F -} PushDouble DoubleIdx
            | {- 0x30 -} PushScope
            | {- 0x31 -} PushNamespace NSInfoIdx
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
            | {- 0x40 -} NewFunction U30
            | {- 0x41 -} Call U30
            | {- 0x42 -} Construct U30
            | {- 0x43 -} CallMethod U30 U30
            | {- 0x44 -} CallStatic U30 U30
            | {- 0x45 -} CallSuper U30 U30
            | {- 0x46 -} CallProperty U30 U30
            | {- 0x47 -} ReturnVoid
            | {- 0x48 -} ReturnValue
            | {- 0x49 -} ConstructSuper U30
            | {- 0x4A -} ConstructProp U30 U30
            | {- 0x4B -} CallSuperId    {-NOT HANDLED-}
            | {- 0x4C -} CallPropLex U30 U30
            | {- 0x4D -} CallInterface  {-NOT HANDLED-}
            | {- 0x4E -} CallSuperVoid U30 U30
            | {- 0x4F -} CallPropVoid U30 U30
              {- 0x50 -} {-Sign1-}  {- Alchemy -}
              {- 0x51 -} {-Sign8-}  {- Alchemy -}
              {- 0x52 -} {-Sign16-} {- Alchemy -}
            | {- 0x53 -} ApplyType
            | {- 0x55 -} NewObject U30
            | {- 0x56 -} NewArray U30
            | {- 0x57 -} NewActivation
            | {- 0x58 -} NewClass ClassInfoIdx
            | {- 0x59 -} GetDescendants MultinameIdx
            | {- 0x5A -} NewCatch U30
            | {- 0x5B -} FindPropGlobalStrict   {-NEW from Tamarin (internal)-}
            | {- 0x5C -} FindPropGlobal         {-NEW from Tamarin (internal)-}
            | {- 0x5D -} FindPropStrict MultinameIdx
            | {- 0x5E -} FindProperty MultinameIdx
            | {- 0x5F -} FindDef        {-NOT HANDLED-}
            | {- 0x60 -} GetLex U30
            | {- 0x61 -} SetProperty MultinameIdx
            | {- 0x62 -} GetLocal U30
            | {- 0x63 -} SetLocal U30
            | {- 0x64 -} GetGlobalScope
            | {- 0x65 -} GetScopeObject U8
            | {- 0x66 -} GetProperty MultinameIdx
            | {- 0x67 -} GetPropertyLate
            | {- 0x68 -} InitProperty MultinameIdx
            | {- 0x69 -} SetPropertyLate
            | {- 0x6A -} DeleteProperty U30
            | {- 0x6B -} DeletePropertyLate
            | {- 0x6C -} GetSlot U30
            | {- 0x6D -} SetSlot U30
            | {- 0x6E -} GetGlobalSlot U30
            | {- 0x6F -} SetGlobalSlot U30
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
            | {- 0x80 -} Coerce MultinameIdx
            | {- 0x81 -} CoerceBoolean
            | {- 0x82 -} CoerceAny
            | {- 0x83 -} CoerceInt
            | {- 0x84 -} CoerceDouble
            | {- 0x85 -} CoerceString
            | {- 0x86 -} AsType U30
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
            | {- 0x94 -} DecLocal U30
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
            | {- 0xB2 -} IsType MultinameIdx
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
            | {- 0xC2 -} IncLocalInt U30
            | {- 0xC3 -} DecLocalInt U30
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
            | {- 0xEF -} Debug U8 StringIdx U8 U30
            | {- 0xF0 -} DebugLine U30
            | {- 0xF1 -} DebugFile StringIdx
            | {- 0xF2 -} BreakpointLine
              {- 0xF3 -} {-timestamp-}
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
toBytes {- 0x33 -} (PushDecimal    {-NEW: PushDecimal according to FlexSDK, lix8 according to Tamarin-}) = 1
toBytes {- 0x34 -} (PushDNaN       {-NEW: PushDNaN according to Flex SDK, lix16 according to Tamarin-}) = 1
--toBytes   {- 0x35 -} {-GetByte-}   {- Alchemy -}    
--toBytes   {- 0x36 -} {-GetShort-}  {- Alchemy -}
--toBytes   {- 0x37 -} {-GetInt-}    {- Alchemy -}
--toBytes   {- 0x38 -} {-GetFloat-}  {- Alchemy -}
--toBytes   {- 0x39 -} {-GetDouble-} {- Alchemy -}
--toBytes   {- 0x3A -} {-SetByte-}   {- Alchemy -}
--toBytes   {- 0x3B -} {-SetShort-}  {- Alchemy -}
--toBytes   {- 0x3C -} {-SetInt-}    {- Alchemy -}
--toBytes   {- 0x3D -} {-SetFloat-}  {- Alchemy -}
--toBytes   {- 0x3E -} {-SetDouble-} {- Alchemy -}
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
toBytes {- 0x4B -} (CallSuperId    {-NOT HANDLED-}) = 1
toBytes {- 0x4C -} (CallPropLex u30_1 u30_2) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x4D -} (CallInterface  {-NOT HANDLED-}) = 1
toBytes {- 0x4E -} (CallSuperVoid u30_1 u30_2) = 1 + u30Bytes u30_1 + u30Bytes u30_2
toBytes {- 0x4F -} (CallPropVoid u30_1 u30_2) = 1 + u30Bytes u30_1 + u30Bytes u30_2
--toBytes   {- 0x50 -} {-Sign1-}  {- Alchemy -}
--toBytes   {- 0x51 -} {-Sign8-}  {- Alchemy -}
--toBytes   {- 0x52 -} {-Sign16-} {- Alchemy -}
toBytes {- 0x53 -} (ApplyType) = 1
toBytes {- 0x55 -} (NewObject u30) = 1 + u30Bytes u30
toBytes {- 0x56 -} (NewArray u30) = 1 + u30Bytes u30
toBytes {- 0x57 -} (NewActivation) = 1
toBytes {- 0x58 -} (NewClass u30) = 1 + u30Bytes u30
toBytes {- 0x59 -} (GetDescendants u30) = 1 + u30Bytes u30
toBytes {- 0x5A -} (NewCatch u30) = 1 + u30Bytes u30
toBytes {- 0x5B -} (FindPropGlobalStrict   {-NEW from Tamarin (internal)-}) = 1
toBytes {- 0x5C -} (FindPropGlobal         {-NEW from Tamarin (internal)-}) = 1
toBytes {- 0x5D -} (FindPropStrict u30) = 1 + u30Bytes u30
toBytes {- 0x5E -} (FindProperty u30) = 1 + u30Bytes u30
toBytes {- 0x5F -} (FindDef        {-NOT HANDLED-}) = 1
toBytes {- 0x60 -} (GetLex u30) = 1 + u30Bytes u30
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
