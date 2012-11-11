module ABC.Def where

{-# LANGUAGE BangPatterns #-}

import Control.Monad.State (StateT)
import Control.DeepSeq
import Data.Int
import Data.Word
import qualified Data.ByteString.Lazy as DBL

type Parser a = StateT DBL.ByteString IO a
--type Parser a = State DBL.ByteString a

type ByteString = DBL.ByteString

type U8 = Word8
type U30 = Word32
type U32 = Word32
type S24 = Int32

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

{-

Multinames and NSSet have a default I just don't know what it is
see abc-decode.es

-}
{-defaultAbc :: Abc
defaultAbc = Abc [0] [0] [0] [""] [NSInfo_Any] [] [] []

abcIntsIdx :: (Integral a) => Abc -> a -> Int32
abcIntsIdx cp w = abcInts cp !! fromIntegral w

abcIntsF :: Abc -> ([Int32] -> [Int32]) -> Abc
abcIntsF cp f = abcIntsR (f $ abcInts cp) cp

abcIntsR :: [Int32] -> Abc -> Abc
abcIntsR value cp = Abc
    value
    (abcUints cp)
    (abcDoubles cp)
    (abcStrings cp)
    (abcNsInfo cp)
    (abcNsSet cp)
    (abcMultinames cp)
    (abcMethodSigs cp)

abcUintsIdx :: (Integral a) => Abc -> a -> U30
abcUintsIdx cp w = abcUints cp !! fromIntegral w

abcUintsF :: Abc -> ([U30] -> [U30]) -> Abc
abcUintsF cp f = abcUintsR (f $ abcUints cp) cp

abcUintsR :: [U30] -> Abc -> Abc
abcUintsR value cp = Abc
    (abcInts cp)
    value
    (abcDoubles cp)
    (abcStrings cp)
    (abcNsInfo cp)
    (abcNsSet cp)
    (abcMultinames cp)
    (abcMethodSigs cp)

abcDoublesIdx :: (Integral a) => Abc -> a -> Double
abcDoublesIdx cp w = abcDoubles cp !! fromIntegral w

abcDoublesF :: Abc -> ([Double] -> [Double]) -> Abc
abcDoublesF cp f = abcDoublesR (f $ abcDoubles cp) cp

abcDoublesR :: [Double] -> Abc -> Abc
abcDoublesR value cp = Abc
    (abcInts cp)
    (abcUints cp)
    value
    (abcStrings cp)
    (abcNsInfo cp)
    (abcNsSet cp)
    (abcMultinames cp)
    (abcMethodSigs cp)

abcStringsIdx :: (Integral a) => Abc -> a -> String
abcStringsIdx cp w = abcStrings cp !! fromIntegral w

abcStringsF :: Abc -> ([String] -> [String]) -> Abc
abcStringsF cp f = abcStringsR (f $ abcStrings cp) cp

abcStringsR :: [String] -> Abc -> Abc
abcStringsR value cp = Abc
    (abcInts cp)
    (abcUints cp)
    (abcDoubles cp)
    value
    (abcNsInfo cp)
    (abcNsSet cp)
    (abcMultinames cp)
    (abcMethodSigs cp)

abcNsInfoIdx :: (Integral a) => Abc -> a -> NSInfo
abcNsInfoIdx cp w = abcNsInfo cp !! fromIntegral w

abcNsInfoF :: Abc -> ([NSInfo] -> [NSInfo]) -> Abc
abcNsInfoF cp f = abcNsInfoR (f $ abcNsInfo cp) cp

abcNsInfoR :: [NSInfo] -> Abc -> Abc
abcNsInfoR value cp = Abc
    (abcInts cp)
    (abcUints cp)
    (abcDoubles cp)
    (abcStrings cp)
    value
    (abcNsSet cp)
    (abcMultinames cp)
    (abcMethodSigs cp)

abcNsSetIdx :: (Integral a) => Abc -> a -> NSSet
abcNsSetIdx cp w = abcNsSet cp !! fromIntegral w

abcNsSetF :: Abc -> ([NSSet] -> [NSSet]) -> Abc
abcNsSetF cp f = abcNsSetR (f $ abcNsSet cp) cp

abcNsSetR :: [NSSet] -> Abc -> Abc
abcNsSetR value cp = Abc
    (abcInts cp)
    (abcUints cp)
    (abcDoubles cp)
    (abcStrings cp)
    (abcNsInfo cp)
    value
    (abcMultinames cp)
    (abcMethodSigs cp)

abcMultinameIdx :: (Integral a) => Abc -> a -> Multiname
abcMultinameIdx cp w = abcMultinames cp !! fromIntegral w

abcMultinameF :: Abc -> ([Multiname] -> [Multiname]) -> Abc
abcMultinameF cp f = abcMultinameR (f $ abcMultinames cp) cp

abcMultinameR :: [Multiname] -> Abc -> Abc
abcMultinameR value cp = Abc
    (abcInts cp)
    (abcUints cp)
    (abcDoubles cp)
    (abcStrings cp)
    (abcNsInfo cp)
    (abcNsSet cp)
    value
    (abcMethodSigs cp)

abcMethodSigsIdx :: (Integral a) => Abc -> a -> MethodSignature
abcMethodSigsIdx cp w = abcMethodSigs cp !! fromIntegral w

abcMethodSigsF :: Abc -> ([MethodSignature] -> [MethodSignature]) -> Abc
abcMethodSigsF cp f = abcMethodSigsR (f $ abcMethodSigs cp) cp

abcMethodSigsR :: [MethodSignature] -> Abc -> Abc
abcMethodSigsR value cp = Abc
    (abcInts cp)
    (abcUints cp)
    (abcDoubles cp)
    (abcStrings cp)
    (abcNsInfo cp)
    (abcNsSet cp)
    (abcMultinames cp)
    value-}

type DoubleIdx = U30
type IntIdx = U30
type UintIdx = U30

{-
    4.4
    String
-}
type StringIdx = U30

{-
    4.4.1
    Namespace
-}
type NSInfoIdx = U30
data NSInfo = {- 0x08 -} NSInfo_Namespace StringIdx
            | {- 0x16 -} NSInfo_PackageNamespace StringIdx
            | {- 0x17 -} NSInfo_PackageInternalNs StringIdx
            | {- 0x18 -} NSInfo_ProtectedNamespace StringIdx
            | {- 0x19 -} NSInfo_ExplicitNamespace StringIdx
            | {- 0x1A -} NSInfo_StaticProtectedNs StringIdx
            | {- 0x05 -} NSInfo_PrivateNs StringIdx
            |            NSInfo_Any {- '*' see 4.4.4 QName -}
            deriving (Show)

instance NFData NSInfo where
    rnf (NSInfo_Namespace a) = a `deepseq` ()
    rnf (NSInfo_PackageNamespace a) = a `deepseq` ()
    rnf (NSInfo_PackageInternalNs a) = a `deepseq` ()
    rnf (NSInfo_ProtectedNamespace a) = a `deepseq` ()
    rnf (NSInfo_ExplicitNamespace a) = a `deepseq` ()
    rnf (NSInfo_StaticProtectedNs a) = a `deepseq` ()
    rnf (NSInfo_PrivateNs a) = a `deepseq` ()

{-
    4.4.2
    Namespace set
-}
type NSSetIdx = U30
type NSSet = [U30]

{-
    4.4.3
    Multiname
-}
type MultinameIdx = U30
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

instance NFData Multiname where
    rnf (Multiname_QName a b) = a `deepseq` b `deepseq` ()
    rnf (Multiname_QNameA a b) = a `deepseq` b `deepseq` ()
    rnf (Multiname_RTQName a) = a `deepseq` ()
    rnf (Multiname_RTQNameA a) = a `deepseq` ()
    rnf (Multiname_Multiname a b) = a `deepseq` b `deepseq` ()
    rnf (Multiname_MultinameA a b) = a `deepseq` b `deepseq` ()
    rnf (Multiname_MultinameL a) = a `deepseq` ()
    rnf (Multiname_MultinameLA a) = a `deepseq` ()

{-
    4.5
    Method signature
-}
type MethodSignatureIdx = U30
data MethodSignature = MethodSignature {
                                         returnType :: MultinameIdx
                                       , paramTypes :: [U30]
                                       , methodName :: StringIdx -- debug
                                       , flags :: Word8
                                       , optionInfo :: Maybe [CPC]
                                       , paramNames :: Maybe [U30] -- debug
                                       }
                                       deriving (Show)

instance NFData MethodSignature where
    rnf (MethodSignature a b c d e f) = a
        `deepseq` b
        `deepseq` c
        `deepseq` d
        `deepseq` e
        `deepseq` f
        `deepseq` ()

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


{-
    4.6
    metadata
-}
type MetadataIdx = U30
data Metadata = Metadata {
                           metaName :: StringIdx
                         , kvps :: [(StringIdx, StringIdx)]
                         }
                         deriving (Show)

instance NFData Metadata where
    rnf (Metadata a b) = a `deepseq` b `deepseq` ()

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

instance NFData InstanceInfo where
    rnf (InstanceInfo a b c d e f g) = a
        `deepseq` b
        `deepseq` c
        `deepseq` d
        `deepseq` e
        `deepseq` f
        `deepseq` g
        `deepseq` ()

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
                             , tiAttributes :: Word8
                             , tiType :: TraitType
                             , tiMeta :: Maybe [MetadataIdx]
                             }
                             deriving (Show)

instance NFData TraitsInfo where
    rnf (TraitsInfo a b c d) = a
        `deepseq` b
        `deepseq` c
        `deepseq` d
        `deepseq` ()

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

instance NFData TraitType where
    rnf (TT_Var a) = a `deepseq` ()
    rnf (TT_Method a) = a `deepseq` ()
    rnf (TT_Getter a) = a `deepseq` ()
    rnf (TT_Setter a) = a `deepseq` ()
    rnf (TT_Class a) = a `deepseq` ()
    rnf (TT_Function a) = a `deepseq` ()
    rnf (TT_Const a) = a `deepseq` ()

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

instance NFData TraitVar where
    rnf (TraitVar a b c d) = a
        `deepseq` b
        `deepseq` c
        `deepseq` d
        `deepseq` ()

{-
    4.8.3
    trait class
-}

data TraitClass = TraitClass {
                               tcId :: U30
                             , tcInit :: U30
                             }
                             deriving (Show)

instance NFData TraitClass where
    rnf (TraitClass a b) = a `deepseq` b `deepseq` ()

{-
    4.8.4
    trait function
-}

data TraitFunction = TraitFunction {
                                     tfId :: U30
                                   , tfFunc :: U30
                                   }
                                   deriving (Show)

instance NFData TraitFunction where
    rnf (TraitFunction a b) = a `deepseq` b `deepseq` ()

{-
    4.8.5
    trait method
-}

data TraitMethod = TraitMethod {
                                 tmDispId :: U30
                               , tmMethod :: U30
                               }
                               deriving (Show)

instance NFData TraitMethod where
    rnf (TraitMethod a b) = a `deepseq` b `deepseq` ()

{-
    4.9
    class info
-}

data ClassInfo = ClassInfo {
                             ciInit :: U30
                           , ciTraits :: [TraitsInfo]
                           }
                           deriving (Show)

instance NFData ClassInfo where
    rnf (ClassInfo a b) = a `deepseq` b `deepseq` ()

{-
    4.10
    script
-}

data ScriptInfo = ScriptInfo {
                               siInit :: U30
                             , siTraits :: [TraitsInfo]
                             }
                             deriving (Show)

instance NFData ScriptInfo where
    rnf (ScriptInfo a b) = a `deepseq` b `deepseq` ()

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

instance NFData Exception where
    rnf (Exception a b c d e) = a
        `deepseq` b
        `deepseq` c
        `deepseq` d
        `deepseq` e
        `deepseq` ()

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
            | {- 0x58 -} NewClass U30
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
            | {- 0x68 -} InitProperty U30
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
            | {- 0x91 -} Increment U30
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
{- 0x91 -} rnf (Increment a) = a `deepseq` ()
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


