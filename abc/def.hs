module ABC.Def where

import Control.Monad.State (StateT)
import Data.Int
import Data.Word
import qualified Data.ByteString.Lazy as DBL
import qualified Data.HashTable.IO as H

type Parser a = StateT DBL.ByteString IO a
--type Parser a = State DBL.ByteString a

type ByteString = DBL.ByteString
type HashTable k v = H.BasicHashTable k v

{-
    4.2
    Abc
-}
data Abc = Abc {
                 abcInts :: [Int32]
               , abcUints :: [Word32]
               , abcDoubles :: [Double]
               , abcStrings :: [String]
               , abcNsInfo :: [NSInfo]
               , abcNsSet :: [NSSet]
               , abcMultinames :: [Multiname]
               , abcMethodSigs :: [MethodSignature]
               , abcMetadata :: [Metadata]
               , abcInstances :: [InstanceInfo]
               , abcClasses :: [ClassInfo]
               --, abcClasses :: [Int]
               --, abcScripts :: [Int]
               --, abcMethodBodies :: [Int]
               }
               deriving (Show)

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

abcUintsIdx :: (Integral a) => Abc -> a -> Word32
abcUintsIdx cp w = abcUints cp !! fromIntegral w

abcUintsF :: Abc -> ([Word32] -> [Word32]) -> Abc
abcUintsF cp f = abcUintsR (f $ abcUints cp) cp

abcUintsR :: [Word32] -> Abc -> Abc
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

{-
    4.4
    String
-}
type StringIdx = Word32

{-
    4.4.1
    Namespace
-}
type NSInfoIdx = Word32
data NSInfo = {- 0x08 -} NSInfo_Namespace Word32
            | {- 0x16 -} NSInfo_PackageNamespace Word32
            | {- 0x17 -} NSInfo_PackageInternalNs Word32
            | {- 0x18 -} NSInfo_ProtectedNamespace Word32
            | {- 0x19 -} NSInfo_ExplicitNamespace Word32
            | {- 0x1A -} NSInfo_StaticProtectedNs Word32
            | {- 0x05 -} NSInfo_PrivateNs Word32
            |            NSInfo_Any {- '*' see 4.4.4 QName -}
            deriving (Show)

{-
    4.4.2
    Namespace set
-}
type NSSetIdx = Word32
type NSSet = [Word32]

{-
    4.4.3
    Multiname
-}
type MultinameIdx = Word32
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
                                         returnType :: MultinameIdx
                                       , paramTypes :: [Word32]
                                       , methodName :: StringIdx
                                       , flags :: Word8
                                       , optionInfo :: Maybe [CPC]
                                       , paramNames :: Maybe [Word32]
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

-- 4.5.1
data CPC = {- 0x00 -} CPC_Undefined
         | {- 0x01 -} CPC_Utf8 Word32
         | {- 0x03 -} CPC_Int Word32
         | {- 0x04 -} CPC_Uint Word32
         | {- 0x05 -} CPC_PrivateNamespace Word32
         | {- 0x06 -} CPC_Double Word32
         | {- 0x07 -} CPC_QName Word32
         | {- 0x08 -} CPC_Namespace Word32
         | {- 0x09 -} CPC_Multiname Word32
         | {- 0x0A -} CPC_False
         | {- 0x0B -} CPC_True
         | {- 0x0C -} CPC_Null
         | {- 0x0D -} CPC_QNameA Word32
         | {- 0x0E -} CPC_MultinameA Word32
         | {- 0x0F -} CPC_RTQName Word32
         | {- 0x10 -} CPC_RTQNameA Word32
         | {- 0x11 -} CPC_RTQNameL Word32
         | {- 0x12 -} CPC_RTQNameLA Word32
         | {- 0x13 -} CPC_NameL Word32
         | {- 0x14 -} CPC_NameLA Word32
         | {- 0x15 -} CPC_NamespaceSet Word32
         | {- 0x16 -} CPC_PackageNamespace Word32
         | {- 0x17 -} CPC_PackageInternalNs Word32
         | {- 0x18 -} CPC_ProtectedNamespace Word32
         | {- 0x19 -} CPC_ExplicitNamespace Word32
         | {- 0x1A -} CPC_StaticProtectedNs Word32
         | {- 0x1B -} CPC_MultinameL Word32
         | {- 0x1C -} CPC_MultinameLA Word32
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
                                   instName :: StringIdx
                                 , instSuperName :: StringIdx
                                 , instFlags :: Word8
                                 , instNs :: Maybe NSInfoIdx
                                 , instInterface :: [MultinameIdx]
                                 , instInit :: Word32
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

{-const SLOT_var                    = 0;
const SLOT_method                 = 1;
const SLOT_getter                 = 2;
const SLOT_setter                 = 3;
const SLOT_class                  = 4;
const SLOT_function               = 6;-}

{-
    4.8
    traits info
-}

data TraitsInfo = TraitsInfo {
                               tiName :: MultinameIdx
                             , tiAttributes :: Word8
                             , tiType :: TraitType
                             , tiMeta :: Maybe Metadata
                             }
                             deriving (Show)

{-
    4.8.1
    trait type
-}

data TraitType = {- 0 -} TT_Slot TraitSlot
               | {- 1 -} TT_Method TraitMethod
               | {- 2 -} TT_Getter TraitMethod
               | {- 3 -} TT_Setter TraitMethod
               | {- 4 -} TT_Class TraitClass
               | {- 5 -} TT_Function TraitFunction
               | {- 6 -} TT_Const TraitSlot
               deriving (Show)

{-
    4.8.2
    trait slot
-}

data TraitSlot = TraitSlot {
                             tsSlotId :: Word32
                           , tsName :: MultinameIdx
                           , tsIndex :: Word32
                           , tsKind :: Word32
                           }
                           deriving (Show)

{-
    4.8.3
    trait class
-}

data TraitClass = TraitClass {
                               tcId :: Word32
                             , tcInit :: Word32
                             }
                             deriving (Show)

{-
    4.8.4
    trait function
-}

data TraitFunction = TraitFunction {
                                     tfId :: Word32
                                   , tfFunc :: Word32
                                   }
                                   deriving (Show)

{-
    4.8.5
    trait method
-}

data TraitMethod = TraitMethod {
                                 tmDispId :: Word32
                               , tmMethod :: Word32
                               }
                               deriving (Show)

{-
    4.8.5
    trait method
-}

data ClassInfo = ClassInfo {
                             ciInit :: Word32
                           , ciTraits :: [TraitsInfo]
                           }
                           deriving (Show)




















