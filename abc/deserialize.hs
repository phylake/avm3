module ABC.Deserialize where

{-# LANGUAGE BangPatterns #-}

import Control.Concurrent.STM
import Data.Bits
import Data.Int (Int32)
import Data.Word
import System (getArgs)
import Util
import TFish
import SWF.Deserialize as SWF hiding (testFile)
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Lazy.Char8 as DBLC

type ByteString = DBL.ByteString

type OptionDetail = (Int, Int)
type OptionInfo = [OptionDetail]

testFile = do
    bs <- DBL.readFile "Test.abc"
    let (minor, bs') = fromU16LE bs
    let (major, bs'') = fromU16LE bs'
    putStrLn $ "major: " ++ show major
    putStrLn $ "minor: " ++ show minor
    let t = parseAbc (defaultAbc, bs'')
    putStrLn $ show t

parseCommon :: Bool
            -> ((a, ByteString) -> (a, ByteString))
            -> (a, ByteString)
            -> (a, ByteString)
parseCommon hasOne f (abc, bs) =
    let (u30, bs') = fromU30LE_vl bs in
    if hasOne
                              {- make sure 0 and 1 == 0 -}
        then let u30' = fromIntegral $ (u30 <||> 1) - 1 in
            forN' f (abc, bs') u30'
        else let u30' = fromIntegral u30 in
            forN' f (abc, bs') u30'


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
               --, abcInstances :: [Int]
               --, abcClasses :: [Int]
               --, abcScripts :: [Int]
               --, abcMethodBodies :: [Int]
               }
               deriving (Show)

{-

Multinames and NSSet have a default I just don't know what it is
see abc-decode.es

-}
defaultAbc :: Abc
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
    value

parseAbc :: (Abc, ByteString) -> (Abc, ByteString)
parseAbc = parseCommon False parseAbcMethodSignature .
           parseCommon True parseAbcMultiname .
           parseCommon True parseAbcNsSet .
           parseCommon True parseAbcNsInfo .
           parseCommon True parseAbcString .
           parseCommon True parseAbcDouble .
           parseCommon True parseAbcUint .
           parseCommon True parseAbcInt

parseAbcInt :: (Abc, ByteString) -> (Abc, ByteString)
parseAbcInt (abc, bs) =
    let (i32, bs') = fromS32LE_vl bs in
    (abcIntsF abc (++[i32]), bs')

parseAbcUint :: (Abc, ByteString) -> (Abc, ByteString)
parseAbcUint (abc, bs) =
    let (u32, bs') = fromU32LE_vl bs in
    (abcUintsF abc (++[u32]), bs')

parseAbcDouble :: (Abc, ByteString) -> (Abc, ByteString)
parseAbcDouble (abc, bs) =
    let (double, bs') = fromDoubleLE bs in
    (abcDoublesF abc (++[double]), bs')

parseAbcString :: (Abc, ByteString) -> (Abc, ByteString)
parseAbcString (abc, bs) =
    let (str, bs') = parseStringInfo bs in
    (abcStringsF abc (++[str]), bs')

parseAbcNsInfo :: (Abc, ByteString) -> (Abc, ByteString)
parseAbcNsInfo (abc, bs) =
    let (ns, bs') = parseNSInfo abc bs in
    (abcNsInfoF abc (++[ns]), bs')

parseAbcNsSet :: (Abc, ByteString) -> (Abc, ByteString)
parseAbcNsSet (abc, bs) =
    let (ns, bs') = parseNSSet abc bs in
    (abcNsSetF abc (++[ns]), bs')

parseAbcMultiname :: (Abc, ByteString) -> (Abc, ByteString)
parseAbcMultiname (abc, bs) =
    let (multinames, bs') = parseMultiname abc bs in
    (abcMultinameF abc (++[multinames]), bs')

parseAbcMethodSignature :: (Abc, ByteString) -> (Abc, ByteString)
parseAbcMethodSignature (abc, bs) =
    let (ms, bs') = parseMethodSignature abc bs in
    (abcMethodSigsF abc (++[ms]), bs')

{-
    4.4
    String
-}
parseStringInfo :: DBL.ByteString -> (String, DBL.ByteString)
parseStringInfo bs =
    let (u30, bs') = fromU30LE_vl bs in
    let (half1, half2) = DBL.splitAt (fromIntegral u30) bs' in
    (DBLC.unpack half1, half2)

{-
    4.4.1
    Namespace
-}
data NSInfo = {- 0x08 -} NSInfo_Namespace String
            | {- 0x16 -} NSInfo_PackageNamespace String
            | {- 0x17 -} NSInfo_PackageInternalNs String
            | {- 0x18 -} NSInfo_ProtectedNamespace String
            | {- 0x19 -} NSInfo_ExplicitNamespace String
            | {- 0x1A -} NSInfo_StaticProtectedNs String
            | {- 0x05 -} NSInfo_PrivateNs String
            |            NSInfo_Any {- '*' see 4.4.4 QName -}
            deriving (Show)

parseNSInfo :: Abc -> DBL.ByteString -> (NSInfo, DBL.ByteString)
parseNSInfo abc bs =
    let ((w:[]), bs') = nWords 1 bs in
    let (idx, bs'') = fromU30LE_vl bs' in
    let ns = parseNSInfoImpl w $ abcStringsIdx abc idx in
    (ns, bs'')
    where
        parseNSInfoImpl w str
            | w == 0x08 = NSInfo_Namespace str
            | w == 0x16 = NSInfo_PackageNamespace str
            | w == 0x17 = NSInfo_PackageInternalNs str
            | w == 0x18 = NSInfo_ProtectedNamespace str
            | w == 0x19 = NSInfo_ExplicitNamespace str
            | w == 0x1A = NSInfo_StaticProtectedNs str
            | w == 0x05 = NSInfo_PrivateNs str

{-
    4.4.2
    Namespace set
-}
type NSSet = [NSInfo]

parseNSSet :: Abc -> DBL.ByteString -> (NSSet, DBL.ByteString)
parseNSSet abc bs =
    let (count, bs') = fromU30LE_vl bs in
    let (_, set, bs'') = forN' parseNSSetImpl (abc, [], bs') count in
    (set, bs'')

parseNSSetImpl :: (Abc, NSSet, DBL.ByteString)
               -> (Abc, NSSet, DBL.ByteString)
parseNSSetImpl (abc, ns, bs) =
    let (idx, bs') = fromU30LE_vl bs in
    let nsInfo = abcNsInfoIdx abc idx in
    (abc, ns ++ [nsInfo], bs')

{-
    4.4.3
    Multiname
-}
data Multiname = {- 0x07 -} Multiname_QName NSInfo String
               | {- 0x0D -} Multiname_QNameA NSInfo String
               | {- 0x0F -} Multiname_RTQName String
               | {- 0x10 -} Multiname_RTQNameA String
               | {- 0x11 -} Multiname_RTQNameL
               | {- 0x12 -} Multiname_RTQNameLA
               | {- 0x09 -} Multiname_Multiname String NSSet
               | {- 0x0E -} Multiname_MultinameA String NSSet
               | {- 0x1B -} Multiname_MultinameL NSSet
               | {- 0x1C -} Multiname_MultinameLA NSSet
               |            Multiname_Any
               deriving (Show)

parseMultiname :: Abc -> DBL.ByteString -> (Multiname, DBL.ByteString)
parseMultiname abc bs =
    let ((w:[]), bs') = nWords 1 bs in
    parseMultinameImpl w abc bs'

parseMultinameImpl :: Word8
                   -> (Abc
                    -> DBL.ByteString
                    -> (Multiname, DBL.ByteString))
parseMultinameImpl w
    | w == 0x07 = parseMultinameQ Multiname_QName
    | w == 0x0D = parseMultinameQ Multiname_QNameA
    | w == 0x0F = parseMultinameRTQ Multiname_RTQName
    | w == 0x10 = parseMultinameRTQ Multiname_RTQNameA
    | w == 0x11 = \_ bs -> (Multiname_RTQNameL, bs)
    | w == 0x12 = \_ bs -> (Multiname_RTQNameLA, bs)
    | w == 0x09 = parseMultinameM Multiname_Multiname
    | w == 0x0E = parseMultinameM Multiname_MultinameA
    | w == 0x1B = parseMultinameML Multiname_MultinameL
    | w == 0x1C = parseMultinameML Multiname_MultinameLA

parseMultinameQ :: (NSInfo -> String -> Multiname)
                -> Abc
                -> DBL.ByteString
                -> (Multiname, DBL.ByteString)
parseMultinameQ f abc bs =
    let (nsIdx, bs') = fromU30LE_vl bs in
    let (nameIdx, bs'') = fromU30LE_vl bs' in
    let ns = abcNsInfoIdx abc nsIdx in
    let name = abcStringsIdx abc nameIdx in
    (f ns name, bs'')

parseMultinameRTQ :: (String -> Multiname)
                  -> Abc
                  -> DBL.ByteString
                  -> (Multiname, DBL.ByteString)
parseMultinameRTQ f abc bs =
    let (nameIdx, bs') = fromU30LE_vl bs in
    let name = abcStringsIdx abc nameIdx in
    (f name, bs')

parseMultinameM :: (String -> NSSet -> Multiname)
                -> Abc
                -> DBL.ByteString
                -> (Multiname, DBL.ByteString)
parseMultinameM f abc bs =
    let (nameIdx, bs') = fromU30LE_vl bs in
    let (setIdx, bs'') = fromU30LE_vl bs' in
    let name = abcStringsIdx abc nameIdx in
    let set = abcNsSetIdx abc setIdx in
    (f name set, bs'')

parseMultinameML :: (NSSet -> Multiname)
                 -> Abc
                 -> DBL.ByteString
                 -> (Multiname, DBL.ByteString)
parseMultinameML f abc bs =
    let (setIdx, bs') = fromU30LE_vl bs in
    let set = abcNsSetIdx abc setIdx in
    (f set, bs')

{-
    4.5
    Method signature
-}
data MethodSignature = MethodSignature {
                                         returnType :: Multiname
                                       , paramType :: [Int]
                                       , name :: Int
                                       , flags :: Word8
                                       , optionInfo :: Maybe OptionInfo
                                       , paramInfo :: Maybe [String]
                                       }
                                       deriving (Show)

parseMethodSignature :: Abc -> DBL.ByteString -> (MethodSignature, DBL.ByteString)
parseMethodSignature abc bs0 =
    let (paramCount, bs1) = fromU30LE_vl bs0 in
    let (returnType, bs2) = fromU30LE_vl bs1 in
    let (pTypes, bs3) = forN' paramCountF ([], bs2) $ fromIntegral paramCount in
    (MethodSignature Multiname_Any [] 0 0 Nothing Nothing, bs0)
    where
        paramCountF :: ([Word32], DBL.ByteString) -> ([Word32], DBL.ByteString)
        paramCountF (ws, bs) =
            let (u30, bs') = fromU30LE_vl bs in (ws ++ [u30], bs')
{-
    4.5.1
    Optional parameters
-}

-- 4.5.1
data CPC = {- 0x01 -} CPC_Utf8
         | {- 0x03 -} CPC_Integer
         | {- 0x04 -} CPC_UInt
         | {- 0x05 -} CPC_PrivateNamespace
         | {- 0x06 -} CPC_Double
         | {- 0x07 -} CPC_QName
         | {- 0x08 -} CPC_Namespace
         | {- 0x09 -} CPC_Multiname
         | {- 0x0A -} CPC_False
         | {- 0x0B -} CPC_True
         | {- 0x0C -} CPC_Null
         | {- 0x0D -} CPC_QNameA
         | {- 0x0E -} CPC_MultinameA
         | {- 0x0F -} CPC_RTQName
         | {- 0x10 -} CPC_RTQNameA
         | {- 0x11 -} CPC_RTQNameL
         | {- 0x12 -} CPC_RTQNameLA
         | {- 0x13 -} CPC_NameL
         | {- 0x14 -} CPC_NameLA
         | {- 0x15 -} CPC_NamespaceSet
         | {- 0x16 -} CPC_PackageNamespace
         | {- 0x17 -} CPC_PackageInternalNS
         | {- 0x18 -} CPC_ProtectedNamespace
         | {- 0x19 -} CPC_ExplicitNamespace
         | {- 0x1A -} CPC_StaticProtectedNS
         | {- 0x1B -} CPC_MultinameL
         | {- 0x1C -} CPC_MultinameLA

data InstanceFlags = {- 0x01 -} IF_ClassSealed
                   | {- 0x02 -} IF_ClassFinal
                   | {- 0x04 -} IF_ClassInterface
                   | {- 0x08 -} IF_ClassProtectedNs



{-const SLOT_var                    = 0;
const SLOT_method                 = 1;
const SLOT_getter                 = 2;
const SLOT_setter                 = 3;
const SLOT_class                  = 4;
const SLOT_function               = 6;-}

{-const METHOD_Arguments            = 0x1;
const METHOD_Activation           = 0x2;
const METHOD_Needrest             = 0x4;
const METHOD_HasOptional          = 0x8;
const METHOD_IgnoreRest           = 0x10;
const METHOD_Native               = 0x20;
const METHOD_Setsdxns             = 0x40;
const METHOD_HasParamNames        = 0x80;-}

-- 4.8
data TraitsInfo = TraitsInfo {
                               tiName :: Int
                             , tiAttribute :: TraitAttributes
                             , tiType :: TraitType
                             }

-- 4.8.1
data TraitType = {- 0 -} TT_Slot TraitSlot
               | {- 1 -} TT_Method TraitMethod
               | {- 2 -} TT_Getter TraitMethod
               | {- 3 -} TT_Setter TraitMethod
               | {- 4 -} TT_Class TraitClass
               | {- 5 -} TT_Function TraitFunction
               | {- 6 -} TT_Const TraitSlot

-- 4.8.2
data TraitSlot = TraitSlot {
                             tsSlotId :: Int
                           , typeName :: Int
                           , vIndex :: Int
                           , vKind :: Int
                           }

-- 4.8.3
data TraitClass = TraitClass {
                               ctSlotId :: Int
                             , classi :: Int
                             }

-- 4.8.4
data TraitFunction = TraitFunction {
                                     tfSlotId :: Int
                                   , function :: Int
                                   }

-- 4.8.5
data TraitMethod = TraitMethod {
                                 dispId :: Int
                               , method :: Int
                               }

-- 4.8.6
data TraitAttributes = {- 0x01 -} TA_Final
                     | {- 0x02 -} TA_Override
                     | {- 0x04 -} TA_Metadata























