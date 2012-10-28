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
    let constantPool = parseConstantPool (defaultPool, bs'')
    putStrLn $ show constantPool

parseCommon :: ((a, ByteString) -> (a, ByteString))
            -> (a, ByteString)
            -> (a, ByteString)
parseCommon f (pool, bs) =
    let (u30, bs') = fromU30LE_vl bs in
    {-
      all constant pools fields already have a length of 1 but the u30
      represents the total length so a raw u30 of 1 isn't valid
    -}
    let u30' = fromIntegral $ (u30 <||> 1) - 1 in
    forN' f (pool, bs') u30'

-- 4.3
data ConstantPool = ConstantPool {
                                   cpInts :: [Int32]
                                 , cpUints :: [Word32]
                                 , cpDoubles :: [Double]
                                 , cpStrings :: [String]
                                 , cpNsInfo :: [NSInfo]
                                 , cpNsSet :: [NSSet]
                                 , cpMultiname :: [Multiname]
                                 }
                                 deriving (Show)

defaultPool :: ConstantPool
defaultPool = ConstantPool [0] [0] [0] [""] [NSInfo_Any] [] []

cpIntsIdx :: (Integral a) => ConstantPool -> a -> Int32
cpIntsIdx cp w = cpInts cp !! fromIntegral w

cpIntsF :: ConstantPool -> ([Int32] -> [Int32]) -> ConstantPool
cpIntsF cp f = cpIntsR (f $ cpInts cp) cp

cpIntsR :: [Int32] -> ConstantPool -> ConstantPool
cpIntsR value cp = ConstantPool
    value
    (cpUints cp)
    (cpDoubles cp)
    (cpStrings cp)
    (cpNsInfo cp)
    (cpNsSet cp)
    (cpMultiname cp)

cpUintsIdx :: (Integral a) => ConstantPool -> a -> Word32
cpUintsIdx cp w = cpUints cp !! fromIntegral w

cpUintsF :: ConstantPool -> ([Word32] -> [Word32]) -> ConstantPool
cpUintsF cp f = cpUintsR (f $ cpUints cp) cp

cpUintsR :: [Word32] -> ConstantPool -> ConstantPool
cpUintsR value cp = ConstantPool
    (cpInts cp)
    value
    (cpDoubles cp)
    (cpStrings cp)
    (cpNsInfo cp)
    (cpNsSet cp)
    (cpMultiname cp)

cpDoublesIdx :: (Integral a) => ConstantPool -> a -> Double
cpDoublesIdx cp w = cpDoubles cp !! fromIntegral w

cpDoublesF :: ConstantPool -> ([Double] -> [Double]) -> ConstantPool
cpDoublesF cp f = cpDoublesR (f $ cpDoubles cp) cp

cpDoublesR :: [Double] -> ConstantPool -> ConstantPool
cpDoublesR value cp = ConstantPool
    (cpInts cp)
    (cpUints cp)
    value
    (cpStrings cp)
    (cpNsInfo cp)
    (cpNsSet cp)
    (cpMultiname cp)

cpStringsIdx :: (Integral a) => ConstantPool -> a -> String
cpStringsIdx cp w = cpStrings cp !! fromIntegral w

cpStringsF :: ConstantPool -> ([String] -> [String]) -> ConstantPool
cpStringsF cp f = cpStringsR (f $ cpStrings cp) cp

cpStringsR :: [String] -> ConstantPool -> ConstantPool
cpStringsR value cp = ConstantPool
    (cpInts cp)
    (cpUints cp)
    (cpDoubles cp)
    value
    (cpNsInfo cp)
    (cpNsSet cp)
    (cpMultiname cp)

cpNsInfoIdx :: (Integral a) => ConstantPool -> a -> NSInfo
cpNsInfoIdx cp w = cpNsInfo cp !! fromIntegral w

cpNsInfoF :: ConstantPool -> ([NSInfo] -> [NSInfo]) -> ConstantPool
cpNsInfoF cp f = cpNsInfoR (f $ cpNsInfo cp) cp

cpNsInfoR :: [NSInfo] -> ConstantPool -> ConstantPool
cpNsInfoR value cp = ConstantPool
    (cpInts cp)
    (cpUints cp)
    (cpDoubles cp)
    (cpStrings cp)
    value
    (cpNsSet cp)
    (cpMultiname cp)

cpNsSetIdx :: (Integral a) => ConstantPool -> a -> NSSet
cpNsSetIdx cp w = cpNsSet cp !! fromIntegral w

cpNsSetF :: ConstantPool -> ([NSSet] -> [NSSet]) -> ConstantPool
cpNsSetF cp f = cpNsSetR (f $ cpNsSet cp) cp

cpNsSetR :: [NSSet] -> ConstantPool -> ConstantPool
cpNsSetR value cp = ConstantPool
    (cpInts cp)
    (cpUints cp)
    (cpDoubles cp)
    (cpStrings cp)
    (cpNsInfo cp)
    value
    (cpMultiname cp)

cpMultinameIdx :: (Integral a) => ConstantPool -> a -> Multiname
cpMultinameIdx cp w = cpMultiname cp !! fromIntegral w

cpMultinameF :: ConstantPool -> ([Multiname] -> [Multiname]) -> ConstantPool
cpMultinameF cp f = cpMultinameR (f $ cpMultiname cp) cp

cpMultinameR :: [Multiname] -> ConstantPool -> ConstantPool
cpMultinameR value cp = ConstantPool
    (cpInts cp)
    (cpUints cp)
    (cpDoubles cp)
    (cpStrings cp)
    (cpNsInfo cp)
    (cpNsSet cp)
    value

parseConstantPool :: (ConstantPool, ByteString)
                  -> (ConstantPool, ByteString)
parseConstantPool = parseCommon parseCpMultiname .
                    parseCommon parseCpNsSet .
                    parseCommon parseCpNsInfo .
                    parseCommon parseCpString .
                    parseCommon parseCpDouble .
                    parseCommon parseCpUint .
                    parseCommon parseCpInt

parseCpInt :: (ConstantPool, ByteString)
           -> (ConstantPool, ByteString)
parseCpInt (pool, bs) =
    let (i32, bs') = fromS32LE_vl bs in
    (cpIntsF pool (++[i32]), bs')

parseCpUint :: (ConstantPool, ByteString)
            -> (ConstantPool, ByteString)
parseCpUint (pool, bs) =
    let (u32, bs') = fromU32LE_vl bs in
    (cpUintsF pool (++[u32]), bs')

parseCpDouble :: (ConstantPool, ByteString)
              -> (ConstantPool, ByteString)
parseCpDouble (pool, bs) =
    let (double, bs') = fromDoubleLE bs in
    (cpDoublesF pool (++[double]), bs')

parseCpString :: (ConstantPool, ByteString)
              -> (ConstantPool, ByteString)
parseCpString (pool, bs) =
    let (str, bs') = parseStringInfo bs in
    (cpStringsF pool (++[str]), bs')

parseCpNsInfo :: (ConstantPool, ByteString)
              -> (ConstantPool, ByteString)
parseCpNsInfo (pool, bs) =
    let (ns, bs') = parseNSInfo pool bs in
    (cpNsInfoF pool (++[ns]), bs')

parseCpNsSet :: (ConstantPool, ByteString)
             -> (ConstantPool, ByteString)
parseCpNsSet (pool, bs) =
    let (ns, bs') = parseNSSet pool bs in
    (cpNsSetF pool (++[ns]), bs')

parseCpMultiname :: (ConstantPool, ByteString)
                 -> (ConstantPool, ByteString)
parseCpMultiname (pool, bs) =
    let (multinames, bs') = parseMultiname pool bs in
    (cpMultinameF pool (++[multinames]), bs')

-- 4.4
parseStringInfo :: DBL.ByteString -> (String, DBL.ByteString)
parseStringInfo bs =
    let (u30, bs') = fromU30LE_vl bs in
    let (half1, half2) = DBL.splitAt (fromIntegral u30) bs' in
    (DBLC.unpack half1, half2)

-- 4.4.1
data NSInfo = {- 0x08 -} NSInfo_Namespace String
            | {- 0x16 -} NSInfo_PackageNamespace String
            | {- 0x17 -} NSInfo_PackageInternalNs String
            | {- 0x18 -} NSInfo_ProtectedNamespace String
            | {- 0x19 -} NSInfo_ExplicitNamespace String
            | {- 0x1A -} NSInfo_StaticProtectedNs String
            | {- 0x05 -} NSInfo_PrivateNs String
            | {- 0x05 -} NSInfo_Any {- *. see 4.4.4 QName -}
            deriving (Show)

parseNSInfo :: ConstantPool -> DBL.ByteString -> (NSInfo, DBL.ByteString)
parseNSInfo pool bs =
    let ((w:[]), bs') = nWords 1 bs in
    let (idx, bs'') = fromU30LE_vl bs' in
    let ns = parseNSInfoImpl w $ cpStringsIdx pool idx in
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

-- 4.4.2
type NSSet = [NSInfo]

parseNSSet :: ConstantPool -> DBL.ByteString -> (NSSet, DBL.ByteString)
parseNSSet pool bs =
    let (count, bs') = fromU30LE_vl bs in
    let (_, set, bs'') = forN' parseNSSetImpl (pool, [], bs') count in
    (set, bs'')

parseNSSetImpl :: (ConstantPool, NSSet, DBL.ByteString)
               -> (ConstantPool, NSSet, DBL.ByteString)
parseNSSetImpl (pool, ns, bs) =
    let (idx, bs') = fromU30LE_vl bs in
    let nsInfo = cpNsInfoIdx pool idx in
    (pool, ns ++ [nsInfo], bs')

-- 4.4.3
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
               deriving (Show)

parseMultiname :: ConstantPool -> DBL.ByteString -> (Multiname, DBL.ByteString)
parseMultiname pool bs =
    let ((w:[]), bs') = nWords 1 bs in
    parseMultinameImpl w pool bs'

parseMultinameImpl :: Word8
                   -> (ConstantPool
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
                -> ConstantPool
                -> DBL.ByteString
                -> (Multiname, DBL.ByteString)
parseMultinameQ f pool bs = undefined

parseMultinameRTQ :: (String -> Multiname)
                  -> ConstantPool
                  -> DBL.ByteString
                  -> (Multiname, DBL.ByteString)
parseMultinameRTQ f pool bs = undefined

parseMultinameM :: (String -> NSSet -> Multiname)
                -> ConstantPool
                -> DBL.ByteString
                -> (Multiname, DBL.ByteString)
parseMultinameM f pool bs = undefined

parseMultinameML :: (NSSet -> Multiname)
                 -> ConstantPool
                 -> DBL.ByteString
                 -> (Multiname, DBL.ByteString)
parseMultinameML f pool bs = undefined

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

data MethodInfo = MethodInfo {
                               returnType :: Int
                             , paramType :: [Int]
                             , name :: Int
                             , flags :: Int
                             , optionInfo :: OptionInfo
                             --, paramInfo :: ParamInfo
                             }

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























