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

type ByteString = DBL.ByteString

testFile = do
    bs <- DBL.readFile "Test.abc"
    let (minor, bs') = fromU16 bs
    let (major, bs'') = fromU16 bs'
    putStrLn $ "major: " ++ show major
    putStrLn $ "minor: " ++ show minor
    let constantPool = parseConstantPool (defaultPool, bs'')
    putStrLn $ show constantPool

parseConstantPool :: (ConstantPool, ByteString)
                  -> (ConstantPool, ByteString)
--parseConstantPool = parseCommon parseDoubles .
parseConstantPool = parseCommon parseCpUints .
{-parseConstantPool =-} parseCommon parseCpInts

parseCpInts :: ByteString
            -> ConstantPool
            -> ConstantPool
parseCpInts bs pool = cpIntsF pool (++ints bs)
    where
        ints :: ByteString -> [Int32]
        ints bs = allBytes fromS32_vl [] bs

parseCpUints :: ByteString
            -> ConstantPool
            -> ConstantPool
parseCpUints bs pool = cpUintsF pool (++uints bs)
    where
        uints :: ByteString -> [Word32]
        uints bs = allBytes fromU32_vl [] bs

parseDoubles :: ByteString
            -> ConstantPool
            -> ConstantPool
parseDoubles bs pool = cpDoublesF pool (++doubles bs)
    where
        doubles :: ByteString -> [Double]
        doubles bs = allBytes fromDouble [] bs

parseCommon :: (ByteString -> a -> a)
            -> (a, ByteString)
            -> (a, ByteString)
parseCommon f (pool, bs) =
    let (u30, bs') = fromU30_vl bs in
    {- minor input validation such that bs aren't splitAt u30 == 1 -}
    let notOne = fromIntegral $ (u30 <||> 1) - 1 in
    let (half1, half2) = DBL.splitAt notOne bs' in
    if DBL.null half1
        then (pool, half2)
        else (f half1 pool, half2)

-- 4.3
data ConstantPool = ConstantPool {
                                   cpInts :: [Int32]
                                 , cpUints :: [Word32]
                                 , cpDoubles :: [Double]
                                 , cpStrings :: [String]
                                 }
                                 deriving (Show)

defaultPool = ConstantPool [0] [0] [0] [""]

cpIntsF :: ConstantPool -> ([Int32] -> [Int32]) -> ConstantPool
cpIntsF cp f = cpIntsR (f $ cpInts cp) cp

cpIntsR :: [Int32] -> ConstantPool -> ConstantPool
cpIntsR value cp = ConstantPool
    value
    (cpUints cp)
    (cpDoubles cp)
    (cpStrings cp)

cpUintsF :: ConstantPool -> ([Word32] -> [Word32]) -> ConstantPool
cpUintsF cp f = cpUintsR (f $ cpUints cp) cp

cpUintsR :: [Word32] -> ConstantPool -> ConstantPool
cpUintsR value cp = ConstantPool
    (cpInts cp)
    value
    (cpDoubles cp)
    (cpStrings cp)

cpDoublesF :: ConstantPool -> ([Double] -> [Double]) -> ConstantPool
cpDoublesF cp f = cpDoublesR (f $ cpDoubles cp) cp

cpDoublesR :: [Double] -> ConstantPool -> ConstantPool
cpDoublesR value cp = ConstantPool
    (cpInts cp)
    (cpUints cp)
    value
    (cpStrings cp)

cpStringsF :: ConstantPool -> ([String] -> [String]) -> ConstantPool
cpStringsF cp f = cpStringsR (f $ cpStrings cp) cp

cpStringsR :: [String] -> ConstantPool -> ConstantPool
cpStringsR value cp = ConstantPool
    (cpInts cp)
    (cpUints cp)
    (cpDoubles cp)
    value






type OptionDetail = (Int, Int)
type OptionInfo = [OptionDetail]

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























