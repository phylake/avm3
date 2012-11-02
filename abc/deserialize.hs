module ABC.Deserialize where

{-# LANGUAGE BangPatterns #-}

import ABC.Def
import ABC.Util
import Control.Concurrent.STM
import Control.Monad.State
import Data.Bits
import Data.Int (Int32)
import Data.Word
import System (getArgs)
import TFish
import Util.Words hiding
    (
      fromU16
    , fromU16LE
    , fromU32
    , fromU32LE
    , fromDouble
    , fromDoubleLE
    , fromU32LE_vl
    , fromU30LE_vl
    , fromS32LE_vl
    )
import SWF.Deserialize as SWF hiding (testFile)
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified Data.HashTable.IO as H

data Abcs = Abc_Int32 [Int32]
          | Abc_Word32 [Word32]
          | Abc_Double [Double]
          | Abc_String [String]
          | Abc_NSInfo [NSInfo]
          | Abc_NSSet [NSSet]
          | Abc_Multiname [Multiname]
          | Abc_MethodSignature [MethodSignature]

putStrLn2 :: String -> IO ()
putStrLn2 = liftIO . putStrLn

--ht <- liftIO H.new
--liftIO $ H.insert ht "foo" (Abc_String [""])

-- newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

testFile = DBL.readFile "abc/Test.abc" >>= runStateT testFileM

--testFileM :: StateT DBL.ByteString IO (HashTable String Abcs)
testFileM :: StateT DBL.ByteString IO Abc
testFileM = do
    minor <- fromU16LE
    major <- fromU16LE

    ints       <- parseCommon True fromS32LE_vl
    uints      <- parseCommon True fromU32LE_vl
    doubles    <- parseCommon True fromDoubleLE
    strings    <- parseCommon True parseStringInfo
    nsinfos    <- parseCommon True parseNSInfo
    nssets     <- parseCommon True parseNSSet
    multinames <- parseCommon True parseMultiname
    signatures <- parseCommon False parseMethodSignature
    return Abc {
          abcInts       = 0:ints
        , abcUints      = 0:uints
        , abcDoubles    = 0:doubles
        , abcStrings    = "":strings
        , abcNsInfo     = NSInfo_Any:nsinfos
        , abcNsSet      = nssets
        , abcMultinames = multinames
        , abcMethodSigs = signatures
    }

parseCommon :: Bool
            -> StateT DBL.ByteString IO a
            -> StateT DBL.ByteString IO [a]
parseCommon hasOne f = do
    u30 <- fromU30LE_vl
    let u30' = if hasOne -- make sure 0 and 1 == 0
        then fromIntegral $ (u30 <||> 1) - 1
        else fromIntegral u30
    forNState u30' f

forNState :: Int -> StateT s IO a -> StateT s IO [a]
forNState n f = if n > 0
    then do
        x <- f
        xs <- forNState (n-1) f
        return $ x:xs
    else do return []

{-
    4.4
    String
-}

parseStringInfo :: StateT DBL.ByteString IO String
parseStringInfo = do
    u30 <- fromU30LE_vl
    string <- StateT $ return . DBL.splitAt (fromIntegral u30)
    return $ DBLC.unpack string

{-
    4.4.1
    Namespace
-}
parseNSInfo :: StateT DBL.ByteString IO NSInfo
parseNSInfo = do
    (w:[]) <- nWordsT 1
    idx <- fromU30LE_vl
    return $ parseNSInfoImpl w idx
    where
        parseNSInfoImpl w
            | w == 0x08 = NSInfo_Namespace
            | w == 0x16 = NSInfo_PackageNamespace
            | w == 0x17 = NSInfo_PackageInternalNs
            | w == 0x18 = NSInfo_ProtectedNamespace
            | w == 0x19 = NSInfo_ExplicitNamespace
            | w == 0x1A = NSInfo_StaticProtectedNs
            | w == 0x05 = NSInfo_PrivateNs

{-
    4.4.2
    Namespace set
-}
parseNSSet :: StateT DBL.ByteString IO NSSet
parseNSSet = do
    count <- fromU30LE_vl
    forNState (fromIntegral count) fromU30LE_vl

{-
    4.4.3
    Multiname
-}
parseMultiname :: StateT DBL.ByteString IO Multiname
parseMultiname = do
    (w:[]) <- nWordsT 1
    parseMultinameImpl w

parseMultinameImpl :: Word8
                   -> StateT DBL.ByteString IO Multiname
parseMultinameImpl w
    | w == 0x07 = parseMultinameDouble Multiname_QName
    | w == 0x0D = parseMultinameDouble Multiname_QNameA
    | w == 0x0F = fromU30LE_vl >>= return . Multiname_RTQName
    | w == 0x10 = fromU30LE_vl >>= return . Multiname_RTQNameA
    | w == 0x11 = return Multiname_RTQNameL
    | w == 0x12 = return Multiname_RTQNameLA
    | w == 0x09 = parseMultinameDouble Multiname_Multiname
    | w == 0x0E = parseMultinameDouble Multiname_MultinameA
    | w == 0x1B = fromU30LE_vl >>= return . Multiname_MultinameL
    | w == 0x1C = fromU30LE_vl >>= return . Multiname_MultinameLA

parseMultinameDouble :: (Word32 -> Word32 -> Multiname)
                     -> StateT DBL.ByteString IO Multiname
parseMultinameDouble f = do
    nameOrNamespace <- fromU30LE_vl
    nameOrSet <- fromU30LE_vl
    return $ f nameOrNamespace nameOrSet

{-
    4.5
    Method signature
-}
parseMethodSignature :: StateT DBL.ByteString IO MethodSignature
parseMethodSignature = do
    paramCount <- fromU30LE_vl
    returnType <- fromU30LE_vl
    pTypes <- forNState (fromIntegral paramCount) fromU30LE_vl
    name <- fromU30LE_vl
    (w:[]) <- nWordsT 1
    return $ MethodSignature Multiname_Any [] 0 0 Nothing Nothing
    --(MethodSignature Multiname_Any [] 0 0 Nothing Nothing, bs0)
    
{-
    4.5.1
    Optional parameters
-}


