module ABC.Deserialize (parseAbc) where

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

putStrLn2 :: String -> IO ()
putStrLn2 = liftIO . putStrLn

--ht <- liftIO H.new
--liftIO $ H.insert ht "foo" (Abc_String [""])

-- newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

testFile = DBL.readFile "abc/Test.abc" >>= runStateT parseAbc

--parseAbc :: StateT DBL.ByteString IO (HashTable String Abcs)
parseAbc :: StateT DBL.ByteString IO Abc
parseAbc = do
    minor <- fromU16LE
    major <- fromU16LE

    ints       <- common True fromS32LE_vl
    uints      <- common True fromU32LE_vl
    doubles    <- common True fromDoubleLE
    strings    <- common True stringInfo
    nsinfos    <- common True nsInfo
    nssets     <- common True nsSet
    multinames <- common True multiname
    signatures <- common False methodSignature
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

common :: Bool
       -> StateT DBL.ByteString IO a
       -> StateT DBL.ByteString IO [a]
common hasOne f = do
    u30 <- fromU30LE_vl
    let u30' = if hasOne -- make sure 0 and 1 == 0
        then fromIntegral $ (u30 <||> 1) - 1
        else fromIntegral u30
    forNState f u30'

forNState :: StateT s IO a -> Int -> StateT s IO [a]
forNState f n = if n > 0
    then do
        x <- f
        xs <- forNState f (n-1)
        return $ x:xs
    else do return []

{-
    4.4
    String
-}

stringInfo :: StateT DBL.ByteString IO String
stringInfo = do
    u30 <- fromU30LE_vl
    string <- StateT $ return . DBL.splitAt (fromIntegral u30)
    return $ DBLC.unpack string

{-
    4.4.1
    Namespace
-}
nsInfo :: StateT DBL.ByteString IO NSInfo
nsInfo = do
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
nsSet :: StateT DBL.ByteString IO NSSet
nsSet = fromU30LE_vl >>= forNState fromU30LE_vl . fromIntegral

{-
    4.4.3
    Multiname
-}
multiname :: StateT DBL.ByteString IO Multiname
multiname = do
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
methodSignature :: StateT DBL.ByteString IO MethodSignature
methodSignature = do
    paramCount <- fromU30LE_vl
    returnType <- fromU30LE_vl
    paramTypes <- forNState fromU30LE_vl $ fromIntegral paramCount
    name <- fromU30LE_vl
    (flags:[]) <- nWordsT 1
    optionInfo <- if (flags .&. msflag_HAS_OPTIONAL == 1)
        then parseOptionalParams
        else return Nothing
    paramNames <- if (flags .&. msflag_HAS_PARAM_NAMES == 1)
        then parseParamNames
        else return Nothing
    return MethodSignature {
         returnType = returnType
       , paramTypes = paramTypes
       , name = name
       , flags = flags
       , optionInfo = optionInfo
       , paramNames = paramNames
    }

{-
    4.5.1
    Optional parameters
-}

parseOptionalParams :: StateT DBL.ByteString IO (Maybe [CPC])
parseOptionalParams =
    fromU30LE_vl >>= forNState optionDetail . fromIntegral >>= return . Just

optionDetail :: StateT DBL.ByteString IO CPC
optionDetail = do
    (w:[]) <- nWordsT 1
    fromU30LE_vl >>= return . cpcChoice w . fromIntegral

cpcChoice :: Word8 -> (Word32 -> CPC)
cpcChoice w
    | w == 0x01 = CPC_Utf8
    | w == 0x03 = CPC_Integer
    | w == 0x04 = CPC_UInt
    | w == 0x05 = CPC_PrivateNamespace
    | w == 0x06 = CPC_Double
    | w == 0x07 = CPC_QName
    | w == 0x08 = CPC_Namespace
    | w == 0x09 = CPC_Multiname
    | w == 0x0A = CPC_False
    | w == 0x0B = CPC_True
    | w == 0x0C = CPC_Null
    | w == 0x0D = CPC_QNameA
    | w == 0x0E = CPC_MultinameA
    | w == 0x0F = CPC_RTQName
    | w == 0x10 = CPC_RTQNameA
    | w == 0x11 = CPC_RTQNameL
    | w == 0x12 = CPC_RTQNameLA
    | w == 0x13 = CPC_NameL
    | w == 0x14 = CPC_NameLA
    | w == 0x15 = CPC_NamespaceSet
    | w == 0x16 = CPC_PackageNamespace
    | w == 0x17 = CPC_PackageInternalNS
    | w == 0x18 = CPC_ProtectedNamespace
    | w == 0x19 = CPC_ExplicitNamespace
    | w == 0x1A = CPC_StaticProtectedNS
    | w == 0x1B = CPC_MultinameL
    | w == 0x1C = CPC_MultinameLA

{-
    4.5.2
    Parameter names
-}

parseParamNames :: StateT DBL.ByteString IO (Maybe [String])
parseParamNames = undefined






























