module ABC.Deserialize where

{-# LANGUAGE BangPatterns #-}

import Control.Concurrent.STM
import Control.Monad.State
import ABC.Def
import ABC.Util
import Data.Bits
import Data.Int (Int32)
import Data.Word
import System (getArgs)
import TFish
import Util hiding ( fromU16
                   , fromU16LE
                   , fromU32
                   , fromU32LE
                   , fromDouble
                   , fromDoubleLE
                   , fromU32LE_vl
                   , fromU30LE_vl
                   , fromS32LE_vl)
import SWF.Deserialize as SWF hiding (testFile)
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Lazy.Char8 as DBLC

testFile = DBL.readFile "Test.abc" >>= runStateT testFileM

testFileM :: StateT DBL.ByteString IO Abc
testFileM = do
    minor <- fromU16LE
    liftIO . putStrLn $ "minor: " ++ show minor
    
    major <- fromU16LE
    liftIO . putStrLn $ "major: " ++ show major
    
    ints       <- parseCommon True fromS32LE_vl
    uints      <- parseCommon True fromU32LE_vl
    doubles    <- parseCommon True fromDoubleLE
    strings    <- parseCommon True parseStringInfo
    nsinfos    <- parseCommon True $ parseNSInfo strings
    nssets     <- parseCommon True $ parseNSSet nsinfos
    multinames <- parseCommon True $ parseMultiname strings nsinfos nssets
    --signatures <- parseCommon False parseMethodSignature
    return Abc {
          abcInts       = 0:ints
        , abcUints      = 0:uints
        , abcDoubles    = 0:doubles
        , abcStrings    = "":strings
        , abcNsInfo     = NSInfo_Any:nsinfos
        , abcNsSet      = nssets
        , abcMultinames = multinames
        , abcMethodSigs = []
    }

-- newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

parseCommon :: Bool
            -> StateT DBL.ByteString IO a
            -> StateT DBL.ByteString IO [a]
parseCommon hasOne f = do
    u30 <- fromU30LE_vl
    let u30' = if hasOne -- make sure 0 and 1 == 0
        then fromIntegral $ (u30 <||> 1) - 1
        else fromIntegral u30
    forNState u30' f

pop :: StateT [a] IO a
pop = StateT $ \(x:xs) -> return (x,xs)

push :: a -> StateT [a] IO ()
push a = StateT $ \xs -> return ((),a:xs)

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
parseNSInfo :: [String] -> StateT DBL.ByteString IO NSInfo
parseNSInfo strings = do
    (w:[]) <- nWordsT 1
    idx <- fromU30LE_vl
    return $ parseNSInfoImpl w $ strings !! (fromIntegral idx)
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
parseNSSet :: [NSInfo] -> StateT DBL.ByteString IO NSSet
parseNSSet ns = do
    count <- fromU30LE_vl
    forNState (fromIntegral count) (parseNSSetImpl ns)

parseNSSetImpl :: [NSInfo] -> StateT DBL.ByteString IO NSInfo
parseNSSetImpl ns = do
    idx <- fromU30LE_vl
    return $ ns !! (fromIntegral idx)

{-
    4.4.3
    Multiname
-}
parseMultiname :: [String]
               -> [NSInfo]
               -> [NSSet]
               -> StateT DBL.ByteString IO Multiname
parseMultiname strings infos sets = do
    (w:[]) <- nWordsT 1
    parseMultinameImpl w strings infos sets

parseMultinameImpl :: Word8
                   -> [String]
                   -> [NSInfo]
                   -> [NSSet]
                   -> StateT DBL.ByteString IO Multiname
parseMultinameImpl w strings infos sets
    | w == 0x07 = parseMultinameQ infos strings Multiname_QName
    | w == 0x0D = parseMultinameQ infos strings Multiname_QNameA
    | w == 0x0F = parseMultinameRTQ strings Multiname_RTQName
    | w == 0x10 = parseMultinameRTQ strings Multiname_RTQNameA
    | w == 0x11 = do { return Multiname_RTQNameL }
    | w == 0x12 = do { return Multiname_RTQNameLA }
    | w == 0x09 = parseMultinameM strings sets Multiname_Multiname
    | w == 0x0E = parseMultinameM strings sets Multiname_MultinameA
    | w == 0x1B = parseMultinameML sets Multiname_MultinameL
    | w == 0x1C = parseMultinameML sets Multiname_MultinameLA

parseMultinameQ :: [NSInfo]
                -> [String] 
                -> (NSInfo -> String -> Multiname)
                -> StateT DBL.ByteString IO Multiname
parseMultinameQ infos strings f = do
    nsIdx <- fromU30LE_vl
    nameIdx <- fromU30LE_vl
    let ns = infos !! (fromIntegral nsIdx)
    let name = strings !! (fromIntegral nameIdx)
    return $ f ns name

parseMultinameRTQ :: [String]
                  -> (String -> Multiname)
                  -> StateT DBL.ByteString IO Multiname
parseMultinameRTQ strings f = do
    nameIdx <- fromU30LE_vl
    let name = strings !! (fromIntegral nameIdx)
    return $ f name

parseMultinameM :: [String]
                -> [NSSet]
                -> (String -> NSSet -> Multiname)
                -> StateT DBL.ByteString IO Multiname
parseMultinameM strings sets f = do
    nameIdx <- fromU30LE_vl
    setIdx <- fromU30LE_vl
    let name = strings !! (fromIntegral nameIdx)
    let set = sets !! (fromIntegral setIdx)
    return $ f name set

parseMultinameML :: [NSSet]
                 -> (NSSet -> Multiname)
                 -> StateT DBL.ByteString IO Multiname
parseMultinameML sets f = do
    setIdx <- fromU30LE_vl
    let set = sets !! (fromIntegral setIdx)
    return $ f set

{-
    4.5
    Method signature
-}
{-parseMethodSignature :: StateT DBL.ByteString IO MethodSignature
parseMethodSignature :: Abc -> DBL.ByteString -> (MethodSignature, DBL.ByteString)
parseMethodSignature abc bs0 =
    let (paramCount, bs1) = fromU30LE_vl bs0 in
    let (returnType, bs2) = fromU30LE_vl bs1 in
    let (pTypes, bs3) = forN' paramCountF ([], bs2) $ fromIntegral paramCount in
    (MethodSignature Multiname_Any [] 0 0 Nothing Nothing, bs0)
    where
        paramCountF :: ([Word32], DBL.ByteString) -> ([Word32], DBL.ByteString)
        paramCountF (ws, bs) =
            let (u30, bs') = fromU30LE_vl bs in (ws ++ [u30], bs')-}
{-
    4.5.1
    Optional parameters
-}


