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

-- newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

testFile = DBL.readFile "abc/Test.abc" >>= runStateT parseAbc

--parseAbc :: StateT DBL.ByteString IO (HashTable String Abcs)
parseAbc :: Parser Abc
parseAbc = do
    minor <- fromU16LE
    major <- fromU16LE

    ints       <- common True fromS32LE_vl
    uints      <- common True fromU32LE_vl
    doubles    <- common True fromDoubleLE
    strings    <- common True parseStrings
    nsinfos    <- common True parseNSInfos
    nssets     <- common True parseNSSets
    multinames <- common True parseMultinames
    signatures <- common False parseMethodSignatures
    metadata   <- common False parseMetadata

    liftIO.putStrLn $ "strings " ++ show strings
    liftIO.putStrLn $ "nsinfos " ++ show nsinfos
    liftIO.putStrLn $ "nssets " ++ show nssets
    liftIO.putStrLn $ "multinames " ++ show multinames
    liftIO.putStrLn $ "signature returnTypes " ++ (concat (map (show.returnType) signatures))
    liftIO.putStrLn $ "metadata " ++ show metadata
    
    classCount <- fromU30LE_vl
    instances <- forNState parseInstances classCount
    classes <- forNState parseClasses classCount
    
    --scripts <- common False parseScripts
    return Abc {
        abcInts       = 0:ints
      , abcUints      = 0:uints
      , abcDoubles    = 0:doubles
      , abcStrings    = "":strings
      , abcNsInfo     = NSInfo_Any:nsinfos
      , abcNsSet      = []:nssets
      , abcMultinames = Multiname_Any:multinames
      , abcMethodSigs = signatures
      , abcMetadata   = metadata
      , abcInstances  = instances
      , abcClasses    = classes
      , abcScripts    = []
    }

common :: Bool
       -> Parser a
       -> Parser [a]
common hasOne f = do
    u30 <- fromU30LE_vl
    let u30' = if hasOne -- make sure 0 and 1 == 0
        then fromIntegral $ (u30 <||> 1) - 1
        else fromIntegral u30
    forNState f u30'

forNState :: (Ord a, Num a, Monad m) => m b -> a -> m [b]
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

parseStrings :: Parser String
parseStrings = do
    u30 <- fromU30LE_vl
    string <- StateT $ return . DBL.splitAt (fromIntegral u30)
    return $ DBLC.unpack string

{-
    4.4.1
    Namespace
-}
parseNSInfos :: Parser NSInfo
parseNSInfos = do
    (w:[]) <- nWordsT 1
    fromU30LE_vl >>= return . parseNSInfoImpl w
    where
        parseNSInfoImpl w
            | w == cpc_Namespace = NSInfo_Namespace
            | w == cpc_PackageNamespace = NSInfo_PackageNamespace
            | w == cpc_PackageInternalNs = NSInfo_PackageInternalNs
            | w == cpc_ProtectedNamespace = NSInfo_ProtectedNamespace
            | w == cpc_ExplicitNamespace = NSInfo_ExplicitNamespace
            | w == cpc_StaticProtectedNs = NSInfo_StaticProtectedNs
            | w == cpc_PrivateNamespace = NSInfo_PrivateNs

{-
    4.4.2
    Namespace set
-}
parseNSSets :: Parser NSSet
parseNSSets = fromU30LE_vl >>= forNState fromU30LE_vl

{-
    4.4.3
    Multiname
-}
parseMultinames :: Parser Multiname
parseMultinames = do
    (w:[]) <- nWordsT 1
    multinameImpl w

multinameImpl :: Word8
              -> Parser Multiname
multinameImpl w
    | w == cpc_QName = multinameDouble Multiname_QName
    | w == cpc_QNameA = multinameDouble Multiname_QNameA
    | w == cpc_RTQName = fromU30LE_vl >>= return . Multiname_RTQName
    | w == cpc_RTQNameA = fromU30LE_vl >>= return . Multiname_RTQNameA
    | w == cpc_RTQNameL = return Multiname_RTQNameL
    | w == cpc_RTQNameLA = return Multiname_RTQNameLA
    | w == cpc_Multiname = multinameDouble Multiname_Multiname
    | w == cpc_MultinameA = multinameDouble Multiname_MultinameA
    | w == cpc_MultinameL = fromU30LE_vl >>= return . Multiname_MultinameL
    | w == cpc_MultinameLA = fromU30LE_vl >>= return . Multiname_MultinameLA

multinameDouble :: (Word32 -> Word32 -> Multiname)
                -> Parser Multiname
multinameDouble f = do
    nameOrNamespace <- fromU30LE_vl
    nameOrSet <- fromU30LE_vl
    return $ f nameOrNamespace nameOrSet

{-
    4.5
    Method signature
-}
parseMethodSignatures :: Parser MethodSignature
parseMethodSignatures = do
    paramCount <- fromU30LE_vl
    returnType <- fromU30LE_vl
    paramTypes <- forNState fromU30LE_vl paramCount
    name <- fromU30LE_vl
    (flags:[]) <- nWordsT 1
    optionInfo <- if (flags .&. msflag_HAS_OPTIONAL == 1)
        then parseOptionalParams
        else return Nothing
    paramNames <- if (flags .&. msflag_HAS_PARAM_NAMES == 1)
        then parseParamNames paramCount
        else return Nothing
    return MethodSignature {
        returnType = returnType
      , paramTypes = paramTypes
      , methodName = name
      , flags = flags
      , optionInfo = optionInfo
      , paramNames = paramNames
    }

{-
    4.5.1
    Optional parameters
-}

parseOptionalParams :: Parser (Maybe [CPC])
parseOptionalParams =
    fromU30LE_vl >>= forNState optionDetail >>= return . Just

optionDetail :: Parser CPC
optionDetail = do
    val <- fromU30LE_vl
    (kind:[]) <- nWordsT 1
    return $ cpcChoice kind val

cpcChoice :: Word8 -> Word32 -> CPC
cpcChoice w idx
    | w == cpc_Undefined = CPC_Undefined
    | w == cpc_Utf8 = CPC_Utf8 idx
    | w == cpc_Decimal = CPC_Decimal idx
    | w == cpc_Int = CPC_Int idx
    | w == cpc_Uint = CPC_Uint idx
    | w == cpc_PrivateNamespace = CPC_PrivateNamespace idx
    | w == cpc_Double = CPC_Double idx
    | w == cpc_QName = CPC_QName idx
    | w == cpc_Namespace = CPC_Namespace idx
    | w == cpc_Multiname = CPC_Multiname idx
    | w == cpc_False = CPC_False
    | w == cpc_True = CPC_True
    | w == cpc_Null = CPC_Null
    | w == cpc_QNameA = CPC_QNameA idx
    | w == cpc_MultinameA = CPC_MultinameA idx
    | w == cpc_RTQName = CPC_RTQName idx
    | w == cpc_RTQNameA = CPC_RTQNameA idx
    | w == cpc_RTQNameL = CPC_RTQNameL idx
    | w == cpc_RTQNameLA = CPC_RTQNameLA idx
    | w == cpc_NameL = CPC_NameL idx
    | w == cpc_NameLA = CPC_NameLA idx
    | w == cpc_NamespaceSet = CPC_NamespaceSet idx
    | w == cpc_PackageNamespace = CPC_PackageNamespace idx
    | w == cpc_PackageInternalNs = CPC_PackageInternalNs idx
    | w == cpc_ProtectedNamespace = CPC_ProtectedNamespace idx
    | w == cpc_ExplicitNamespace = CPC_ExplicitNamespace idx
    | w == cpc_StaticProtectedNs = CPC_StaticProtectedNs idx
    | w == cpc_MultinameL = CPC_MultinameL idx
    | w == cpc_MultinameLA = CPC_MultinameLA idx

{-
    4.5.2
    Parameter names
-}

parseParamNames :: Word32 -> Parser (Maybe [Word32])
parseParamNames count =
    forNState fromU30LE_vl count >>= return . Just

{-
    4.6
    metadata
-}

parseMetadata :: Parser Metadata
parseMetadata = do
    name <- fromU30LE_vl
    pairs <- fromU30LE_vl
    keys <- forNState fromU30LE_vl pairs
    values <- forNState fromU30LE_vl pairs
    return $ Metadata name $ zip keys values

{-
    4.7
    instances
-}

parseInstances :: Parser InstanceInfo
parseInstances = do
    name <- fromU30LE_vl
    superName <- fromU30LE_vl
    (flags:[]) <- nWordsT 1
    protectedNs <- if (flags .&. instf_CLASS_PROTECTEDNS == 1)
        then fromU30LE_vl >>= return . Just
        else return Nothing
    interfaces <- fromU30LE_vl >>= forNState fromU30LE_vl
    iinit <- fromU30LE_vl
    traits <- fromU30LE_vl >>= forNState parseTrait
    return InstanceInfo {
        instName = name
      , instSuperName = superName
      , instFlags = flags
      , instNs = protectedNs
      , instInterface = interfaces
      , instInit = iinit
      , instTraits = traits
    }

{-
    4.8
    traits info
-}

parseTrait :: Parser TraitsInfo
parseTrait = do
    name <- fromU30LE_vl
    (kind:[]) <- nWordsT 1
    traitType <- traitInfoChoice (kind .&. 0xf)
    meta <- if (kind .&. 0x40 == 1)
        then fromU30LE_vl >>= forNState fromU30LE_vl >>= return . Just
        else return Nothing
    return TraitsInfo {
        tiName = name
      , tiAttributes = kind `shiftR` 4
      , tiType = traitType
      , tiMeta = meta
    }

{-
    4.8.1
    trait type
-}

traitInfoChoice :: Word8 -> Parser TraitType
traitInfoChoice w
    | w == 0 = parseTraitSlot TT_Slot
    | w == 1 = parseTraitMethod TT_Method
    | w == 2 = parseTraitMethod TT_Getter
    | w == 3 = parseTraitMethod TT_Setter
    | w == 4 = parseTraitClass TT_Class
    | w == 5 = parseTraitFunction TT_Function
    | w == 6 = parseTraitSlot TT_Const

{-
    4.8.2
    trait slot
-}

parseTraitSlot :: (TraitSlot -> TraitType) -> Parser TraitType
parseTraitSlot f = do
    slot <- fromU30LE_vl
    name <- fromU30LE_vl
    index <- fromU30LE_vl
    kind <- if index == 0
        then return Nothing
        else do
            (w:[]) <- nWordsT 1
            return $ Just w
    return $ f TraitSlot {
        tsId = slot
      , tsName = name
      , tsVindex = index
      , tsVkind = kind
    }

{-
    4.8.3
    trait class
-}

parseTraitClass :: (TraitClass -> TraitType) -> Parser TraitType
parseTraitClass f = do
    slot <- fromU30LE_vl
    init <- fromU30LE_vl
    return $ f TraitClass {
        tcId = slot
      , tcInit = init
    }

{-
    4.8.4
    trait function
-}

parseTraitFunction :: (TraitFunction -> TraitType) -> Parser TraitType
parseTraitFunction f = do
    id <- fromU30LE_vl
    func <- fromU30LE_vl
    return $ f TraitFunction {
        tfId = id
      , tfFunc = func
    }

{-
    4.8.5
    trait method
-}

parseTraitMethod :: (TraitMethod -> TraitType) -> Parser TraitType
parseTraitMethod f = do
    id <- fromU30LE_vl
    meth <- fromU30LE_vl
    return $ f TraitMethod {
        tmDispId = id
      , tmMethod = meth
    }
    
{-
    4.9
    class info
-}

parseClasses :: Parser ClassInfo
parseClasses = do
    initIdx <- fromU30LE_vl
    traits <- fromU30LE_vl >>= forNState parseTrait
    return ClassInfo {
        ciInit = initIdx
      , ciTraits = traits
    }

{-
    4.9
    class info
-}

parseScripts :: Parser ScriptInfo
parseScripts = do
    initIdx <- fromU30LE_vl
    traits <- fromU30LE_vl >>= forNState parseTrait
    return ScriptInfo {
        siInit = initIdx
      , siTraits = traits
    }


















