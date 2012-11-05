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
    
    --metadata   <- common False parseMetadata
    fromU30LE_vl {- abc-parse.as:67 parseMetaData doesn't exist -}
    
    instanceClassCount <- fromU30LE_vl
    get >>= liftIO . DBL.writeFile "abc/preInstance.abc"
    instances <- forNState parseInstances instanceClassCount
    get >>= liftIO . DBL.writeFile "abc/postInstance.abc"
    classes <- forNState parseClasses instanceClassCount
    return Abc {
        abcInts       = 0:ints
      , abcUints      = 0:uints
      , abcDoubles    = 0:doubles
      , abcStrings    = "":strings
      , abcNsInfo     = NSInfo_Any:nsinfos
      , abcNsSet      = []:nssets
      , abcMultinames = Multiname_Any:multinames
      , abcMethodSigs = signatures
      , abcMetadata   = []
      , abcInstances  = instances
      , abcClasses    = classes
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
    | w == 0x07 = multinameDouble Multiname_QName
    | w == 0x0D = multinameDouble Multiname_QNameA
    | w == 0x0F = fromU30LE_vl >>= return . Multiname_RTQName
    | w == 0x10 = fromU30LE_vl >>= return . Multiname_RTQNameA
    | w == 0x11 = return Multiname_RTQNameL
    | w == 0x12 = return Multiname_RTQNameLA
    | w == 0x09 = multinameDouble Multiname_Multiname
    | w == 0x0E = multinameDouble Multiname_MultinameA
    | w == 0x1B = fromU30LE_vl >>= return . Multiname_MultinameL
    | w == 0x1C = fromU30LE_vl >>= return . Multiname_MultinameLA

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
    | w == 0x00 = CPC_Undefined
    | w == 0x01 = CPC_Utf8 idx
    | w == 0x03 = CPC_Int idx
    | w == 0x04 = CPC_Uint idx
    | w == 0x05 = CPC_PrivateNamespace idx
    | w == 0x06 = CPC_Double idx
    | w == 0x07 = CPC_QName idx
    | w == 0x08 = CPC_Namespace idx
    | w == 0x09 = CPC_Multiname idx
    | w == 0x0A = CPC_False
    | w == 0x0B = CPC_True
    | w == 0x0C = CPC_Null
    | w == 0x0D = CPC_QNameA idx
    | w == 0x0E = CPC_MultinameA idx
    | w == 0x0F = CPC_RTQName idx
    | w == 0x10 = CPC_RTQNameA idx
    | w == 0x11 = CPC_RTQNameL idx
    | w == 0x12 = CPC_RTQNameLA idx
    | w == 0x13 = CPC_NameL idx
    | w == 0x14 = CPC_NameLA idx
    | w == 0x15 = CPC_NamespaceSet idx
    | w == 0x16 = CPC_PackageNamespace idx
    | w == 0x17 = CPC_PackageInternalNs idx
    | w == 0x18 = CPC_ProtectedNamespace idx
    | w == 0x19 = CPC_ExplicitNamespace idx
    | w == 0x1A = CPC_StaticProtectedNs idx
    | w == 0x1B = CPC_MultinameL idx
    | w == 0x1C = CPC_MultinameLA idx

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
    pairs <- fromU30LE_vl >>= forNState parseMetadataPair
    return $ Metadata name pairs

parseMetadataPair :: Parser (StringIdx, StringIdx)
parseMetadataPair = do
    key <- fromU30LE_vl
    value <- fromU30LE_vl
    return (key, value)

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
    liftIO.putStrLn $ "trait kind " ++ show kind
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

{-switch(t.kind) {
case TRAIT_Slot:
case TRAIT_Const:
    trait = new ABCSlotTrait(name(t.name), t.attrs, t.kind==TRAIT_Const, t.slot_id, name(t.type_name), t.val_index, t.val_kind);
    break;
case TRAIT_Method:
case TRAIT_Getter:
case TRAIT_Setter:
    trait = new ABCOtherTrait(name(t.name), t.attrs, t.kind, t.disp_id, t.method);
    break;
case TRAIT_Class:
    trait = new ABCOtherTrait(name(t.name), t.attrs, t.kind, t.slot_id, t["class"]);
    break;
}-}

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
    4.7
    classes
-}

parseClasses :: Parser ClassInfo
parseClasses = do
    initIdx <- fromU30LE_vl
    liftIO.putStrLn $ "initIdx " ++ show initIdx
    traitCount <- fromU30LE_vl
    liftIO.putStrLn $ "traitCount " ++ show traitCount
    traits <- forNState parseTrait traitCount

    --traits <- fromU30LE_vl >>= forNState parseTrait
    return ClassInfo {
        ciInit = initIdx
      , ciTraits = traits
    }


















