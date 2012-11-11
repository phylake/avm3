module ABC.Deserialize (parseAbc) where

{-# LANGUAGE BangPatterns #-}

import ABC.Def
import ABC.Util
import Control.Monad.State
import Data.Bits
import Data.Int (Int32)
import Data.Word
import TFish
import Util.Words hiding
    (
      fromU8
    , fromU16
    , fromU16LE
    , fromU32
    , fromU32LE
    , fromDouble
    , fromDoubleLE
    , fromU32LE_vl
    , fromU30LE_vl
    , fromS32LE_vl
    , fromS24LE
    )
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Lazy.Char8 as DBLC

testFile = DBL.readFile "abc/Test.abc" >>= runStateT parseAbc

parseAbc :: Parser Abc
parseAbc = do
    -- TODO validate these
    minor <- fromU16LE -- 16
    major <- fromU16LE -- 46

    ints       <- common True fromS32LE_vl
    uints      <- common True fromU32LE_vl
    doubles    <- common True fromDoubleLE
    strings    <- common True parseStrings
    nsinfos    <- common True parseNSInfos
    nssets     <- common True parseNSSets
    multinames <- common True parseMultinames
    signatures <- common False parseMethodSignatures
    metadata   <- common False parseMetadata

    classCount <- fromU30LE_vl
    instances <- forNState parseInstance classCount
    classes <- forNState parseClass classCount

    scripts <- common False parseScript
    methodBodies <- common False parseMethodBody
    return Abc {
        abcInts         = 0:ints
      , abcUints        = 0:uints
      , abcDoubles      = 0:doubles
      , abcStrings      = "":strings
      , abcNsInfo       = NSInfo_Any:nsinfos
      , abcNsSet        = []:nssets
      , abcMultinames   = Multiname_Any:multinames
      , abcMethodSigs   = signatures
      , abcMetadata     = metadata
      , abcInstances    = instances
      , abcClasses      = classes
      , abcScripts      = scripts
      , abcMethodBodies = methodBodies
    }

common :: Bool -> Parser a -> Parser [a]
common hasOne f = do
    u30 <- fromU30LE_vl
    let u30' = if hasOne -- make sure 0 and 1 == 0
        then fromIntegral $ (u30 <||> 1) - 1
        else fromIntegral u30
    forNState f u30'

forNState :: (Ord n, Num n, Monad m) => m a -> n -> m [a]
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
    w <- fromU8
    fromU30LE_vl >>= return . nsInfoCons w
    where
        nsInfoCons w
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
parseMultinames = fromU8 >>= multinameImpl

multinameImpl :: Word8 -> Parser Multiname
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

multinameDouble :: (Word32 -> Word32 -> Multiname) -> Parser Multiname
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
    flags <- fromU8
    optionInfo <- if (flags .&. msflag_HAS_OPTIONAL == msflag_HAS_OPTIONAL)
        then parseOptionalParams {- TODO checkout count abcdump.as:717 -}
        else return Nothing
    paramNames <- if (flags .&. msflag_HAS_PARAM_NAMES == msflag_HAS_PARAM_NAMES)
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
    kind <- fromU8
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

parseInstance :: Parser InstanceInfo
parseInstance = do
    name <- fromU30LE_vl
    superName <- fromU30LE_vl
    flags <- fromU8
    protectedNs <- if (flags .&. instf_CLASS_PROTECTEDNS == instf_CLASS_PROTECTEDNS)
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
    kind <- fromU8
    traitType <- traitInfoChoice (kind .&. 0xf)
    meta <- if (kind .&. 0x40 == 0x40)
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
    | w == trait_var      = parseTraitVar TT_Var
    | w == trait_method   = parseTraitMethod TT_Method
    | w == trait_getter   = parseTraitMethod TT_Getter
    | w == trait_setter   = parseTraitMethod TT_Setter
    | w == trait_class    = parseTraitClass TT_Class
    | w == trait_function = parseTraitFunction TT_Function
    | w == trait_const    = parseTraitVar TT_Const

{-
    4.8.2
    trait slot
-}

parseTraitVar :: (TraitVar -> TraitType) -> Parser TraitType
parseTraitVar f = do
    slot <- fromU30LE_vl
    name <- fromU30LE_vl
    index <- fromU30LE_vl
    kind <- if index == 0
        then return Nothing
        else do
            (w:[]) <- nWordsT 1
            return $ Just w
    return $ f TraitVar {
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
    tmDispId <- fromU30LE_vl
    tmMethod <- fromU30LE_vl
    return $ f TraitMethod {
        tmDispId = tmDispId
      , tmMethod = tmMethod
    }

{-
    4.9
    class info
-}

parseClass :: Parser ClassInfo
parseClass = do
    ciInit <- fromU30LE_vl
    ciTraits <- fromU30LE_vl >>= forNState parseTrait
    return ClassInfo {
        ciInit = ciInit
      , ciTraits = ciTraits
    }

{-
    4.10
    script
-}

parseScript :: Parser ScriptInfo
parseScript = do
    siInit <- fromU30LE_vl
    siTraits <- fromU30LE_vl >>= forNState parseTrait
    return ScriptInfo {
        siInit = siInit
      , siTraits = siTraits
    }

{-
    4.11
    method body
-}

parseMethodBody :: Parser MethodBody
parseMethodBody = do
    mbMethod <- fromU30LE_vl
    mbMaxStack <- fromU30LE_vl
    mbLocalCount <- fromU30LE_vl
    mbInitScopeDepth <- fromU30LE_vl
    mbMaxScopeDepth <- fromU30LE_vl

    -- TODO there has to be a better way
    mbCodeCount <- fromU30LE_vl
    bs <- get
    let (opcodeBytes, bs2) = DBL.splitAt (fromIntegral mbCodeCount) bs
    put opcodeBytes
    mbCode <- allBytes parseOpCode
    put bs2

    mbExceptions <- fromU30LE_vl >>= forNState parseException
    mbTraits <- fromU30LE_vl >>= forNState parseTrait
    return MethodBody {
        mbMethod = mbMethod
      , mbMaxStack = mbMaxStack
      , mbLocalCount = mbLocalCount
      , mbInitScopeDepth = mbInitScopeDepth
      , mbMaxScopeDepth = mbMaxScopeDepth
      , mbCode = mbCode
      , mbExceptions = mbExceptions
      , mbTraits = mbTraits
    }
    where
        allBytes f = do
            bs <- get
            if DBL.null bs
                then return []
                else do
                    x <- f
                    xs <- allBytes f
                    return $ x:xs

parseOpCode :: Parser OpCode
parseOpCode = fromU8 >>= parseOpCodeChoice

parseOpCodeChoice :: Word8 -> Parser OpCode
parseOpCodeChoice w
    | w == 0x01 = return Breakpoint
    | w == 0x02 = return Nop
    | w == 0x03 = return Throw
    | w == 0x04 = fromU30LE_vl >>= return . GetSuper
    | w == 0x05 = fromU30LE_vl >>= return . SetSuper
    | w == 0x06 = fromU30LE_vl >>= return . DefaultXmlNamespace
    | w == 0x07 = return DefaultXmlNamespaceL
    | w == 0x08 = fromU30LE_vl >>= return . Kill
    | w == 0x09 = return Label
    {-| w == 0x0A = return-}
    {-| w == 0x0B = return-}
    | w == 0x0C = fromS24LE >>= return . IfNotLessThan
    | w == 0x0D = fromS24LE >>= return . IfNotLessEqual
    | w == 0x0E = fromS24LE >>= return . IfNotGreaterThan
    | w == 0x0F = fromS24LE >>= return . IfNotGreaterEqual
    | w == 0x10 = fromS24LE >>= return . Jump
    | w == 0x11 = fromS24LE >>= return . IfTrue
    | w == 0x12 = fromS24LE >>= return . IfFalse
    | w == 0x13 = fromS24LE >>= return . IfEqual
    | w == 0x14 = fromS24LE >>= return . IfNotEqual
    | w == 0x15 = fromS24LE >>= return . IfLessThan
    | w == 0x16 = fromS24LE >>= return . IfLessEqual
    | w == 0x17 = fromS24LE >>= return . IfGreaterThan
    | w == 0x18 = fromS24LE >>= return . IfGreaterEqual
    | w == 0x19 = fromS24LE >>= return . IfStrictEqual
    | w == 0x1A = fromS24LE >>= return . IfStrictNotEqual
    | w == 0x1B = do
        defaultOffset <- fromS24LE
        caseOffsets <- fromU30LE_vl >>= forNState fromS24LE
        return $ LookupSwitch defaultOffset caseOffsets
    | w == 0x1C = return PushWith
    | w == 0x1D = return PopScope
    | w == 0x1E = return NextName
    | w == 0x1F = return HasNext
    | w == 0x20 = return PushNull
    | w == 0x21 = return PushUndefined
    | w == 0x22 = return PushConstant
    | w == 0x23 = return NextValue
    | w == 0x24 = fromU8 >>= return . PushByte
    | w == 0x25 = fromU30LE_vl >>= return . PushShort
    | w == 0x26 = return PushTrue
    | w == 0x27 = return PushFalse
    | w == 0x28 = return PushNaN
    | w == 0x29 = return Pop
    | w == 0x2A = return Dup
    | w == 0x2B = return Swap
    | w == 0x2C = fromU30LE_vl >>= return . PushString
    | w == 0x2D = fromU30LE_vl >>= return . PushInt
    | w == 0x2E = fromU30LE_vl >>= return . PushUInt
    | w == 0x2F = fromU30LE_vl >>= return . PushDouble
    | w == 0x30 = return PushScope
    | w == 0x31 = fromU30LE_vl >>= return . PushNamespace
    | w == 0x32 = return $ HasNext2 0 0
    | w == 0x33 = return PushDecimal
    | w == 0x34 = return PushDNaN
    {-| w == 0x35 = return GetByte-}
    {-| w == 0x36 = return GetShort-}
    {-| w == 0x37 = return GetInt-}
    {-| w == 0x38 = return GetFloat-}
    {-| w == 0x39 = return GetDouble-}
    {-| w == 0x3A = return SetByte-}
    {-| w == 0x3B = return SetShort-}
    {-| w == 0x3C = return SetInt-}
    {-| w == 0x3D = return SetFloat-}
    {-| w == 0x3E = return SetDouble-}
    {-| w == 0x3F = return-}
    | w == 0x40 = fromU30LE_vl >>= return . NewFunction
    | w == 0x41 = fromU30LE_vl >>= return . Call
    | w == 0x42 = fromU30LE_vl >>= return . Construct
    | w == 0x43 = do
        index <- fromU30LE_vl
        argCount <- fromU30LE_vl
        return $ CallMethod index argCount
    | w == 0x44 = do
        index <- fromU30LE_vl
        argCount <- fromU30LE_vl
        return $ CallStatic index argCount
    | w == 0x45 = do
        index <- fromU30LE_vl
        argCount <- fromU30LE_vl
        return $ CallSuper index argCount
    | w == 0x46 = do
        index <- fromU30LE_vl
        argCount <- fromU30LE_vl
        return $ CallProperty index argCount
    | w == 0x47 = return ReturnVoid
    | w == 0x48 = return ReturnValue
    | w == 0x49 = fromU30LE_vl >>= return . ConstructSuper
    | w == 0x4A = do
        index <- fromU30LE_vl
        argCount <- fromU30LE_vl
        return $ ConstructProp index argCount
    | w == 0x4B = return CallSuperId
    | w == 0x4C = do
        index <- fromU30LE_vl
        argCount <- fromU30LE_vl
        return $ CallPropLex index argCount
    | w == 0x4D = return CallInterface
    | w == 0x4E = do
        index <- fromU30LE_vl
        argCount <- fromU30LE_vl
        return $ CallSuperVoid index argCount
    | w == 0x4F = do
        index <- fromU30LE_vl
        argCount <- fromU30LE_vl
        return $ CallPropVoid index argCount
    {-| w == 0x50 = return Sign1-}
    {-| w == 0x51 = return Sign8-}
    {-| w == 0x52 = return Sign16-}
    | w == 0x53 = return ApplyType
    | w == 0x55 = fromU30LE_vl >>= return . NewObject
    | w == 0x56 = fromU30LE_vl >>= return . NewArray
    | w == 0x57 = return NewActivation
    | w == 0x58 = fromU30LE_vl >>= return . NewClass
    | w == 0x59 = fromU30LE_vl >>= return . GetDescendants
    | w == 0x5A = fromU30LE_vl >>= return . NewCatch
    | w == 0x5B = return FindPropGlobalStrict
    | w == 0x5C = return FindPropGlobal
    | w == 0x5D = fromU30LE_vl >>= return . FindPropStrict
    | w == 0x5E = fromU30LE_vl >>= return . FindProperty
    | w == 0x5F = return FindDef
    | w == 0x60 = fromU30LE_vl >>= return . GetLex
    | w == 0x61 = fromU30LE_vl >>= return . SetProperty
    | w == 0x62 = fromU30LE_vl >>= return . GetLocal
    | w == 0x63 = fromU30LE_vl >>= return . SetLocal
    | w == 0x64 = return GetGlobalScope
    | w == 0x65 = fromU8 >>= return . GetScopeObject
    | w == 0x66 = fromU30LE_vl >>= return . GetProperty
    | w == 0x67 = return GetPropertyLate
    | w == 0x68 = fromU30LE_vl >>= return . InitProperty
    | w == 0x69 = return SetPropertyLate
    | w == 0x6A = fromU30LE_vl >>= return . DeleteProperty
    | w == 0x6B = return DeletePropertyLate
    | w == 0x6C = fromU30LE_vl >>= return . GetSlot
    | w == 0x6D = fromU30LE_vl >>= return . SetSlot
    | w == 0x6E = fromU30LE_vl >>= return . GetGlobalSlot
    | w == 0x6F = fromU30LE_vl >>= return . SetGlobalSlot
    | w == 0x70 = return ConvertString
    | w == 0x71 = return EscXmlElem
    | w == 0x72 = return EscXmlAttr
    | w == 0x73 = return ConvertInt
    | w == 0x74 = return ConvertUInt
    | w == 0x75 = return ConvertDouble
    | w == 0x76 = return ConvertBoolean
    | w == 0x77 = return ConvertObject
    | w == 0x78 = return CheckFilter
    {-| w == 0x79 = return convert_m-}
    {-| w == 0x7A = return convert_m_p-}
    {-| w == 0x7B = return-}
    {-| w == 0x7C = return-}
    {-| w == 0x7D = return-}
    {-| w == 0x7E = return-}
    {-| w == 0x7F = return-}
    | w == 0x80 = fromU30LE_vl >>= return . Coerce
    | w == 0x81 = return CoerceBoolean
    | w == 0x82 = return CoerceAny
    | w == 0x83 = return CoerceInt
    | w == 0x84 = return CoerceDouble
    | w == 0x85 = return CoerceString
    | w == 0x86 = fromU30LE_vl >>= return . AsType
    | w == 0x87 = return AsTypeLate
    | w == 0x88 = return CoerceUInt
    | w == 0x89 = return CoerceObject
    {-| w == 0x8A = return-}
    {-| w == 0x8B = return-}
    {-| w == 0x8C = return-}
    {-| w == 0x8D = return-}
    {-| w == 0x8E = return-}
    {-| w == 0x8F = return negate_p-}
    | w == 0x90 = return Negate
    | w == 0x91 = fromU30LE_vl >>= return . Increment
    | w == 0x92 = return IncLocal
    | w == 0x93 = return Decrement
    | w == 0x94 = fromU30LE_vl >>= return . DecLocal
    | w == 0x95 = return TypeOf
    | w == 0x96 = return Not
    | w == 0x97 = return BitNot
    {-| w == 0x98 = return-}
    {-| w == 0x99 = return-}
    | w == 0x9A = return Concat
    | w == 0x9B = return AddDouble
    {-| w == 0x9C = return increment_p-}
    {-| w == 0x9D = return inclocal_p-}
    {-| w == 0x9E = return decrement_p-}
    {-| w == 0x9F = return declocal_p-}
    | w == 0xA0 = return Add
    | w == 0xA1 = return Subtract
    | w == 0xA2 = return Multiply
    | w == 0xA3 = return Divide
    | w == 0xA4 = return Modulo
    | w == 0xA5 = return ShiftLeft
    | w == 0xA6 = return ShiftRight
    | w == 0xA7 = return ShiftRightUnsigned
    | w == 0xA8 = return BitAnd
    | w == 0xA9 = return BitOr
    | w == 0xAA = return BitXor
    | w == 0xAB = return Equals
    | w == 0xAC = return StrictEquals
    | w == 0xAD = return LessThan
    | w == 0xAE = return LessEquals
    | w == 0xAF = return GreaterThan
    | w == 0xB0 = return GreaterEquals
    | w == 0xB1 = return InstanceOf
    | w == 0xB2 = fromU30LE_vl >>= return . IsType
    | w == 0xB3 = return IsTypeLate
    | w == 0xB4 = return In
    {-| w == 0xB5 = return-}
    {-| w == 0xB6 = return-}
    {-| w == 0xB7 = return-}
    {-| w == 0xB8 = return-}
    {-| w == 0xB9 = return-}
    {-| w == 0xBA = return-}
    {-| w == 0xBB = return-}
    {-| w == 0xBC = return-}
    {-| w == 0xBD = return-}
    {-| w == 0xBE = return-}
    {-| w == 0xBF = return-}
    | w == 0xC0 = return IncrementInt
    | w == 0xC1 = return DecrementInt
    | w == 0xC2 = fromU30LE_vl >>= return . IncLocalInt
    | w == 0xC3 = fromU30LE_vl >>= return . DecLocalInt
    | w == 0xC4 = return NegateInt
    | w == 0xC5 = return AddInt
    | w == 0xC6 = return SubtractInt
    | w == 0xC7 = return MultiplyInt
    {-| w == 0xC8 = return-}
    {-| w == 0xC9 = return-}
    {-| w == 0xCA = return-}
    {-| w == 0xCB = return-}
    {-| w == 0xCC = return-}
    {-| w == 0xCD = return-}
    {-| w == 0xCE = return-}
    {-| w == 0xCF = return-}
    | w == 0xD0 = return GetLocal0
    | w == 0xD1 = return GetLocal1
    | w == 0xD2 = return GetLocal2
    | w == 0xD3 = return GetLocal3
    | w == 0xD4 = return SetLocal0
    | w == 0xD5 = return SetLocal1
    | w == 0xD6 = return SetLocal2
    | w == 0xD7 = return SetLocal3
    {-| w == 0xD8 = return-}
    {-| w == 0xD9 = return-}
    {-| w == 0xDA = return-}
    {-| w == 0xDB = return-}
    {-| w == 0xDC = return-}
    {-| w == 0xDD = return-}
    {-| w == 0xDE = return-}
    {-| w == 0xDF = return-}
    {-| w == 0xE0 = return-}
    {-| w == 0xE1 = return-}
    {-| w == 0xE2 = return-}
    {-| w == 0xE3 = return-}
    {-| w == 0xE4 = return-}
    {-| w == 0xE5 = return-}
    {-| w == 0xE6 = return-}
    {-| w == 0xE7 = return-}
    {-| w == 0xE8 = return-}
    {-| w == 0xE9 = return-}
    {-| w == 0xEA = return-}
    {-| w == 0xEB = return-}
    {-| w == 0xEC = return-}
    {-| w == 0xED = return-}
    {-| w == 0xEE = return abs_jump-}
    | w == 0xEF = do
        debugType <- fromU8
        index <- fromU30LE_vl
        reg <- fromU8
        extra <- fromU30LE_vl
        return $ Debug debugType index reg extra
    | w == 0xF0 = fromU30LE_vl >>= return . DebugLine
    | w == 0xF1 = fromU30LE_vl >>= return . DebugFile
    | w == 0xF2 = return BreakpointLine
    {-| w == 0xF3 = return timestamp-}
    {-| w == 0xF5 = return verifypass-}
    {-| w == 0xF6 = return alloc-}
    {-| w == 0xF7 = return mark-}
    {-| w == 0xF8 = return wb-}
    {-| w == 0xF9 = return prologue-}
    {-| w == 0xFA = return sendenter-}
    {-| w == 0xFB = return doubletoatom-}
    {-| w == 0xFC = return sweep-}
    {-| w == 0xFD = return codegenop-}
    {-| w == 0xFE = return verifyop-}
    {-| w == 0xFF = return decode-}

{-
    4.12
    exception
-}

parseException :: Parser Exception
parseException = do
    exFrom    <- fromU30LE_vl
    exTo      <- fromU30LE_vl
    exTarget  <- fromU30LE_vl
    exType    <- fromU30LE_vl
    exVarname <- fromU30LE_vl
    return Exception {
        exFrom = exFrom
      , exTo = exTo
      , exTarget = exTarget
      , exType = exType
      , exVarname = exVarname
    }


















