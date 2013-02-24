module Abc.Deserialize (parseAbc) where

import Abc.Def
import Abc.Util
import Control.Monad
import Data.Bits
import Data.Enumerator as E
import Data.Enumerator.Binary as EB
import Data.Enumerator.List as EL
import Data.Int (Int32)
import Data.Word
import TFish
import Util.Misc (allBytes, replicateM')
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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC

testFile = run_ (EB.enumFile "abc/Test.abc" $$ parseAbc)

p :: String -> Parser ()
p = tryIO . putStrLn

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
  instances <- replicateM' parseInstance classCount
  classes <- replicateM' parseClass classCount

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
common hasOne f = fromU30LE_vl >>= u30' hasOne >>= replicateM' f
  where
    u30' :: Bool -> U30 -> Parser Int
    u30' hasOne u30
      | hasOne == True && u30 == 0 = return 0
      | hasOne == True && u30 == 1 = return 0
      | hasOne == True = return $ fromIntegral u30-1
      | otherwise = return $ fromIntegral u30

{-
  4.4
  String
-}

parseStrings :: Parser String
parseStrings = do
  u30 <- fromU30LE_vl
  EB.take (fromIntegral u30) >>= return . BLC.unpack

{-
  4.4.1
  Namespace
-}
parseNSInfos :: Parser NSInfo
parseNSInfos = do
  w <- fromU8
  fromU30LE_vl >>= return. nsInfoCons w
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
parseNSSets = fromU30LE_vl >>= replicateM' fromU30LE_vl

{-
  4.4.3
  Multiname
-}
parseMultinames :: Parser Multiname
parseMultinames = fromU8 >>= multinameChoice

multinameChoice :: Word8 -> Parser Multiname
multinameChoice w
  | w == cpc_QName = multinameDouble Multiname_QName
  | w == cpc_QNameA = multinameDouble Multiname_QNameA
  | w == cpc_RTQName = liftM Multiname_RTQName fromU30LE_vl
  | w == cpc_RTQNameA = liftM Multiname_RTQNameA fromU30LE_vl
  | w == cpc_RTQNameL = return Multiname_RTQNameL
  | w == cpc_RTQNameLA = return Multiname_RTQNameLA
  | w == cpc_Multiname = multinameDouble Multiname_Multiname
  | w == cpc_MultinameA = multinameDouble Multiname_MultinameA
  | w == cpc_MultinameL = liftM Multiname_MultinameL fromU30LE_vl
  | w == cpc_MultinameLA = liftM Multiname_MultinameLA fromU30LE_vl

multinameDouble :: (Word32 -> Word32 -> Multiname) -> Parser Multiname
multinameDouble f = do
  nameOrNamespace <- fromU30LE_vl
  nameOrSet <- fromU30LE_vl
  return$ f nameOrNamespace nameOrSet

{-
  4.5
  Method signature
-}
parseMethodSignatures :: Parser MethodSignature
parseMethodSignatures = do
  paramCount <- fromU30LE_vl
  returnType <- fromU30LE_vl
  paramTypes <- replicateM' fromU30LE_vl paramCount
  name <- fromU30LE_vl
  flags <- fromU8
  optionInfo <- if (flags .&. msflag_HAS_OPTIONAL == msflag_HAS_OPTIONAL)
    then parseOptionalParams {- TODO checkout count abcdump.as:717 -}
    else return Nothing
  paramNames <- if (flags .&. msflag_HAS_PARAM_NAMES == msflag_HAS_PARAM_NAMES)
    then parseParamNames paramCount
    else return Nothing
  return MethodSignature {
    msReturnType = returnType
  , msParamTypes = paramTypes
  , msMethodName = name
  , msFlags = flags
  , msOptionInfo = optionInfo
  , msParamNames = paramNames
  }

{-
  4.5.1
  Optional parameters
-}

parseOptionalParams :: Parser (Maybe [CPC])
parseOptionalParams =
  fromU30LE_vl >>= replicateM' optionDetail >>= returnJ

optionDetail :: Parser CPC
optionDetail = do
  val <- fromU30LE_vl
  kind <- fromU8
  return$ cpcChoice kind val

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
  replicateM' fromU30LE_vl count >>= returnJ

{-
  4.6
  metadata
-}

parseMetadata :: Parser Metadata
parseMetadata = do
  name <- fromU30LE_vl
  pairs <- fromU30LE_vl
  keys <- replicateM' fromU30LE_vl pairs
  values <- replicateM' fromU30LE_vl pairs
  return$ Metadata name$ Prelude.zip keys values

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
    then fromU30LE_vl >>= returnJ
    else return Nothing
  interfaces <- fromU30LE_vl >>= replicateM' fromU30LE_vl
  iinit <- fromU30LE_vl
  traits <- fromU30LE_vl >>= replicateM' parseTrait
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
  let final = kind .&. 0x10 == 0x10
  let override = kind .&. 0x20 == 0x20
  meta <- if (kind .&. 0x40 == 0x40)
    then fromU30LE_vl >>= replicateM' fromU30LE_vl >>= returnJ
    else return Nothing
  return TraitsInfo {
    tiName = name
  , tiFinal = final
  , tiOverride = override
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
    else fromU8 >>= returnJ
  return$ f TraitVar {
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
parseTraitClass f = liftM f$ liftM2 TraitClass fromU30LE_vl fromU30LE_vl

{-
  4.8.4
  trait function
-}

parseTraitFunction :: (TraitFunction -> TraitType) -> Parser TraitType
parseTraitFunction f = liftM f$ liftM2 TraitFunction fromU30LE_vl fromU30LE_vl

{-
  4.8.5
  trait method
-}

parseTraitMethod :: (TraitMethod -> TraitType) -> Parser TraitType
parseTraitMethod f = liftM f$ liftM2 TraitMethod fromU30LE_vl fromU30LE_vl

{-
  4.9
  class info
-}

parseClass :: Parser ClassInfo
parseClass = do
  ciInit <- fromU30LE_vl
  ciTraits <- fromU30LE_vl >>= replicateM' parseTrait
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
  siTraits <- fromU30LE_vl >>= replicateM' parseTrait
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
  mbCodeCount <- fromU30LE_vl
  opcodeBytes <- EB.take (fromIntegral mbCodeCount)
  mbCode <- tryIO$ run_ (enumList 1 (BL.toChunks opcodeBytes) $$ parseOpCode)
  mbExceptions <- fromU30LE_vl >>= replicateM' parseException
  mbTraits <- fromU30LE_vl >>= replicateM' parseTrait
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

parseOpCode :: Parser [OpCode]
parseOpCode = do
  w <- E.peek
  case w of
    Nothing -> return []
    otherwise -> do
      op <- fromU8 >>= parseOpCodeChoice
      ops <- parseOpCode
      return (op:ops)

parseOpCodeChoice :: Word8 -> Parser OpCode
parseOpCodeChoice w
  | w == 0x01 = return Breakpoint
  | w == 0x02 = return Nop
  | w == 0x03 = return Throw
  | w == 0x04 = liftM GetSuper fromU30LE_vl
  | w == 0x05 = liftM SetSuper fromU30LE_vl
  | w == 0x06 = liftM DefaultXmlNamespace fromU30LE_vl
  | w == 0x07 = return DefaultXmlNamespaceL
  | w == 0x08 = liftM Kill fromU30LE_vl
  | w == 0x09 = return Label
  {-| w == 0x0A = return-}
  {-| w == 0x0B = return-}
  | w == 0x0C = liftM IfNotLessThan fromS24LE
  | w == 0x0D = liftM IfNotLessEqual fromS24LE
  | w == 0x0E = liftM IfNotGreaterThan fromS24LE
  | w == 0x0F = liftM IfNotGreaterEqual fromS24LE
  | w == 0x10 = liftM Jump fromS24LE
  | w == 0x11 = liftM IfTrue fromS24LE
  | w == 0x12 = liftM IfFalse fromS24LE
  | w == 0x13 = liftM IfEqual fromS24LE
  | w == 0x14 = liftM IfNotEqual fromS24LE
  | w == 0x15 = liftM IfLessThan fromS24LE
  | w == 0x16 = liftM IfLessEqual fromS24LE
  | w == 0x17 = liftM IfGreaterThan fromS24LE
  | w == 0x18 = liftM IfGreaterEqual fromS24LE
  | w == 0x19 = liftM IfStrictEqual fromS24LE
  | w == 0x1A = liftM IfStrictNotEqual fromS24LE
  | w == 0x1B = do
    defaultOffset <- fromS24LE
    caseOffsets <- fromU30LE_vl >>= replicateM' fromS24LE
    return$ LookupSwitch defaultOffset caseOffsets
  | w == 0x1C = return PushWith
  | w == 0x1D = return PopScope
  | w == 0x1E = return NextName
  | w == 0x1F = return HasNext
  | w == 0x20 = return PushNull
  | w == 0x21 = return PushUndefined
  | w == 0x22 = return PushConstant
  | w == 0x23 = return NextValue
  | w == 0x24 = liftM PushByte fromU8
  | w == 0x25 = liftM PushShort fromU30LE_vl
  | w == 0x26 = return PushTrue
  | w == 0x27 = return PushFalse
  | w == 0x28 = return PushNaN
  | w == 0x29 = return Pop
  | w == 0x2A = return Dup
  | w == 0x2B = return Swap
  | w == 0x2C = liftM PushString fromU30LE_vl
  | w == 0x2D = liftM PushInt fromU30LE_vl
  | w == 0x2E = liftM PushUInt fromU30LE_vl
  | w == 0x2F = liftM PushDouble fromU30LE_vl
  | w == 0x30 = return PushScope
  | w == 0x31 = liftM PushNamespace fromU30LE_vl
  | w == 0x32 = return$ HasNext2 0 0
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
  | w == 0x40 = liftM NewFunction fromU30LE_vl
  | w == 0x41 = liftM Call fromU30LE_vl
  | w == 0x42 = liftM Construct fromU30LE_vl
  | w == 0x43 = liftM2 CallMethod fromU30LE_vl fromU30LE_vl
  | w == 0x44 = liftM2 CallStatic fromU30LE_vl fromU30LE_vl
  | w == 0x45 = liftM2 CallSuper fromU30LE_vl fromU30LE_vl
  | w == 0x46 = liftM2 CallProperty fromU30LE_vl fromU30LE_vl
  | w == 0x47 = return ReturnVoid
  | w == 0x48 = return ReturnValue
  | w == 0x49 = liftM ConstructSuper fromU30LE_vl
  | w == 0x4A = liftM2 ConstructProp fromU30LE_vl fromU30LE_vl
  | w == 0x4B = return CallSuperId
  | w == 0x4C = liftM2 CallPropLex fromU30LE_vl fromU30LE_vl
  | w == 0x4D = return CallInterface
  | w == 0x4E = liftM2 CallSuperVoid fromU30LE_vl fromU30LE_vl
  | w == 0x4F = liftM2 CallPropVoid fromU30LE_vl fromU30LE_vl
  {-| w == 0x50 = return Sign1-}
  {-| w == 0x51 = return Sign8-}
  {-| w == 0x52 = return Sign16-}
  | w == 0x53 = return ApplyType
  | w == 0x55 = liftM NewObject fromU30LE_vl
  | w == 0x56 = liftM NewArray fromU30LE_vl
  | w == 0x57 = return NewActivation
  | w == 0x58 = liftM NewClass fromU30LE_vl
  | w == 0x59 = liftM GetDescendants fromU30LE_vl
  | w == 0x5A = liftM NewCatch fromU30LE_vl
  | w == 0x5B = return FindPropGlobalStrict
  | w == 0x5C = return FindPropGlobal
  | w == 0x5D = liftM FindPropStrict fromU30LE_vl
  | w == 0x5E = liftM FindProperty fromU30LE_vl
  | w == 0x5F = return FindDef
  | w == 0x60 = liftM GetLex fromU30LE_vl
  | w == 0x61 = liftM SetProperty fromU30LE_vl
  | w == 0x62 = liftM GetLocal fromU30LE_vl
  | w == 0x63 = liftM SetLocal fromU30LE_vl
  | w == 0x64 = return GetGlobalScope
  | w == 0x65 = liftM GetScopeObject fromU8
  | w == 0x66 = liftM GetProperty fromU30LE_vl
  | w == 0x67 = return GetPropertyLate
  | w == 0x68 = liftM InitProperty fromU30LE_vl
  | w == 0x69 = return SetPropertyLate
  | w == 0x6A = liftM DeleteProperty fromU30LE_vl
  | w == 0x6B = return DeletePropertyLate
  | w == 0x6C = liftM GetSlot fromU30LE_vl
  | w == 0x6D = liftM SetSlot fromU30LE_vl
  | w == 0x6E = liftM GetGlobalSlot fromU30LE_vl
  | w == 0x6F = liftM SetGlobalSlot fromU30LE_vl
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
  | w == 0x80 = liftM Coerce fromU30LE_vl
  | w == 0x81 = return CoerceBoolean
  | w == 0x82 = return CoerceAny
  | w == 0x83 = return CoerceInt
  | w == 0x84 = return CoerceDouble
  | w == 0x85 = return CoerceString
  | w == 0x86 = liftM AsType fromU30LE_vl
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
  | w == 0x91 = return Increment
  | w == 0x92 = return IncLocal
  | w == 0x93 = return Decrement
  | w == 0x94 = liftM DecLocal fromU30LE_vl
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
  | w == 0xB2 = liftM IsType fromU30LE_vl
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
  | w == 0xC2 = liftM IncLocalInt fromU30LE_vl
  | w == 0xC3 = liftM DecLocalInt fromU30LE_vl
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
    return$ Debug debugType index reg extra
  | w == 0xF0 = liftM DebugLine fromU30LE_vl
  | w == 0xF1 = liftM DebugFile fromU30LE_vl
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


















