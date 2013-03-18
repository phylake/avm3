{-# LANGUAGE ScopedTypeVariables #-}
module Amf.Deserialize (allFromAmf) where

import           Amf.Def
import           Amf.Util as U
import           Control.Monad
import           Data.Binary.IEEE754 (wordToDouble)
import           Data.Bits
import           Data.Char (digitToInt, intToDigit)
import           Data.Enumerator as E hiding (replicateM)
import           Data.Enumerator.Binary as EB hiding (replicateM)
import           Data.Enumerator.List as EL hiding (replicateM)
import           Data.Int
import           Data.Word
import           Util.Misc
import           Util.Words
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified MonadLib as ML

{-

BEGIN test code

-}

testFile :: IO ()
testFile = do
  (amfs :: [Amf], (string, cot, tt)) <- run_ (EB.enumFile "amf/file.amf" $$ ML.runStateT nilTable allFromAmf)
  putStrLn "AMFS"
  putStrLn $ unlines $ Prelude.map show amfs
  putStrLn "TABLES"
  putStrLn "Strings"
  putStrLn $ unlines $ Prelude.map show string
  putStrLn "Complex Objects"
  putStrLn $ unlines $ Prelude.map show cot
  putStrLn "Traits"
  putStrLn $ unlines $ Prelude.map show tt
  return ()

p :: String -> ML.StateT Tables (Iteratee B.ByteString IO) ()
p = ML.lift . ML.lift . putStrLn

{-

END test code

-}

allFromAmf :: Parser [Amf]
allFromAmf = do
  w <- U.peek
  case w of
    Nothing -> return []
    otherwise -> do
      (amf :: Amf) <- fromAmf
      rest <- allFromAmf
      return$ amf:rest

instance AmfPrim Amf where
  fromAmf = U.head_ >>= fromAmfImpl where
    fromAmfImpl :: Word8 -> Parser Amf
    fromAmfImpl w
      | w == 0x0 = return AmfUndefined
      | w == 0x1 = return AmfNull
      | w == 0x2 = return AmfFalse
      | w == 0x3 = return AmfTrue
      | w == 0x4 = fromIntegerType
      | w == 0x5 = fromDoubleType
      | w == 0x6 = fromStringType
      | w == 0x7 = fromXmlDocType
      | w == 0x8 = fail "Date unsupported"
      | w == 0x9 = fromArrayType
      | w == 0xA = fromObjectType
      | w == 0xB = fromXmlType
      | w == 0xC = fromByteArray
      | otherwise = fail "encountered unknown marker"



--toAmfImpl :: ([Amf], Tables) -> Either AmfErr [Word8]
--toAmfImpl (AmfUndefined:amfs)       = undefined
--toAmfImpl (AmfNull:amfs)            = undefined
--toAmfImpl (AmfFalse:amfs)           = undefined
--toAmfImpl (AmfTrue:amfs)            = undefined
--toAmfImpl ((AmfInt u29):amfs)       = undefined
--toAmfImpl ((AmfDouble double):amfs) = undefined
--toAmfImpl ((AmfString utf8vr):amfs) = undefined
--toAmfImpl ((AmfXmlDoc string):amfs) = undefined
--toAmfImpl ((AmfDate string):amfs)   = undefined
--toAmfImpl ((AmfArray u29a):amfs)    = undefined
--toAmfImpl ((AmfObject u29o):amfs)   = undefined
--toAmfImpl ((AmfXml string):amfs)    = undefined
--toAmfImpl ((AmfByteArray ba):amfs)  = undefined

{-
  AmfByteArray

  U29O-ref |
  U29B-value U8*
-}

fromByteArray :: Parser Amf
fromByteArray = refOrValue fromU29BRef fromU29BValue

fromU29BRef :: U29 -> Parser Amf
fromU29BRef = return . AmfByteArray . U29B_Ref

fromU29BValue :: U29 -> Parser Amf
fromU29BValue len = U.take (fromIntegral len) >>=
                    return . BL.unpack >>= return . AmfByteArray . U29B_Value

{-
  AmfXml and AmfXmlDoc

  U29O-ref |
  U29X-value UTF8-char*
-}

fromXmlType :: Parser Amf
fromXmlType = fromCommonXml AmfXml

fromXmlDocType :: Parser Amf
fromXmlDocType = fromCommonXml AmfXmlDoc

fromCommonXml :: (U29X -> Amf) -> Parser Amf
fromCommonXml f = fromAmf >>= return . f

{-
  AmfObject

  (
    U29O-ref |
    U29O-traits-ref |
    U29O-traits-ext class-name U8* |
    U29O-traits class-name UTF-8-vr*
  )
  value-type* dynamic-member*
-}

fromObjectType :: Parser Amf
fromObjectType = fromAmf >>= u29OFunc
  
u29OFunc :: U29 -> Parser Amf
u29OFunc u29
  | {- U29O-ref        -} u29 .&. 0x1 == 0x0 = fromU29ORef u29
  | {- U29O-traits-ref -} u29 .&. 0x3 == 0x1 = fromU29OTraitsRef u29
  | {- U29O-traits-ext -} u29 .&. 0x7 == 0x7 = fromU29OTraitsExt u29
  | {- U29O-traits     -} u29 .&. 0x7 == 0x3 = fromU29OTraits u29
  | otherwise = fail$ "u29OFunc - unrecognized u29"

fromU29ORef :: U29 -> Parser Amf
fromU29ORef u29 = return . AmfObject . U29O_Ref$ u29 `shiftR` 1

fromU29OTraitsRef :: U29 -> Parser Amf
fromU29OTraitsRef u29 = do
  (traits :: [Traits]) <- getTT
  commonU29O (traits !! (u29 `shiftR` 2)) >>= return . AmfObject . U29O_TraitsRef

fromU29OTraitsExt :: U29 -> Parser Amf
fromU29OTraitsExt u29 = fail "fromU29OTraitsExt not implemented"

fromU29OTraits :: U29 -> Parser Amf
fromU29OTraits u29 = do
  (utf8vr :: UTF_8_vr) <- fromAmf
  (props :: [UTF_8_vr]) <- replicateM (u29 `shiftR` 4) fromUtf8Loop
  let traits = (utf8vr, props, u29 .&. 0x8 == 0x8)
  pushTT traits >>= commonU29O >>= return . AmfObject . U29O_Traits
  where
    fromUtf8Loop :: Parser UTF_8_vr
    fromUtf8Loop = do
      (utf8vr :: UTF_8_vr) <- fromAmf
      (str :: String) <- getString utf8vr
      if str == utf8_empty
        then fail "fromUtf8Loop - empty string"
        else return utf8vr

commonU29O :: Traits -> Parser [Assoc_Value]
commonU29O (_, props, dynamic) = if dynamic
  then liftM2 (++) (strictProps props) fromAssoc
  else strictProps props

strictProps :: [UTF_8_vr] -> Parser [Assoc_Value]
strictProps props = do
  (valueAmfs :: [Amf]) <- replicateM (Prelude.length props) fromAmf
  if Prelude.length valueAmfs /= Prelude.length props
    then fail$ "strictProps - length mismatch"
      ++ "\n\t props: " ++ show props
      ++ "\n\tvalues: " ++ show valueAmfs
    else return$ Prelude.zip props valueAmfs

{-
  AmfArray

    U29O-ref |
  (
    U29A-value
    (UTF-8-empty | assoc-value* UTF-8-empty)
    value-type*
  )
-}

fromArrayType :: Parser Amf
fromArrayType = refOrValue fromU29ARef fromU29AValue

fromU29ARef :: U29 -> Parser Amf
fromU29ARef = return . AmfArray . U29A_Ref

fromU29AValue :: U29 -> Parser Amf
fromU29AValue len = do
  assocValues <- fromAssoc
  (denseAmfs :: [Amf]) <- replicateM len fromAmf
  pushCOT$ AmfArray$ U29A_Value assocValues denseAmfs

fromAssoc :: Parser [Assoc_Value]
fromAssoc = do
  (key :: UTF_8_vr) <- fromAmf
  (key2 :: String) <- getString key
  if key2 == utf8_empty
    then return []
    else do
      (value :: Amf) <- fromAmf
      rest <- fromAssoc
      return$ (key, value):rest

{-
  Char
-}

instance AmfPrim UTF_8_vr where
  fromAmf = do
    (u29 :: U29) <- fromAmf
    let lenOrIdx = u29 `shiftR` 1
    if u29 .&. 1 == 0
      {- ref - lenOrIdx used as index -}
      then return . U29S_Ref$ fromIntegral lenOrIdx
      {- value - lenOrIdx used as length -}
      else U.take (fromIntegral lenOrIdx) >>=
           return . BLC.unpack >>= pushST >>= return . U29S_Value

fromStringType :: Parser Amf
fromStringType = fromAmf >>= return . AmfString

utf8_empty :: String
utf8_empty = ""

{-
  Double
-}

instance AmfPrim Double where
  fromAmf = U.take 8 >>= return . wordToDouble . toWord64 . BL.unpack

fromDoubleType :: Parser Amf
fromDoubleType = fromAmf >>= return . AmfDouble

{-
  Int
-}

instance AmfPrim Int where
  fromAmf = do
    w <- U.take 1
    ws <- U.takeWhile hasSignalBit
    return . fromIntegral . fromU29 $ BL.unpack (BL.append w ws)

fromIntegerType :: Parser Amf
fromIntegerType = fromAmf >>= return . AmfInt

{- the number of bytes an Int will occupy once serialized -}
--deserializedU29Length :: (Real a) => a -> Int
deserializedU29Length :: Int -> Int
deserializedU29Length x
  | x >= 0x00000000 && x <= 0x0000007f = 1
  | x >= 0x00000080 && x <= 0x00003fff = 2
  | x >= 0x00004000 && x <= 0x001fffff = 3
  | x >= 0x00200000 && x <= 0x3fffffff = 4
  | otherwise                          = 0

{-
  Tables
-}

getString :: UTF_8_vr -> Parser String
getString (U29S_Value v) = return v
getString (U29S_Ref u29) = fail "getString - not handling refs"

{-
getString :: UTF_8_vr -> [Amf] -> String
getString (U29S_Value v) as = v
getString (U29S_Ref u29) as = stringFromTable as u29

stringFromTable :: [Amf] -> Int -> String
stringFromTable amfs idx = table !! idx'
  where
    table = stringTable amfs
    idx' = (length table) - idx - 1

stringTable :: [Amf] -> [String]
stringTable ((AmfStack as):bs) = (stringTable as) ++ (stringTable bs)
stringTable ((AmfString (U29S_Value a)):as) = a : stringTable as
stringTable (a:as) = stringTable as
stringTable [] = []

complexObjectTable :: [Amf] -> [Amf]
complexObjectTable ((AmfArray (U29A_Value a b)):as) = AmfArray (U29A_Value a b) : complexObjectTable as
complexObjectTable (a:as) = complexObjectTable as
complexObjectTable [] = []

traitsTable :: [Amf] -> [Traits]
traitsTable ((AmfObject (U29O_Traits cls props _ [])):as) = (cls, props, False) : traitsTable as
traitsTable ((AmfObject (U29O_Traits cls props _  _)):as) = (cls, props, True) : traitsTable as
traitsTable (a:as) = traitsTable as
traitsTable [] = []
-}
{-
  Util
-}

toStrict :: BL.ByteString -> Parser B.ByteString
toStrict = return . B.pack . BL.unpack

pushST :: String -> Parser String
pushST a = do
  (st, cot, tt) <- ML.get
  ML.set (a : st, cot, tt)
  return a

pushCOT :: Amf -> Parser Amf
pushCOT a = do
  (st, cot, tt) <- ML.get
  ML.set (st, a : cot, tt)
  return a

pushTT :: Traits -> Parser Traits
pushTT a = do
  (st, cot, tt) <- ML.get
  ML.set (st, cot, a : tt)
  return a

getST :: Parser [String]
getST = ML.get >>= return . t31

getCOT :: Parser [Amf]
getCOT = ML.get >>= return . t32

getTT :: Parser [Traits]
getTT = ML.get >>= return . t33

nilTable :: Tables
nilTable = ([], [], [])

refOrValue :: (Int -> Parser Amf) -- ref
           -> (Int -> Parser Amf) -- value
           -> Parser Amf
refOrValue ref value = do
  (u29 :: U29) <- fromAmf
  if u29 .&. 1 == 0
    then ref  $ u29 `shiftR` 1
    else value$ u29 `shiftR` 1
