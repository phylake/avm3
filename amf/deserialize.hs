{-# LANGUAGE ScopedTypeVariables #-}
module Amf.Deserialize (deserialize, deserializeBs, toValues) where

import           Amf.Def
import           Amf.Util as U
import           Control.Monad (replicateM, liftM2)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Binary.IEEE754 (wordToDouble)
import           Data.Bits
import           Data.Char (digitToInt, intToDigit)
import           Data.Conduit
import           Data.Conduit.Binary as CB
import           Data.Int (Int32)
import           Data.Word
import           Util.Misc
import           Util.Words
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified MonadLib as ML

reverseAmfs :: ([Amf], Tables) -> IO ([Amf], Tables)
reverseAmfs (amfs, (st, cot, tt)) =
  return (reverse amfs, (reverse st, reverse cot, reverse tt))

-- resolve all references
toValues :: ([Amf], Tables) -> ([Amf], Tables)
toValues = undefined

deserialize :: FilePath -> IO ([Amf], Tables)
deserialize file = (runResourceT $
  CB.sourceFile file $$ ML.runStateT nilTable parseAmf) >>= reverseAmfs

deserializeBs :: BL.ByteString -> IO ([Amf], Tables)
deserializeBs lbs = (runResourceT $
  CB.sourceLbs lbs $$ ML.runStateT nilTable parseAmf) >>= reverseAmfs

parseAmf :: Parser [Amf]
parseAmf = do
  w <- U.peek
  case w of
    Nothing -> return []
    otherwise -> do
      (amf :: Amf) <- fromAmf
      rest <- parseAmf
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
                    return . AmfByteArray . U29B_Value . BL.unpack

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

{-
  Doc correction:
  - "The remaining 1 to 28 significant bits are used to encode an object reference index (an integer)."
  + "The remaining 1 to 27 significant bits are used to encode an object reference index (an integer)."
-}
fromU29ORef :: U29 -> Parser Amf
fromU29ORef = return . AmfObject . U29O_Ref . (`shiftR` 2)

fromU29OTraitsRef :: U29 -> Parser Amf
fromU29OTraitsRef u29 = do
  getTT traitIdx >>= checkDynamic >>=
    pushCOT . AmfObject . uncurry (U29O_TraitsRef traitIdx)
  where
    traitIdx = u29 `shiftR` 2

    checkDynamic :: Traits -> Parser ([Amf], [Assoc_Value])
    checkDynamic (_, props, True) =
      liftM2 (,) (replicateM (Prelude.length props) fromAmf) fromAssoc
    checkDynamic (_, props, False) =
      liftM2 (,) (replicateM (Prelude.length props) fromAmf) (return [])

fromU29OTraitsExt :: U29 -> Parser Amf
fromU29OTraitsExt u29 = fail "fromU29OTraitsExt not implemented"

fromU29OTraits :: U29 -> Parser Amf
fromU29OTraits u29 = do
  (className :: UTF_8_vr) <- fromAmf
  (props :: [UTF_8_vr]) <- replicateM (u29 `shiftR` 4) fromUtf8Loop
  pushTT (className, props, u29 .&. 0x8 == 0x8) >>= checkDynamic >>=
    pushCOT . AmfObject . uncurry (U29O_Traits className)
  where
    fromUtf8Loop :: Parser UTF_8_vr
    fromUtf8Loop = do
      (utf8vr :: UTF_8_vr) <- fromAmf
      (str :: String) <- getString utf8vr
      if str == utf8_empty
        then fail "fromUtf8Loop - empty string"
        else return utf8vr
    
    checkDynamic :: Traits -> Parser ([Assoc_Value], [Assoc_Value])
    checkDynamic (_, props, True) = liftM2 (,) (strictProps props) fromAssoc
    checkDynamic (_, props, False) = liftM2 (,) (strictProps props) (return [])

    strictProps :: [UTF_8_vr] -> Parser [Assoc_Value]
    strictProps props = do
      (valueAmfs :: [Amf]) <- replicateM (Prelude.length props) fromAmf
      if Prelude.length valueAmfs /= Prelude.length props
        then fail $ "strictProps - length mismatch"
          ++ "\n\t props: " ++ show props
          ++ "\n\tvalues: " ++ show valueAmfs
        else return $ Prelude.zip props valueAmfs


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
fromU29AValue len =
  liftM2 U29A_Value fromAssoc (replicateM len fromAmf) >>= pushCOT . AmfArray

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

instance AmfPrim Int32 where
  fromAmf = liftM2 BL.append (U.take 1) (U.takeWhile hasSignalBit) >>=
    return . fromIntegral . fromU29 . BL.unpack

instance AmfPrim Int where
  fromAmf = do (i :: Int32) <- fromAmf; return (fromIntegral i)

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
getString (U29S_Ref u29) = getST u29

pushST :: String -> Parser String
pushST "" = return ""
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

getST :: Int -> Parser String
getST i = do
  (st, _, _) <- ML.get
  return $ st !! (length st - i - 1)

getCOT :: Int -> Parser Amf
getCOT i = do
  (_, cot, _) <- ML.get
  return $ cot !! (length cot - i - 1)

getTT :: Int -> Parser Traits
getTT i = do
  (_, _, tt) <- ML.get
  return $ tt !! (length tt - i - 1)

nilTable :: Tables
nilTable = ([], [], [])

{-
  Util
-}

toStrict :: BL.ByteString -> Parser B.ByteString
toStrict = return . B.pack . BL.unpack

refOrValue :: (Int -> Parser Amf) -- ref
           -> (Int -> Parser Amf) -- value
           -> Parser Amf
refOrValue ref value = do
  (u29 :: U29) <- fromAmf
  if u29 .&. 1 == 0
    then ref  $ u29 `shiftR` 1
    else value$ u29 `shiftR` 1

{-
BEGIN test code
-}

test :: IO ()
test = do
  (amfs :: [Amf], (string, cot, tt)) <- deserialize "amf/file.amf"
  putStrLn "AMFS"
  putStrLn $ unlines $ Prelude.map show amfs
  putStrLn "TABLES - STRINGS"
  putStrLn $ unlines $ Prelude.map show string
  putStrLn "TABLES - COMPLEX OBJECTS"
  putStrLn $ unlines $ Prelude.map show cot
  putStrLn "TABLES - TRAITS"
  putStrLn $ unlines $ Prelude.map show tt
  return ()

p :: String -> Parser ()
p = ML.lift . lift . lift . putStrLn

{-
END test code
-}
