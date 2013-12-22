{-# LANGUAGE ScopedTypeVariables #-}
module Data.Amf.Deserialize (
  deserialize
, deserializeBs
, toValues
) where

import           Control.Monad (replicateM, liftM, liftM2)
import           Data.Amf.Def
import           Data.Amf.Deserialize.Def
import           Data.Amf.Deserialize.Util as U
import           Data.Binary.IEEE754 (wordToDouble)
import           Data.Bits
import           Data.Conduit
import           Data.Conduit.Binary as CB
import           Data.Conduit.List as CL
import           Data.Int (Int32)
import           Data.Word (Word8)
import           Util.Words
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified MonadLib as ML

-- | Resolve all references in the implicit dictionary
toValues :: ([Amf], Tables) -> [Amf]
toValues (amfs, tables) = Prelude.map (toValue tables) amfs

deserialize :: FilePath
            -> IO ([Amf], Tables)
            -- ^ A list of Amf and the implicit dictionary that was built up
deserialize file = (runResourceT $
  CB.sourceFile file $$ ML.runStateT emptyTables parseAmf) >>= return . reverseAmfs

deserializeBs :: BL.ByteString -> IO ([Amf], Tables)
deserializeBs lbs = (runResourceT $
  CB.sourceLbs lbs $$ ML.runStateT emptyTables parseAmf) >>= return . reverseAmfs

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
      | w == 0x00 = return AmfUndefined
      | w == 0x01 = return AmfNull
      | w == 0x02 = return AmfFalse
      | w == 0x03 = return AmfTrue
      | w == 0x04 = fromIntType
      | w == 0x05 = fromNumberType
      | w == 0x06 = fromStringType
      | w == 0x07 = fromXmlDocType
      | w == 0x08 = fail "Date unsupported"
      | w == 0x09 = fromArrayType
      | w == 0x0A = fromObjectType
      | w == 0x0B = fromXmlType
      | w == 0x0C = fromByteArray
      | w == 0x0D = fail "AmfVecInt unsupported"
      | w == 0x0E = fail "AmfVecUInt unsupported"
      | w == 0x0F = fail "AmfVecNumber unsupported"
      | w == 0x10 = fail "AmfVecObject unsupported"
      | w == 0x11 = fail "AmfDictionary unsupported"
      | otherwise = fail "encountered unknown marker"

-- AmfByteArray

fromByteArray :: Parser Amf
fromByteArray = refOrValue fromU29BRef fromU29BValue

fromU29BRef :: U29 -> Parser Amf
fromU29BRef = return . AmfByteArray . U29B_Ref

fromU29BValue :: U29 -> Parser Amf
fromU29BValue len =
  liftM (AmfByteArray . U29B_Value) $ U.take (fromIntegral len)

-- AmfXml and AmfXmlDoc

fromXmlType :: Parser Amf
fromXmlType = fromCommonXml AmfXml

fromXmlDocType :: Parser Amf
fromXmlDocType = fromCommonXml AmfXmlDoc

fromCommonXml :: (U29X -> Amf) -> Parser Amf
fromCommonXml = (flip liftM) fromAmf

-- AmfObject

fromObjectType :: Parser Amf
fromObjectType = fromAmf >>= u29OFunc

u29OFunc :: U29 -> Parser Amf
u29OFunc u29
  | {- U29O-ref        -} u29 .&. 0x1 == 0x0 = fromU29ORef u29
  | {- U29O-traits-ref -} u29 .&. 0x3 == 0x1 = fromU29OTraitsRef u29
  | {- U29O-traits-ext -} u29 .&. 0x7 == 0x7 = fromU29OTraitsExt u29
  | {- U29O-traits     -} u29 .&. 0x7 == 0x3 = fromU29OTraits u29
  | otherwise = fail$ "u29OFunc - unrecognized u29"

-- | Doc correction:
-- - "The remaining 1 to 28 significant bits are used to encode an object reference index (an integer)."
-- + "The remaining 1 to 27 significant bits are used to encode an object reference index (an integer)."
fromU29ORef :: U29 -> Parser Amf
fromU29ORef = return . AmfObject . U29O_Ref . (`shiftR` 2)

fromU29OTraitsRef :: U29 -> Parser Amf
fromU29OTraitsRef u29 = do
  U.getTT traitIdx >>= checkDynamic >>=
    U.pushCOT . AmfObject . uncurry (U29O_TraitsRef traitIdx)
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
  U.pushTT (className, props, u29 .&. 0x8 == 0x8) >>= checkDynamic >>=
    U.pushCOT . AmfObject . uncurry (U29O_Traits className)
  where
    fromUtf8Loop :: Parser UTF_8_vr
    fromUtf8Loop = do
      (utf8vr :: UTF_8_vr) <- fromAmf
      (str :: String) <- U.getString utf8vr
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


-- AmfArray

fromArrayType :: Parser Amf
fromArrayType = refOrValue fromU29ARef fromU29AValue

fromU29ARef :: U29 -> Parser Amf
fromU29ARef = return . AmfArray . U29A_Ref

fromU29AValue :: U29 -> Parser Amf
fromU29AValue len =
  liftM2 U29A_Value fromAssoc (replicateM len fromAmf) >>= U.pushCOT . AmfArray

fromAssoc :: Parser [Assoc_Value]
fromAssoc = do
  (key :: UTF_8_vr) <- fromAmf
  (key2 :: String) <- U.getString key
  if key2 == utf8_empty
    then return []
    else do
      (value :: Amf) <- fromAmf
      rest <- fromAssoc
      return $ (key, value):rest

-- Char

instance AmfPrim UTF_8_vr where
  fromAmf = do
    (u29 :: U29) <- fromAmf
    let lenOrIdx = u29 `shiftR` 1
    if u29 .&. 1 == 0
      {- ref - lenOrIdx used as index -}
      then return . U29S_Ref$ fromIntegral lenOrIdx
      {- value - lenOrIdx used as length -}
      else U.take (fromIntegral lenOrIdx) >>=
           return . BLC.unpack >>= U.pushST >>= return . U29S_Value

fromStringType :: Parser Amf
fromStringType = liftM AmfString fromAmf

utf8_empty :: String
utf8_empty = ""

-- Double

instance AmfPrim Double where
  fromAmf = U.take 8 >>= return . wordToDouble . toWord64 . BL.unpack

fromNumberType :: Parser Amf
fromNumberType = liftM AmfNumber fromAmf

-- Int

instance AmfPrim Int32 where
  fromAmf = liftM2 BL.append (U.takeWhile hasSignalBit) (U.take 1) >>=
    return . fromIntegral . fromU29 . Prelude.map fromIntegral . BL.unpack

instance AmfPrim Int where
  fromAmf = do (i :: Int32) <- fromAmf; return (fromIntegral i)

fromIntType :: Parser Amf
fromIntType = liftM AmfInt fromAmf

-- Util

refOrValue :: (Int -> Parser Amf) -- ^ ref
           -> (Int -> Parser Amf) -- ^ value
           -> Parser Amf
refOrValue ref value = do
  (u29 :: U29) <- fromAmf
  if u29 .&. 1 == 0
    then ref  $ u29 `shiftR` 1
    else value$ u29 `shiftR` 1
