module Abc.Util (
  returnJ
, toStrict
, fromU8
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
) where

import Abc.Def
import Data.Enumerator as E
import Data.Enumerator.Binary as EB
import Data.Enumerator.List as EL
import Data.Int
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
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
import qualified Util.Words as Util
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

returnJ :: Monad m => a -> Iteratee b m (Maybe a)
returnJ = return . Just

toStrict :: BL.ByteString -> Parser B.ByteString
toStrict = return . B.pack . BL.unpack
{-# INLINE toStrict #-}

fromU8 :: Parser Word8
fromU8 = take' 1 >>= return . fst . Util.fromU8
{-# INLINE fromU8 #-}

fromU16 :: Parser Word16
fromU16 = take' 2 >>= return . fst . Util.fromU16
{-# INLINE fromU16 #-}

fromU16LE :: Parser Word16
fromU16LE = take' 2 >>= return . fst . Util.fromU16LE
{-# INLINE fromU16LE #-}

fromU32 :: Parser Word32
fromU32 = take' 4 >>= return . fst . Util.fromU32
{-# INLINE fromU32 #-}

fromU32LE :: Parser Word32
fromU32LE = take' 4 >>= return . fst . Util.fromU32LE
{-# INLINE fromU32LE #-}

fromDouble :: Parser Double
fromDouble = take' 8 >>= return . fst . Util.fromDouble
{-# INLINE fromDouble #-}

fromDoubleLE :: Parser Double
fromDoubleLE = take' 8 >>= return . fst . Util.fromDoubleLE
{-# INLINE fromDoubleLE #-}

fromU32LE_vl :: Parser Word32
fromU32LE_vl = do
  ws <- EB.takeWhile hasSignalBit >>= toStrict
  w <- EB.take 1 >>= toStrict
  return . fst $ Util.fromU32LE_vl $ B.append ws w
{-# INLINE fromU32LE_vl #-}

fromU30LE_vl :: Parser Word32
fromU30LE_vl = do
  ws <- EB.takeWhile hasSignalBit >>= toStrict
  w <- EB.take 1 >>= toStrict
  return . fst $ Util.fromU30LE_vl $ B.append ws w
{-# INLINE fromU30LE_vl #-}

fromS32LE_vl :: Parser Int32
fromS32LE_vl = do
  ws <- EB.takeWhile hasSignalBit >>= toStrict
  w <- EB.take 1 >>= toStrict
  return . Util.fromS32LE_vl . B.unpack $ B.append ws w
{-# INLINE fromS32LE_vl #-}

fromS24LE :: Parser Int32
--fromS24LE = EB.take 3 >>= return . Util.fromS24LE . BL.unpack
fromS24LE = do
  bs <- EB.take 3
  tryIO.putStrLn$ "ws " ++ show (BL.unpack bs)
  return . Util.fromS24LE $ BL.unpack bs
{-# INLINE fromS24LE #-}

take' :: Integer -> Parser B.ByteString
take' i = EB.take i >>= toStrict
