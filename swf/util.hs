{-# LANGUAGE ScopedTypeVariables #-}
module Swf.Util (
    nWords
  , readU8
  , readU16LE
  , readFixed8
  , readU32LE
  , readDoubleLE
  , readU32LE_vl
  , readU30LE_vl
  , readS32LE_vl
  , readS24LE
  , unless_flag
  , rv_bool
  , rv_w32
  , rv_w16
  , rv_w8
  , toStrict
) where

import           Control.Monad.Identity
import           Data.Bits
import           Data.Enumerator as E
import           Data.Enumerator.Binary as EB
import           Data.Enumerator.List as EL
import           Data.Int
import           Data.Word
import           Swf.Def
import           Util.Misc
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified MonadLib as ML
import qualified Util.Words as U

toStrict :: BSL.ByteString -> Parser BS.ByteString
toStrict = return. BS.pack. BSL.unpack

nWords :: Integer -> Parser [Word8]
nWords i = EB.take i >>= return. BSL.unpack

readU8 :: Parser Word8
readU8 = nWords 1 >>= return. Prelude.head

readU16LE :: Parser Word16
readU16LE = nWords 2 >>= return. U.toWord16LE

readFixed8 :: Parser Float
readFixed8 = do
  (w1:w2:[]) <- nWords 2
  let (decimal :: Float) = fromIntegral w1 / maxDecimalPrecision
  return$ (fromIntegral w2) + decimal
  where
    maxDecimalPrecision :: Float
    maxDecimalPrecision = 10 ** ceiling(logBase 10 $ 2 ** 8)

readU32LE :: Parser Word32
readU32LE = nWords 4 >>= return. U.toWord32LE

readDoubleLE :: Parser Double
readDoubleLE = nWords 8 >>= return. U.toDouble. Prelude.reverse

readU32LE_vl :: Parser Word32
readU32LE_vl = varLenUintLE >>= return. fromIntegral

readU30LE_vl :: Parser Word32
readU30LE_vl = do
  w32 <- readU32LE_vl
  return$ w32 .&. 0x3fffffff

readS24LE :: Parser Int32
readS24LE = do
  bs <- EB.take 3 >>= toStrict
  let (ret,_) = U.fromS24LE bs
  return ret

readS32LE_vl :: Parser Int32
readS32LE_vl = do
  bs <- varIntBS
  return. fromIntegral. U.fromS32LE_vl_impl$ Prelude.map fromIntegral$ BS.unpack bs

varLenUintLE :: Parser Word64
varLenUintLE = varIntBS >>= return. fst. U.varLenUintLE

varIntBS :: Parser BS.ByteString
varIntBS = EB.takeWhile U.hasSignalBit >>= toStrict

unless_flag :: BitParser a -- false action
            -> BitParser a -- true action
            -> BitParser a
unless_flag false true = do
  bool <- rv_bool
  if bool then true else false

rv_bool :: BitParser Bool
rv_bool = do
  (p,ws) <- ML.get
  let right = Prelude.drop (floor$ p/8) ws
  let middle = Prelude.take (floor$ (7 + p`mod`8 + 1) / 8) right
  let (w:[]) = U.shuffle_bits (floor$ (8 - p - 1) `mod` 8) middle
  ML.set (p+1, ws)
  return $ w.&.1 == 1

rv_w32 :: Float -> BitParser Word32
rv_w32 w = rv_bits U.toWord32 mask w
  where
    mask :: Word32
    mask = 0xffffffff `shiftR` (32 - fromIntegral w)

rv_w16 :: Float -> BitParser Word16
rv_w16 w = rv_bits U.toWord16 mask w
  where
    mask :: Word16
    mask = 0xffff `shiftR` (16 - fromIntegral w)

rv_w8 :: Float -> BitParser Word8
rv_w8 w = rv_bits Prelude.head mask w
  where
    mask :: Word8
    mask = 0xff `shiftR` (8 - fromIntegral w)

rv_bits :: (Bits a)
        => ([Word8] -> a)
        -> a -- mask
        -> Float
        -> BitParser a
rv_bits toWord mask w = do
  (p,ws) <- ML.get
  let right = Prelude.drop (floor$ p/8) ws
  let middle = Prelude.take (floor$ (7 + p`mod`8 + w) / 8) right
  let value = U.shuffle_bits (floor$ (8 - p - w) `mod` 8) middle
  ML.set (p+w, ws)
  return $ toWord value .&. mask
