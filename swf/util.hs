{-# LANGUAGE ScopedTypeVariables #-}
module Swf.Util (
    takeWords
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
import           Data.Conduit
import           Data.Conduit.Binary as CB
import           Data.Conduit.List as CL
import           Data.Int
import           Data.Void
import           Data.Word
import           Swf.Def
import           Util.Misc
import           Util.Words
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified MonadLib as ML

toStrict :: BL.ByteString -> Parser B.ByteString
toStrict = return . B.pack . BL.unpack

takeWords :: Int -> Parser [Word8]
takeWords i = CB.take i >>= return . BL.unpack

readU8 :: Parser Word8
readU8 = takeWords 1 >>= return . Prelude.head

readU16LE :: Parser Word16
readU16LE = takeWords 2 >>= return . toWord16LE

readFixed8 :: Parser Float
readFixed8 = do
  (w1:w2:[]) <- takeWords 2
  let (decimal :: Float) = fromIntegral w1 / maxDecimalPrecision
  return $ (fromIntegral w2) + decimal
  where
    maxDecimalPrecision :: Float
    maxDecimalPrecision = 10 ** ceiling(logBase 10 $ 2 ** 8)

readU32LE :: Parser Word32
readU32LE = takeWords 4 >>= return . toWord32LE

readDoubleLE :: Parser Double
readDoubleLE = takeWords 8 >>= return . toDouble . Prelude.reverse

readU32LE_vl :: Parser Word32
readU32LE_vl = CB.takeWhile hasSignalBit =$ CL.consume >>=
  return . fromU32LE_vl . B.unpack . Prelude.foldl B.append B.empty

readU30LE_vl :: Parser Word32
readU30LE_vl = readU32LE_vl >>= return . (.&. 0x3fffffff)

readS24LE :: Parser Int32
readS24LE = CB.take 3 >>= toStrict >>= return . fromS24LE . B.unpack

readS32LE_vl :: Parser Int32
readS32LE_vl = CB.takeWhile hasSignalBit =$= CL.consume >>=
  return . fromS32LE_vl . B.unpack . Prelude.foldl B.append B.empty

unless_flag :: BitParser a -- false action
            -> BitParser a -- true action
            -> BitParser a
unless_flag false true = do
  bool <- rv_bool
  if bool then true else false

rv_w32 :: Float -> BitParser Word32
rv_w32 w = rv_bits toWord32 mask w
  where
    mask :: Word32
    mask = 0xffffffff `shiftR` (32 - fromIntegral w)

rv_w16 :: Float -> BitParser Word16
rv_w16 w = rv_bits toWord16 mask w
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
  let value = shuffle_bits (floor$ (8 - p - w) `mod` 8) middle
  ML.set (p+w, ws)
  return $ toWord value .&. mask

-- TODO use rv_bits
rv_bool :: BitParser Bool
rv_bool = do
  (p,ws) <- ML.get
  let right = Prelude.drop (floor$ p/8) ws
  let middle = Prelude.take (floor$ (7 + p`mod`8 + 1) / 8) right
  let (w:[]) = shuffle_bits (floor$ (8 - p - 1) `mod` 8) middle
  ML.set (p+1, ws)
  return $ w.&.1 == 1
