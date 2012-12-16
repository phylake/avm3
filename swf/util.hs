module Swf.Util (
    nWords
  , fromU8
  , fromU16LE
  , fromU32LE
  , fromDoubleLE
  , fromU32LE_vl
  , fromU30LE_vl
  , fromS32LE_vl
  , fromS24LE
  , unless_flag
  , rv_bool
  , rv_w32
  , rv_w16
  , rv_w8
) where

import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import Data.Int
import Data.Word
import Data.Bits
import Swf.Def
import Util.Misc
import Util.Words hiding
    (
      nWords
    , fromU8
    , fromU16LE
    , fromU32LE
    , fromDoubleLE
    , fromU32LE_vl
    , fromU30LE_vl
    , fromS32LE_vl
    , fromS24LE
    )
import qualified Util.Words as Util
    (
      nWords
    , fromU8
    , fromU16LE
    , fromU32LE
    , fromDoubleLE
    , fromU32LE_vl
    , fromU30LE_vl
    , fromS32LE_vl
    , fromS24LE
    )

nWords :: Int64 -> Parser [Word8]
nWords i = StateT $ return . Util.nWords i

fromU8 :: Parser Word8
fromU8 = StateT $ return . Util.fromU8

fromU16LE :: Parser Word16
fromU16LE = StateT $ return . Util.fromU16LE

fromU32LE :: Parser Word32
fromU32LE = StateT $ return . Util.fromU32LE

fromDoubleLE :: Parser Double
fromDoubleLE = StateT $ return . Util.fromDoubleLE

fromU32LE_vl :: Parser Word32
fromU32LE_vl = StateT $ return . Util.fromU32LE_vl

fromU30LE_vl :: Parser Word32
fromU30LE_vl = StateT $ return . Util.fromU30LE_vl

fromS32LE_vl :: Parser Int32
fromS32LE_vl = StateT $ return . Util.fromS32LE_vl

fromS24LE :: Parser Int32
fromS24LE = StateT $ return . Util.fromS24LE

unless_flag :: BitParser a -- false action
            -> BitParser a -- true action
            -> BitParser a
unless_flag false true = do
  bool <- rv_bool
  if bool then true else false

rv_bool :: BitParser Bool
rv_bool = do
  (p,ws) <- get
  let right = drop (floor$ p/8) ws
  let middle = take (floor$ (7 + p`mod`8 + 1) / 8) right
  let (w:[]) = shuffle_bits (floor$ (8 - p - 1) `mod` 8) middle
  put (p+1, ws)
  return $ w.&.1 == 1

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
rv_w8 w = rv_bits head mask w
  where
    mask :: Word8
    mask = 0xff `shiftR` (8 - fromIntegral w)

rv_bits :: (Bits a)
        => ([Word8] -> a)
        -> a -- mask
        -> Float
        -> BitParser a
rv_bits toWord mask w = do
  (p,ws) <- get
  let right = drop (floor$ p/8) ws
  let middle = take (floor$ (7 + p`mod`8 + w) / 8) right
  let value = shuffle_bits (floor$ (8 - p - w) `mod` 8) middle
  put (p+w, ws)
  return $ toWord value .&. mask
