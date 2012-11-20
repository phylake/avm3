module Swf.Util where

import Swf.Def
import Data.ByteString.Lazy (ByteString)
import Data.Int
import Data.Word
import Control.Monad.State
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

{-nWords :: Int64 -> ByteString -> ([Word8], ByteString)
nWords n bs = (BS.unpack $ BS.take n bs, BS.drop n bs)-}

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
