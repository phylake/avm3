module ABC.Util
    (
      nWordsT
    , fromU16
    , fromU16LE
    , fromU32
    , fromU32LE
    , fromDouble
    , fromDoubleLE
    , fromU32LE_vl
    , fromU30LE_vl
    , fromS32LE_vl
    ) where

import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import Data.Int
import Data.Word
import Util.Words hiding
    (
      fromU16
    , fromU16LE
    , fromU32
    , fromU32LE
    , fromDouble
    , fromDoubleLE
    , fromU32LE_vl
    , fromU30LE_vl
    , fromS32LE_vl
    )
import qualified Util.Words as Util
    (
      fromU16
    , fromU16LE
    , fromU32
    , fromU32LE
    , fromDouble
    , fromDoubleLE
    , fromU32LE_vl
    , fromU30LE_vl
    , fromS32LE_vl
    )

nWordsT :: Int64 -> StateT ByteString IO [Word8]
nWordsT n = StateT $ return . nWords n

fromU16 :: StateT ByteString IO Word16
fromU16 = StateT $ return . Util.fromU16

fromU16LE :: StateT ByteString IO Word16
fromU16LE = StateT $ return . Util.fromU16LE

fromU32 :: StateT ByteString IO Word32
fromU32 = StateT $ return . Util.fromU32

fromU32LE :: StateT ByteString IO Word32
fromU32LE = StateT $ return . Util.fromU32LE

fromDouble :: StateT ByteString IO Double
fromDouble = StateT $ return . Util.fromDouble

fromDoubleLE :: StateT ByteString IO Double
fromDoubleLE = StateT $ return . Util.fromDoubleLE

fromU32LE_vl :: StateT ByteString IO Word32
fromU32LE_vl = StateT $ return . Util.fromU32LE_vl

fromU30LE_vl :: StateT ByteString IO Word32
fromU30LE_vl = StateT $ return . Util.fromU30LE_vl

fromS32LE_vl :: StateT ByteString IO Int32
fromS32LE_vl = StateT $ return . Util.fromS32LE_vl
