module ABC.Util where

import Control.Monad.State
import Data.Word
import Data.Int
import Util hiding ( fromU16
                   , fromU16LE
                   , fromU32
                   , fromU32LE
                   , fromDouble
                   , fromDoubleLE
                   , fromU32LE_vl
                   , fromU30LE_vl
                   , fromS32LE_vl)
import qualified Util ( fromU16
                      , fromU16LE
                      , fromU32
                      , fromU32LE
                      , fromDouble
                      , fromDoubleLE
                      , fromU32LE_vl
                      , fromU30LE_vl
                      , fromS32LE_vl)
import Data.ByteString.Lazy (ByteString)

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

{-fromU16 :: State ByteString Word16
fromU16 = state Util.fromU16

fromU16LE :: State ByteString Word16
fromU16LE = state Util.fromU16LE

fromU32 :: State ByteString Word32
fromU32 = state Util.fromU32

fromU32LE :: State ByteString Word32
fromU32LE = state Util.fromU32LE

fromDouble :: State ByteString Double
fromDouble = state Util.fromDouble

fromDoubleLE :: State ByteString Double
fromDoubleLE = state Util.fromDoubleLE

fromU32LE_vl :: State ByteString Word32
fromU32LE_vl = state Util.fromU32LE_vl

fromU30LE_vl :: State ByteString Word32
fromU30LE_vl = state Util.fromU30LE_vl-}
