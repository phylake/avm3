module ABC.Util where

import ABC.Def
import Data.ByteString.Lazy (ByteString)
import Data.Int
import Data.Word
import Control.Monad.State
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

fromU8 :: Parser Word8
fromU8 = StateT $ return . Util.fromU8
{-# INLINE fromU8 #-}

fromU16 :: Parser Word16
fromU16 = StateT $ return . Util.fromU16
{-# INLINE fromU16 #-}

fromU16LE :: Parser Word16
fromU16LE = StateT $ return . Util.fromU16LE
{-# INLINE fromU16LE #-}

fromU32 :: Parser Word32
fromU32 = StateT $ return . Util.fromU32
{-# INLINE fromU32 #-}

fromU32LE :: Parser Word32
fromU32LE = StateT $ return . Util.fromU32LE
{-# INLINE fromU32LE #-}

fromDouble :: Parser Double
fromDouble = StateT $ return . Util.fromDouble
{-# INLINE fromDouble #-}

fromDoubleLE :: Parser Double
fromDoubleLE = StateT $ return . Util.fromDoubleLE
{-# INLINE fromDoubleLE #-}

fromU32LE_vl :: Parser Word32
fromU32LE_vl = StateT $ return . Util.fromU32LE_vl
{-# INLINE fromU32LE_vl #-}

fromU30LE_vl :: Parser Word32
fromU30LE_vl = StateT $ return . Util.fromU30LE_vl
{-# INLINE fromU30LE_vl #-}

fromS32LE_vl :: Parser Int32
fromS32LE_vl = StateT $ return . Util.fromS32LE_vl
{-# INLINE fromS32LE_vl #-}

fromS24LE :: Parser Int32
fromS24LE = StateT $ return . Util.fromS24LE
{-# INLINE fromS24LE #-}
