{-# LANGUAGE ScopedTypeVariables #-}
module Data.Amf.Serialize (serialize) where

import           Control.Monad (replicateM, liftM2)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Amf.Def
--import           Data.Amf.Util as U
import           Data.Binary.IEEE754 (doubleToWord)
import           Data.Bits
import           Data.Char (digitToInt, intToDigit)
import           Data.Conduit
import           Data.Conduit.Binary as CB
import           Data.Word
import           Data.Int (Int32)
import           Util.Words
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified MonadLib as ML

type Serializer = ML.StateT Tables IO BL.ByteString

serialize :: [Amf] -> IO (BL.ByteString, Tables)
serialize [] = return (BL.empty, emptyTables)
serialize amfs = ML.runStateT emptyTables $ loop amfs

loop :: [Amf] -> Serializer
loop (AmfUndefined:amfs)          = return BL.empty   >>= append 0x00 (loop amfs)
loop (AmfNull:amfs)               = return BL.empty   >>= append 0x01 (loop amfs)
loop (AmfFalse:amfs)              = return BL.empty   >>= append 0x02 (loop amfs)
loop (AmfTrue:amfs)               = return BL.empty   >>= append 0x03 (loop amfs)
loop (amf@(AmfInt _):amfs)        = amfInt amf        >>= append 0x04 (loop amfs)
loop (amf@(AmfNumber _):amfs)     = amfNumber amf     >>= append 0x05 (loop amfs)
loop (amf@(AmfString _):amfs)     = amfString amf     >>= append 0x06 (loop amfs)
loop (amf@(AmfXmlDoc _):amfs)     = amfXmlDoc amf     >>= append 0x07 (loop amfs)
loop (amf@(AmfDate _):amfs)       = amfDate amf       >>= append 0x08 (loop amfs)
loop (amf@(AmfArray _):amfs)      = amfArray amf      >>= append 0x09 (loop amfs)
loop (amf@(AmfObject _):amfs)     = amfObject amf     >>= append 0x0A (loop amfs)
loop (amf@(AmfXml _):amfs)        = amfXml amf        >>= append 0x0B (loop amfs)
loop (amf@(AmfByteArray _):amfs)  = amfByteArray amf  >>= append 0x0C (loop amfs)
loop (amf@(AmfVecInt _):amfs)     = amfVecInt amf     >>= append 0x0D (loop amfs)
loop (amf@(AmfVecUInt _):amfs)    = amfVecUInt amf    >>= append 0x0E (loop amfs)
loop (amf@(AmfVecNumber _):amfs)  = amfVecNumber amf  >>= append 0x0F (loop amfs)
loop (amf@(AmfVecObject _):amfs)  = amfVecObject amf  >>= append 0x10 (loop amfs)
loop (amf@(AmfDictionary _):amfs) = amfDictionary amf >>= append 0x11 (loop amfs)
loop []                           = return BLC.empty

append :: Word8 -> Serializer -> BLC.ByteString -> Serializer
append w s b = s >>= \sb -> return $ BL.concat [BL.singleton w, b, sb]

amfInt :: Amf -> Serializer
amfInt (AmfInt a) = return $ f a
  where
    f = BL.pack . Prelude.map fromIntegral . toU29 . fromIntegral

amfNumber :: Amf -> Serializer
amfNumber (AmfNumber a) = return $ BL.unfoldr f (doubleToWord a, 7)
  where
    f (_, -1) = Nothing
    f (w, c) = Just (fromIntegral $ w `shiftR` (c*8), (w, c-1))

amfString :: Amf -> Serializer
amfString (AmfString a) = undefined

amfXmlDoc :: Amf -> Serializer
amfXmlDoc (AmfXmlDoc a) = undefined

amfDate :: Amf -> Serializer
amfDate (AmfDate a) = undefined

amfArray :: Amf -> Serializer
amfArray (AmfArray a) = undefined

amfObject :: Amf -> Serializer
amfObject (AmfObject a) = undefined

amfXml :: Amf -> Serializer
amfXml (AmfXml a) = undefined

amfByteArray :: Amf -> Serializer
amfByteArray (AmfByteArray a) = undefined

amfVecInt :: Amf -> Serializer
amfVecInt (AmfVecInt a) = undefined

amfVecUInt :: Amf -> Serializer
amfVecUInt (AmfVecUInt a) = undefined

amfVecNumber :: Amf -> Serializer
amfVecNumber (AmfVecNumber a) = undefined

amfVecObject :: Amf -> Serializer
amfVecObject (AmfVecObject a) = undefined

amfDictionary :: Amf -> Serializer
amfDictionary (AmfDictionary a) = undefined
