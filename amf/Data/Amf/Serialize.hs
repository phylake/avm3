{-# LANGUAGE ScopedTypeVariables #-}
module Data.Amf.Serialize (serialize) where

import           Control.Monad (replicateM, liftM2)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Amf.Def
--import           Data.Amf.Util as U
import           Data.Binary.IEEE754 (wordToDouble)
import           Data.Bits
import           Data.Char (digitToInt, intToDigit)
import           Data.Conduit
import           Data.Conduit.Binary as CB
import           Data.Word (Word8, Word32)
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
loop (AmfUndefined:amfs)          = amfWord8 0x00     >>= append (loop amfs)
loop (AmfNull:amfs)               = amfWord8 0x01     >>= append (loop amfs)
loop (AmfFalse:amfs)              = amfWord8 0x02     >>= append (loop amfs)
loop (AmfTrue:amfs)               = amfWord8 0x03     >>= append (loop amfs)
loop (amf@(AmfInt _):amfs)        = amfInt amf        >>= append (loop amfs)
loop (amf@(AmfNumber _):amfs)     = amfNumber amf     >>= append (loop amfs)
loop (amf@(AmfString _):amfs)     = amfString amf     >>= append (loop amfs)
loop (amf@(AmfXmlDoc _):amfs)     = amfXmlDoc amf     >>= append (loop amfs)
loop (amf@(AmfDate _):amfs)       = amfDate amf       >>= append (loop amfs)
loop (amf@(AmfArray _):amfs)      = amfArray amf      >>= append (loop amfs)
loop (amf@(AmfObject _):amfs)     = amfObject amf     >>= append (loop amfs)
loop (amf@(AmfXml _):amfs)        = amfXml amf        >>= append (loop amfs)
loop (amf@(AmfByteArray _):amfs)  = amfByteArray amf  >>= append (loop amfs)
loop (amf@(AmfVecInt _):amfs)     = amfVecInt amf     >>= append (loop amfs)
loop (amf@(AmfVecUInt _):amfs)    = amfVecUInt amf    >>= append (loop amfs)
loop (amf@(AmfVecNumber _):amfs)  = amfVecNumber amf  >>= append (loop amfs)
loop (amf@(AmfVecObject _):amfs)  = amfVecObject amf  >>= append (loop amfs)
loop (amf@(AmfDictionary _):amfs) = amfDictionary amf >>= append (loop amfs)

append :: Serializer -> BLC.ByteString -> Serializer
append s b = s >>= return . BL.append b

amfWord8 :: Word8 -> Serializer
amfWord8 = return . BL.singleton

amfInt :: Amf -> Serializer
amfInt (AmfInt a) = undefined

amfNumber :: Amf -> Serializer
amfNumber (AmfNumber a) = undefined

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

{- the number of bytes an Int will occupy once serialized -}
--deserializedU29Length :: (Real a) => a -> Int
deserializedU29Length :: Int32 -> Int
deserializedU29Length x
  | x >= 0x00000000 && x <= 0x0000007f = 1
  | x >= 0x00000080 && x <= 0x00003fff = 2
  | x >= 0x00004000 && x <= 0x001fffff = 3
  | x >= 0x00200000 && x <= 0x3fffffff = 4
  | otherwise                          = 0
