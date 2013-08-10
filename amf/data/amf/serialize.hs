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
import           Data.Word (Word8)
import           Util.Words
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified MonadLib as ML

type Serializer = ML.StateT Tables IO

serialize :: [Amf] -> IO B.ByteString
serialize = undefined

--toAmfImpl :: ([Amf], Tables) -> Either AmfErr [Word8]
--toAmfImpl (AmfUndefined:amfs)       = undefined
--toAmfImpl (AmfNull:amfs)            = undefined
--toAmfImpl (AmfFalse:amfs)           = undefined
--toAmfImpl (AmfTrue:amfs)            = undefined
--toAmfImpl ((AmfInt u29):amfs)       = undefined
--toAmfImpl ((AmfNumber double):amfs) = undefined
--toAmfImpl ((AmfString utf8vr):amfs) = undefined
--toAmfImpl ((AmfXmlDoc string):amfs) = undefined
--toAmfImpl ((AmfDate string):amfs)   = undefined
--toAmfImpl ((AmfArray u29a):amfs)    = undefined
--toAmfImpl ((AmfObject u29o):amfs)   = undefined
--toAmfImpl ((AmfXml string):amfs)    = undefined
--toAmfImpl ((AmfByteArray ba):amfs)  = undefined
