module Data.Amf.Util (
  Data.Amf.Util.peek
, head_
, Data.Amf.Util.take
, Data.Amf.Util.takeWhile
) where

import           Data.Amf.Def
import           Data.Conduit
import           Data.Conduit.Binary as CB
import           Data.Conduit.List as CL
import           Data.Void
import           Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified MonadLib as ML

peek :: Parser (Maybe B.ByteString)
peek = ML.lift $ CL.peek

head_ :: Parser Word8
head_ = ML.lift $ do Just a <- CB.head; return a

take :: Int -> Parser BL.ByteString
take = ML.lift . CB.take

takeWhile :: (Word8 -> Bool) -> Parser BL.ByteString
takeWhile f = ML.lift $ CB.takeWhile f =$ CL.consume >>= return . BL.fromChunks
