module Amf.Util (peek, head_, Amf.Util.take, Amf.Util.takeWhile) where

import           Amf.Def
import           Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified MonadLib as ML

peek :: (Monad m, ML.MonadT t) => t (E.Iteratee B.ByteString m) (Maybe B.ByteString)
peek = ML.lift $ E.peek

head_ :: (Monad m, ML.MonadT t) => t (E.Iteratee B.ByteString m) Word8
head_ = ML.lift $ EB.head_

take :: (Monad m, ML.MonadT t)
     => Integer
     -> t (E.Iteratee B.ByteString m) BL.ByteString
take i = ML.lift $ EB.take i

takeWhile :: (Monad m, ML.MonadT t)
          => (Word8 -> Bool)
          -> t (E.Iteratee B.ByteString m) BL.ByteString
takeWhile f = ML.lift $ EB.takeWhile f
