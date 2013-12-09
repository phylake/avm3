{-# LANGUAGE ScopedTypeVariables #-}
module Data.Amf.Deserialize.Util where

import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Amf.Def
import           Data.Amf.Deserialize.Def
import           Data.Conduit ((=$))
import           Data.Conduit.Binary as CB
import           Data.Conduit.List as CL
import           Data.Int (Int32)
import           Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified MonadLib as ML

{-
  Parser
-}

peek :: Parser (Maybe B.ByteString)
peek = ML.lift $ CL.peek

head_ :: Parser Word8
head_ = ML.lift $ do Just a <- CB.head; return a

take :: Int -> Parser BL.ByteString
take = ML.lift . CB.take

takeWhile :: (Word8 -> Bool) -> Parser BL.ByteString
takeWhile f = ML.lift $ CB.takeWhile f =$ CL.consume >>= return . BL.fromChunks

reverseAmfs :: ([Amf], Tables) -> ([Amf], Tables)
reverseAmfs (amfs, (st, cot, tt)) =
  (reverse amfs, (reverse st, reverse cot, reverse tt))

{-
  Tables
-}

getString :: UTF_8_vr -> Parser String
getString (U29S_Value v) = return v
getString (U29S_Ref u29) = getST u29

pushST :: String -> Parser String
pushST "" = return ""
pushST a = do
  (st, cot, tt) <- ML.get
  ML.set (a : st, cot, tt)
  return a

pushCOT :: Amf -> Parser Amf
pushCOT a = do
  (st, cot, tt) <- ML.get
  ML.set (st, a : cot, tt)
  return a

pushTT :: Traits -> Parser Traits
pushTT a = do
  (st, cot, tt) <- ML.get
  ML.set (st, cot, a : tt)
  return a

getST :: Int -> Parser String
getST i = do
  (st, _, _) <- ML.get
  return $ st !! (length st - i - 1)

getCOT :: Int -> Parser Amf
getCOT i = do
  (_, cot, _) <- ML.get
  return $ cot !! (length cot - i - 1)

getTT :: Int -> Parser Traits
getTT i = do
  (_, _, tt) <- ML.get
  return $ tt !! (length tt - i - 1)

{-
  Util
-}

toStrict :: BL.ByteString -> Parser B.ByteString
toStrict = return . B.pack . BL.unpack

p :: String -> Parser ()
p = ML.lift . lift . lift . putStrLn
