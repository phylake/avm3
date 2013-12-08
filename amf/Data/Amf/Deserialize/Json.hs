{-# LANGUAGE ScopedTypeVariables #-}
module Data.Amf.Deserialize.Json (amfToJson) where

import           Data.Amf.Def
import           Data.Int (Int32)
import           Data.Word
import           Data.Word (Word8)
import           Text.JSON
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified MonadLib as ML

kvToObject :: [(String, JSValue)] -> JSValue
kvToObject = JSObject . toJSObject

toArray :: JSON a => [a] -> JSValue
toArray = JSArray . map showJSON

amfToJson :: String -> [Amf] -> JSValue
amfToJson [] = JSArray []
