{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Abc.Deserialize (parseAbc)
import           Abc.Json2
import           Swf.Def
import           Swf.Deserialize as Swf
import           System.Environment (getArgs)
import           System.FilePath (dropExtension)
import           Text.JSON
import qualified Abc.Def as Abc
import qualified Data.ByteString as B
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB

main :: IO ()
main = do
  (file:_) <- getArgs
  abcBytes <- Swf.deserialize file >>= return . getDoAbc
  B.writeFile (dropExtension file ++ ".abc") abcBytes
  
  (abc :: Abc.Abc) <- E.run_ (EB.enumFile (dropExtension file ++ ".abc") E.$$ parseAbc)
  writeFile (dropExtension file ++ ".abc.a.json") $ encode $ abcToJson abc
  writeFile (dropExtension file ++ ".abc.b.json") $ encode $ showJSON abc
  where
    getDoAbc ((Swf_DoABC _ _ abc):swfs) = abc
    getDoAbc (swf:swfs) = getDoAbc swfs
