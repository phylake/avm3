module Main where

import Swf.Deserialize as Swf
import Swf.Def
import System.Environment (getArgs)
import System.FilePath (dropExtension)
import Data.ByteString as B

main = do
  (file:_) <- getArgs
  swfs <- Swf.deserialize file
  B.writeFile (dropExtension file ++ ".abc") $ getDoAbc swfs
  where
    getDoAbc ((Swf_DoABC _ _ abc):swfs) = abc
    getDoAbc (swf:swfs) = getDoAbc swfs
