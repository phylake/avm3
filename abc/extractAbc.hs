module Main where

import SWF.Deserialize as SWF hiding (testFile)
import System (getArgs)
import qualified Data.ByteString.Lazy as DBL (readFile, writeFile)

main = extractAbc

extractAbc = do
    (file:_) <- getArgs
    let file' = takeWhile (\c -> c /= '.') file
    bs <- DBL.readFile file
    let (Swf_DoABC w32 str abc) = getDoAbc $ SWF.fromByteString bs
    DBL.writeFile (file' ++ ".abc") abc
    where
        getDoAbc (Right ((Swf_DoABC a b c):swfs)) = Swf_DoABC a b c
        getDoAbc (Right (swf:swfs)) = getDoAbc $ Right swfs
