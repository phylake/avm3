module Util where

import Data.Binary.IEEE754 (wordToDouble)
import Data.Bits
import Data.Int
import Data.List (intercalate)
import Data.Word
import Numeric (showHex)
import System.Directory
import TFish
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Char8 as DBC

charToWord8 :: Char -> Word8
charToWord8 = head . DB.unpack . DBC.singleton

word8ToChar :: Word8 -> Char
word8ToChar = head . DBC.unpack . DB.singleton

stringToHex :: String -> [String]
stringToHex str = map showHex' $ DB.unpack $ DBC.pack str
    where
        showHex' = (flip showHex) ""

stringToHexRe :: String -> IO ()
stringToHexRe str = putStrLn $ intercalate "\\s?" $ stringToHex str

allBytes :: (DBL.ByteString -> (a, DBL.ByteString))
         -> [a] -- initial data
         -> DBL.ByteString
         -> [a] -- initial ++ accumulated
allBytes f acc bs = if DBL.null bs
    then acc
    else let (one, bs') = f bs in allBytes f (acc ++ [one]) bs'

--nWords :: Int64 -> DBL.ByteString -> ([Word8], DBL.ByteString)
nWords n bs = (DBL.unpack $ DBL.take n bs, DBL.drop n bs)

fromU16 :: DBL.ByteString -> (Word16, DBL.ByteString)
fromU16 bs = let (ws, bs') = nWords 2 bs in (toWord16 ws, bs')

fromU16LE :: DBL.ByteString -> (Word16, DBL.ByteString)
fromU16LE bs = let (ws, bs') = nWords 2 bs in (toWord16LE ws, bs')

fromU32 :: DBL.ByteString -> (Word32, DBL.ByteString)
fromU32 bs = let (ws, bs') = nWords 4 bs in (toWord32 ws, bs')

fromU32LE :: DBL.ByteString -> (Word32, DBL.ByteString)
fromU32LE bs = let (ws, bs') = nWords 4 bs in (toWord32LE ws, bs')

fromDouble :: DBL.ByteString -> (Double, DBL.ByteString)
fromDouble bs = let (ws, bs') = nWords 8 bs in (toDouble ws, bs')

toWord16 :: [Word8] -> Word16
toWord16 ws = foldl' foldWords 0 (take 2 ws)

toWord16LE :: [Word8] -> Word16
toWord16LE ws = foldr (flip foldWords) 0 (take 2 ws)

toWord32 :: [Word8] -> Word32
toWord32 ws = foldl' foldWords 0 (take 4 ws)

toWord32LE :: [Word8] -> Word32
toWord32LE ws = foldr (flip foldWords) 0 (take 4 ws)

toWord64 :: [Word8] -> Word64
toWord64 ws = foldl' foldWords 0 (take 8 ws)

toWord64LE :: [Word8] -> Word64
toWord64LE ws = foldr (flip foldWords) 0 (take 8 ws)

toDouble :: [Word8] -> Double
toDouble = wordToDouble . toWord64

{- variable length integers -}

fromU32_vl :: DBL.ByteString -> (Word32, DBL.ByteString)
fromU32_vl bs = let (w64, bs') = varLenUintBSL bs in (fromIntegral w64, bs')

fromU30_vl :: DBL.ByteString -> (Word32, DBL.ByteString)
fromU30_vl bs = let (w32, bs') = fromU32_vl bs in (w32 .&. 0x3fffffff, bs')

{- TODO little-endian -}
fromS32_vl :: DBL.ByteString -> (Int32, DBL.ByteString)
fromS32_vl bs =
    let (unpackThese, bs') = DBL.splitAt (varIntLenBSL bs) bs in
    (fromIntegral . impl $ DBL.unpack unpackThese, bs')
    where
        highByte w l = sign .|. byte0
            where
                sign  = (w .&. 0x40) `shiftL` 25
                byte0 = (w .&. 0x3f) `shiftL` l

        impl (w1:[]) = highByte w1 0
        impl (w2:w1:[]) = highByte w2 7 .|. w1
        impl (w3:w2:w1:[]) = highByte w3 14 .|. byte1 .|. byte0
            where
                byte1 = (w2 .&. 0x7f) `shiftL` 7
                byte0 =  w1
        impl (w4:w3:w2:w1:[]) = highByte w4 27 .|. byte2 .|. byte1 .|. byte0
            where
                byte2 = (w3 .&. 0x7f) `shiftL` 14
                byte1 = (w2 .&. 0x7f) `shiftL` 7
                byte0 =  w1

foldl' f acc []     = acc
foldl' f acc (x:xs) = let acc' = acc `f` x 
                      in seq acc' $ foldl' f acc' xs

foldWords acc w = (acc `shiftL` 8) .|. fromIntegral w
foldVarLen acc w = (acc `shiftL` 7) .|. (fromIntegral w .&. 0x7f)

{-varLenInt :: Bits a => [a] -> (a, [a])
varLenInt [] = (0, [])
varLenInt ws =
    let (foldThese, ws') = span hasSignalBit ws in
    let vli = head ws' .|. foldl foldVarLen 0 foldThese `shiftL` 7 in
    (vli, tail ws')-}

hasSignalBit :: Word8 -> Bool
hasSignalBit w = w .&. 0x80 == 0x80

{- little-endian -}
varLenUintBSL :: DBL.ByteString -> (Word64, DBL.ByteString)
varLenUintBSL bs =
    let (foldThese, bs') = DBL.splitAt (varIntLenBSL bs) bs in
    let vli = DBL.foldr (flip foldVarLen) 0 foldThese in
    (vli, bs')

varIntLenBSL :: DBL.ByteString -> Int64
varIntLenBSL = (+1) . DBL.length . DBL.takeWhile hasSignalBit

varIntLen :: [Word8] -> Int
varIntLen = (+1) . length . takeWhile hasSignalBit

fromU29 :: (Bits a) => [a] -> a
fromU29 (w1:[]) = lsb1
    where
        lsb1 =  w1 .&. 0x7f
fromU29 (w2:w1:[]) = lsb2 .|. lsb1
    where
        lsb2 = (w2 .&. 0x7f) `shiftL` 7
        lsb1 =  w1 .&. 0x7f
fromU29 (w3:w2:w1:[]) = lsb3 .|. lsb2 .|. lsb1
    where
        lsb3 = (w3 .&. 0x7f) `shiftL` 14
        lsb2 = (w2 .&. 0x7f) `shiftL` 7
        lsb1 =  w1 .&. 0x7f
fromU29 (w4:w3:w2:w1:[]) = lsb4 .|. lsb3 .|. lsb2 .|. lsb1
    where
        lsb4 = (w4 .&. 0x7f) `shiftL` 21
        lsb3 = (w3 .&. 0x7f) `shiftL` 14
        lsb2 = (w2 .&. 0x7f) `shiftL` 7
        lsb1 =  w1 {- NO mask. see spec -}

allDirs :: FilePath -> ([FilePath] -> IO a) -> IO a
allDirs dir f = do
    rawDirs <- filterIO doesDirectoryExist $ getDirectoryContents dir
    let realDirs = drop 2 $ reverse rawDirs {- remove ./ and ../ -}
    f realDirs

allFiles :: FilePath -> ([FilePath] -> IO a) -> IO a
allFiles dir f = do
    files <- filterIO doesFileExist $ getDirectoryContents dir
    f files

filterIO :: (a -> IO Bool) -> IO [a] -> IO [a]
filterIO f ioAs = do
    as <- ioAs
    case as of
        [] -> return []
        (x:xs) -> do
            tf <- f x
            if tf
                then filterIO f (return xs) >>= (\xs' -> return $ x:xs')
                else filterIO f (return xs)

forN :: (Ord n, Num n, Monad m)
     => (a -> m a)
     -> a
     -> n
     -> m a
forN f m n
    | n > 0     = return m >>= f >>= \m' -> forN f m' (n-1)
    | otherwise = return m

t21 = fst
t22 = snd

t31 (a,_,_) = a
t32 (_,a,_) = a
t33 (_,_,a) = a

t41 (a,_,_,_) = a
t42 (_,a,_,_) = a
t43 (_,_,a,_) = a
t44 (_,_,_,a) = a
