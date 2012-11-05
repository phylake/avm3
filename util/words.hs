module Util.Words where

import Control.Applicative ((<$>))
import Data.Binary.IEEE754 (wordToDouble)
import Data.Bits
import Data.Int
import Data.List (intercalate)
import Data.Monoid (mappend)
import Data.Word
import Numeric (showHex)
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

nWords :: Int64 -> DBL.ByteString -> ([Word8], DBL.ByteString)
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

fromDoubleLE :: DBL.ByteString -> (Double, DBL.ByteString)
fromDoubleLE bs = let (ws, bs') = nWords 8 bs in (toDouble $ reverse ws, bs')

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

fromU32LE_vl :: DBL.ByteString -> (Word32, DBL.ByteString)
fromU32LE_vl bs = let (w64, bs') = varLenUintBSL bs in (fromIntegral w64, bs')

fromU30LE_vl :: DBL.ByteString -> (Word32, DBL.ByteString)
fromU30LE_vl bs = let (w32, bs') = fromU32LE_vl bs in (w32 .&. 0x3fffffff, bs')

fromS32LE_vl :: DBL.ByteString -> (Int32, DBL.ByteString)
fromS32LE_vl bs =
    let (unpackThese, bs') = DBL.splitAt (varIntLenBSL bs) bs in
    (fromIntegral . fromS32LE_vl_impl $ map fromIntegral $ DBL.unpack unpackThese, bs')
    where
        fromS32LE_vl_impl :: [Word32] -> Word32
        fromS32LE_vl_impl (w1:[]) = sign .|. w1'
            where
                sign = (w1 .&. 0x40) `shiftL` 25
                w1'  = (w1 .&. 0x3f) `shiftL` 0
        fromS32LE_vl_impl (w2:w1:[]) = sign .|. w1' .|. w2'
            where
                sign = (w1 .&. 0x40) `shiftL` 25
                w1'  = (w1 .&. 0x3f) `shiftL` 7
                w2'  = (w2 .&. 0x7f) `shiftL` 0
        fromS32LE_vl_impl (w3:w2:w1:[]) = sign .|. w1' .|. w2' .|. w3'
            where
                sign = (w1 .&. 0x40) `shiftL` 25
                w1'  = (w1 .&. 0x3f) `shiftL` 14
                w2'  = (w2 .&. 0x7f) `shiftL` 7
                w3'  = (w3 .&. 0x7f) `shiftL` 0
        fromS32LE_vl_impl (w4:w3:w2:w1:[]) = sign .|. w1' .|. w2' .|. w3' .|. w4'
            where
                sign = (w1 .&. 0x40) `shiftL` 25
                w1'  = (w1 .&. 0x3f) `shiftL` 21
                w2'  = (w2 .&. 0x7f) `shiftL` 14
                w3'  = (w3 .&. 0x7f) `shiftL` 7
                w4'  = (w4 .&. 0x7f) `shiftL` 0


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

forN :: (Ord n, Num n, Monad m)
     => (a -> m a)
     -> a
     -> n
     -> m a
forN f m n
    | n > 0     = return m >>= f >>= \m' -> forN f m' (n-1)
    | otherwise = return m

forN' f a n
    | n > 0 = forN' f (f a) (n-1)
    | otherwise = a

t21 = fst
t22 = snd

t31 (a,_,_) = a
t32 (_,a,_) = a
t33 (_,_,a) = a

t41 (a,_,_,_) = a
t42 (_,a,_,_) = a
t43 (_,_,a,_) = a
t44 (_,_,_,a) = a
