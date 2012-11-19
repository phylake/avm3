module Util.Words (
    charToWord8
  , word8ToChar
  , stringToHex
  , stringToHexRe
  , nWords
  , fromU8
  , fromU16
  , fromU16LE
  , fromU32
  , fromU32LE
  , fromDouble
  , fromDoubleLE
  , toWord16
  , toWord16LE
  , toWord32
  , toWord32LE
  , toWord64
  , toWord64LE
  , toDouble
  , fromU32LE_vl
  , fromU30LE_vl
  , fromS24LE
  , fromS32LE_vl
  , hasSignalBit
  , varLenUintLE
  , varIntLenBS
  , varIntLen
  , fromU29
) where

import Control.Applicative ((<$>))
import Data.Binary.IEEE754 (wordToDouble)
import Data.Bits
import Data.Int
import Data.List (intercalate)
import Data.Monoid (mappend)
import Data.Word
import Numeric (showHex)
import TFish
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

type ByteString = BS.ByteString

charToWord8 :: Char -> Word8
charToWord8 = head . BS.unpack . BSC.singleton

word8ToChar :: Word8 -> Char
word8ToChar = head . BSC.unpack . BS.singleton

stringToHex :: String -> [String]
stringToHex = map showHex' . BS.unpack . BSC.pack
    where
        showHex' = (flip showHex) ""

stringToHexRe :: String -> IO ()
stringToHexRe str = putStrLn $ intercalate "\\s?" $ stringToHex str

nWords :: Int64 -> ByteString -> ([Word8], ByteString)
nWords n bs = (BS.unpack $ BS.take n bs, BS.drop n bs)

fromU8 :: ByteString -> (Word8, ByteString)
fromU8 bs = let ((w:[]), bs') = nWords 1 bs in (w, bs')

fromU16 :: ByteString -> (Word16, ByteString)
fromU16 bs = let (ws, bs') = nWords 2 bs in (toWord16 ws, bs')

fromU16LE :: ByteString -> (Word16, ByteString)
fromU16LE bs = let (ws, bs') = nWords 2 bs in (toWord16LE ws, bs')

fromU32 :: ByteString -> (Word32, ByteString)
fromU32 bs = let (ws, bs') = nWords 4 bs in (toWord32 ws, bs')

fromU32LE :: ByteString -> (Word32, ByteString)
fromU32LE bs = let (ws, bs') = nWords 4 bs in (toWord32LE ws, bs')

fromDouble :: ByteString -> (Double, ByteString)
fromDouble bs = let (ws, bs') = nWords 8 bs in (toDouble ws, bs')

fromDoubleLE :: ByteString -> (Double, ByteString)
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

fromU32LE_vl :: ByteString -> (Word32, ByteString)
fromU32LE_vl bs = let (w64, bs') = varLenUintLE bs in (fromIntegral w64, bs')

fromU30LE_vl :: ByteString -> (Word32, ByteString)
fromU30LE_vl bs = let (w32, bs') = fromU32LE_vl bs in (w32 .&. 0x3fffffff, bs')

fromS24LE :: ByteString -> (Int32, ByteString)
fromS24LE bs =
    let (unpackThese, bs') = BS.splitAt 3 bs in
    (fromIntegral . fromS24LE_impl $ map fromIntegral $ BS.unpack unpackThese, bs')
    where
        fromS24LE_impl :: [Word32] -> Word32
        fromS24LE_impl (w3:w2:w1:[]) = sign .|. w1' .|. w2' .|. w3'
            where
                sign = (w1 .&. 0x80) `shiftL` 24
                w1'  = (w1 .&. 0x7f) `shiftL` 16
                w2'  =  w2 `shiftL` 8
                w3'  =  w3

fromS32LE_vl :: ByteString -> (Int32, ByteString)
fromS32LE_vl bs =
    let (unpackThese, bs') = BS.splitAt (varIntLenBS bs) bs in
    (fromIntegral . fromS32LE_vl_impl $ map fromIntegral $ BS.unpack unpackThese, bs')
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
varLenUintLE :: ByteString -> (Word64, ByteString)
varLenUintLE bs =
    let (foldThese, bs') = BS.splitAt (varIntLenBS bs) bs in
    let vli = BS.foldr (flip foldVarLen) 0 foldThese in
    (vli, bs')

varIntLenBS :: ByteString -> Int64
varIntLenBS = (+1) . BS.length . BS.takeWhile hasSignalBit

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
