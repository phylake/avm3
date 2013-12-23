module Util.Words (
  charToWord8
, word8ToChar
, stringToHex
, stringToHexRe
, shuffle_bits
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
, foldWords
, foldVarLen
, hasSignalBit
, varIntLenBS
, varIntLen
, u30Bytes
, fromU29
, toU29
) where

import Control.Applicative ((<$>))
import Data.Binary.IEEE754 (wordToDouble)
import Data.Bits
import Data.Int
import Data.List (intercalate)
import Data.Monoid (mappend)
import Data.Word
import Numeric (showHex)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

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

shuffle_bits :: (Bits a, Num a)
             => Int -- ^ right shift
             -> [a]
             -> [a]
shuffle_bits shift ws = map map_f $ zip (0:init ws) ws
  where
    r_shift = shift `mod` 8
    l_shift = 8 - r_shift
    r_mask = 0xff `shiftR` r_shift
    l_mask = 0xff `shiftL` l_shift .&. 0xff
    map_f = \(w1,w2) -> (w1 `shiftL` l_shift .&. l_mask) .|.
                        (w2 `shiftR` r_shift .&. r_mask)

nWords :: Int -> BS.ByteString -> ([Word8], BS.ByteString)
nWords n bs = (BS.unpack $ BS.take n bs, BS.drop n bs)

fromU8 :: BS.ByteString -> (Word8, BS.ByteString)
fromU8 bs = let ((w:[]), bs') = nWords 1 bs in (w, bs')

fromU16 :: BS.ByteString -> (Word16, BS.ByteString)
fromU16 bs = let (ws, bs') = nWords 2 bs in (toWord16 ws, bs')

fromU16LE :: BS.ByteString -> (Word16, BS.ByteString)
fromU16LE bs = let (ws, bs') = nWords 2 bs in (toWord16LE ws, bs')

fromU32 :: BS.ByteString -> (Word32, BS.ByteString)
fromU32 bs = let (ws, bs') = nWords 4 bs in (toWord32 ws, bs')

fromU32LE :: BS.ByteString -> (Word32, BS.ByteString)
fromU32LE bs = let (ws, bs') = nWords 4 bs in (toWord32LE ws, bs')

fromDouble :: BS.ByteString -> (Double, BS.ByteString)
fromDouble bs = let (ws, bs') = nWords 8 bs in (toDouble ws, bs')

fromDoubleLE :: BS.ByteString -> (Double, BS.ByteString)
fromDoubleLE bs = let (ws, bs') = nWords 8 bs in (toDouble $ reverse ws, bs')

toWord16 :: [Word8] -> Word16
toWord16 = foldl foldWords 0 . take 2

toWord16LE :: [Word8] -> Word16
toWord16LE = foldr (flip foldWords) 0 . take 2

toWord32 :: [Word8] -> Word32
toWord32 = foldl foldWords 0 . take 4

toWord32LE :: [Word8] -> Word32
toWord32LE = foldr (flip foldWords) 0 . take 4

toWord64 :: [Word8] -> Word64
toWord64 = foldl foldWords 0 . take 8

toWord64LE :: [Word8] -> Word64
toWord64LE = foldr (flip foldWords) 0 . take 8

toDouble :: [Word8] -> Double
toDouble = wordToDouble . toWord64

{- variable length integers -}

fromU32LE_vl :: [Word8] -> Word32
fromU32LE_vl = foldr (flip foldVarLen) 0

fromU30LE_vl :: [Word8] -> Word32
fromU30LE_vl = (.&. 0x3fffffff) . fromU32LE_vl

fromS24LE :: [Word8] -> Int32
fromS24LE ws@(w3:w2:w1:[])
  | w1 .&. 0x80 == 0x80 = foldr (flip foldWords) 0 ws .|. 0xFF000000
  | otherwise = foldr (flip foldWords) 0 ws

fromS32LE_vl :: [Word8] -> Int32
fromS32LE_vl = foldr (flip foldVarLen) 0

foldWords :: (Integral a, Bits b, Num b) => b -> a -> b
foldWords acc w = (acc `shiftL` 8) .|. fromIntegral w

foldVarLen :: (Integral a, Bits b, Num b) => b -> a -> b
foldVarLen acc w = (acc `shiftL` 7) .|. (fromIntegral w .&. 0x7f)

hasSignalBit :: Word8 -> Bool
hasSignalBit w = w .&. 0x80 == 0x80

varIntLenBS :: BS.ByteString -> Int
varIntLenBS = (+1) . BS.length . BS.takeWhile hasSignalBit

varIntLen :: [Word8] -> Int
varIntLen = (+1) . length . takeWhile hasSignalBit

-- | The number of bytes the input bits will occupy in a variable length
-- int once serialized
u30Bytes :: (Num a, Ord a, Num b) => a -> b
u30Bytes u30
  | u30 >= 0x00000000 && u30 <= 0x0000007F =  1
  | u30 >= 0x00000080 && u30 <= 0x00003FFF =  2
  | u30 >= 0x00004000 && u30 <= 0x001FFFFF =  3
  | u30 >= 0x00200000 && u30 <= 0x3FFFFFFF =  4
  | otherwise                              = -1

fromU29 :: [Word8] {- ^ Big-endian -} -> Word32
fromU29 = fromU29_ . map fromIntegral where
  fromU29_ (w1:[]) = lsb1
    where
      lsb1 =  w1 .&. 0x7f
  fromU29_ (w2:w1:[]) = lsb2 .|. lsb1
    where
      lsb2 = (w2 .&. 0x7f) `shiftL` 7
      lsb1 =  w1 .&. 0x7f
  fromU29_ (w3:w2:w1:[]) = lsb3 .|. lsb2 .|. lsb1
    where
      lsb3 = (w3 .&. 0x7f) `shiftL` 14
      lsb2 = (w2 .&. 0x7f) `shiftL` 7
      lsb1 =  w1 .&. 0x7f
  fromU29_ (w4:w3:w2:w1:[]) = lsb4 .|. lsb3 .|. lsb2 .|. lsb1
    where
      lsb4 = (w4 .&. 0x7f) `shiftL` 21
      lsb3 = (w3 .&. 0x7f) `shiftL` 14
      lsb2 = (w2 .&. 0x7f) `shiftL` 7
      lsb1 =  w1 {- NO mask. see spec -}

toU29 :: Word32 -> [Word8] -- ^ Big-endian
toU29 w32
  | u30Bytes w32 == 1 = map fromIntegral [lsb1m]
  | u30Bytes w32 == 2 = map fromIntegral [lsb2, lsb1m]
  | u30Bytes w32 == 3 = map fromIntegral [lsb3, lsb2, lsb1m]
  | u30Bytes w32 == 4 = map fromIntegral [lsb4, lsb3, lsb2, lsb1]
  | otherwise         = []
  where
    lsb1m = w32 .&. 0x7F
    lsb1  = w32 {- NO mask. see spec -}
    lsb2  = (w32 `shiftR`  7 .&. 0x7F) .|. 0x80 -- signal bit
    lsb3  = (w32 `shiftR` 14 .&. 0x7F) .|. 0x80
    lsb4  = (w32 `shiftR` 21 .&. 0x7F) .|. 0x80
