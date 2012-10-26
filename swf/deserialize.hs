module SWF.Deserialize where

import Codec.Compression.Zlib
import Control.Applicative ((<|>))
import Data.Binary.IEEE754 (wordToDouble)
import Data.Bits
import Data.Char (digitToInt, intToDigit)
import Data.Maybe (listToMaybe)
import Data.Word
import System (getArgs)
import Util
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Char8 as DBC
import qualified Data.ByteString.Lazy.Char8 as DBLC

testFile = do
    args <- getArgs
    let (Just file) = listToMaybe args <|> Just "file.swf"
    bs <- DBL.readFile file
    case parseSwf bs of
        Left err -> putStrLn err
        Right swfs -> putStrLn $ unlines $ map show $ reverse swfs

parseSwf :: DBL.ByteString -> Either String [Swf]
parseSwf bs = (parseHeader1 $ nWords 8 bs) >>=
              parseHeader2                 >>=
              \bs' -> consumeAll ([], bs') >>=
              \(swfs, _) -> Right swfs

unzipSwf = do
    file <- getLine
    let (name, ext) = span (\c -> c /= '.') file
    bs <- DBL.readFile file
    case (parseHeader1 $ nWords 8 bs) of
        Left err -> putStrLn err
        Right bs' -> let combined = DBL.append (DBL.take 8 bs) bs' in
                     DBL.writeFile (name ++ "_uncompressed" ++ ext) combined


fromByteString :: DBL.ByteString -> Either String [Swf]
fromByteString bs =
    case (parseHeader1 $ nWords 8 bs) >>= parseHeader2 >>= \bs' -> consumeAll ([], bs') of
        Left err -> Left err
        Right (swfs, _) -> Right swfs

{- PRE zlib -}
parseHeader1 :: ([Word8], DBL.ByteString) -> Either String DBL.ByteString
parseHeader1 ((cf:w:s:version:_:_:_:_:[]), bs)
    | not ((w8e cf 'C' || w8e cf 'F') && w8e w 'W' && w8e s 'S') = Left "parseHeader1 - invalid header"
    {-| version `elem` [10..14] = Left $ "parseHeader1 - invalid version: " ++ show version-}
    | w8e cf 'C' = Right $ decompress bs
    | w8e cf 'F' = Right bs
    | otherwise = Left "parseHeader1 - unknown error"
    where
        w8e = (==) . word8ToChar

{- POST zlib -}
-- (Rect -> Word16 -> Word16 -> Swf)
parseHeader2 :: DBL.ByteString -> Either String DBL.ByteString
parseHeader2 bs = Right bs''
    where
        bs' = snd $ fromRect bs
        -- frame rate and frame count
        bs'' = DBL.drop 4 bs'

consumeAll :: ([Swf], DBL.ByteString) -> Either String ([Swf], DBL.ByteString)
consumeAll swft@(_, bs) = if DBL.null bs
    then Right swft
    else fromSwfLoop swft >>= consumeAll

class SwfPrim a where
    fromSwf :: DBL.ByteString -> Either String (a, DBL.ByteString)

data RecordHeader = RecordHeader {
                                   rhTag :: Word16    -- tag
                                 , rhLength :: Word32 -- tag length
                                 }

recordHeader :: DBL.ByteString -> (RecordHeader, DBL.ByteString)
recordHeader bs =
    let (w16, bs') = fromU16LE bs in
    let tag = fromIntegral $ (w16 `shiftR` 6) .&. 0x3ff in
    let shortLen = fromIntegral $ w16 .&. 0x3f in
    if shortLen /= 0x3f
        then (RecordHeader tag shortLen, bs')
        else let (w32, bs'') = fromU32LE bs' in
            let longLen = fromIntegral w32 in
            (RecordHeader tag longLen, bs'')

data Rect = Rect {
                   xMin :: Int
                 , xMax :: Int
                 , yMin :: Int
                 , yMax :: Int
                 }
                 deriving (Show)

fromRect :: DBL.ByteString -> (Rect, DBL.ByteString)
fromRect bs = (rect, bs')
    where
        nBits = fromIntegral $ DBL.head bs `shiftR` 3
        wordWidth = ceiling $ (nBits*4 + 5)/8.0
        bs' = DBL.drop wordWidth bs
        -- TODO make this real
        rect = Rect 1 1 1 1

fromString :: DBL.ByteString -> (String, DBL.ByteString)
fromString bs =
    let (string, bs') = DBL.span (\w -> w /= 0x00) bs in
    (DBLC.unpack string, DBL.drop 1 bs')

fromAbc :: DBL.ByteString -> Either String Swf
fromAbc bs =
    let (w32, bs') = fromU32LE bs in
    let (string, bs'') = fromString bs' in
    Right $ Swf_DoABC w32 string bs''

fromSymbolClass :: DBL.ByteString -> Either String Swf
fromSymbolClass bs =
    let (numSymbols, bs') = fromU16LE bs in
    Right $ Swf_SymbolClass $ tagNamePairs bs' (fromIntegral numSymbols) []
    where
        --tagNamePairs :: (DBL.ByteString, Int, [(Word16, String)]) -> (DBL.ByteString, Int, [(Word16, String)])
        tagNamePairs :: DBL.ByteString -> Int -> [(Word16, String)] -> [(Word16, String)]
        tagNamePairs bs i pairs
            | i > 0 = pair : tagNamePairs bs' (i-1) pairs
            | otherwise = pairs
            where
                tagT = fromU16LE bs
                nameT = fromString $ snd tagT
                pair = (fst tagT, fst nameT)
                bs' = snd nameT


data Definition a = Definition RecordHeader a

{- !undocumented tag! -}
data Swf = Swf_Header Word8 Word32 Rect Word16 Word16
         | {-  0 -} Swf_End
         | {-  1 -} Swf_ShowFrame
         | {-  2 -} Swf_DefineShape
         | {-  4 -} Swf_PlaceObject
         | {-  5 -} Swf_RemoveObject
         | {-  6 -} Swf_DefineBits
         | {-  7 -} Swf_DefineButton
         | {-  8 -} Swf_JPEGTables
         | {-  9 -} Swf_SetBackgroundColor
         | {- 10 -} Swf_DefineFont
         | {- 11 -} Swf_DefineText
         | {- 12 -} Swf_DoAction
         | {- 13 -} Swf_DefineFontInfo
         | {- 14 -} Swf_DefineSound
         | {- 15 -} Swf_StartSound
         | {- 17 -} Swf_DefineButtonSound
         | {- 18 -} Swf_SoundbufHead
         | {- 19 -} Swf_SoundbufBlock
         | {- 20 -} Swf_DefineBitsLossless
         | {- 21 -} Swf_DefineBitsJPEG2
         | {- 22 -} Swf_DefineShape2
         | {- 23 -} Swf_DefineButtonCxform
         | {- 24 -} Swf_Protect
         | {- 26 -} Swf_PlaceObject2
         | {- 28 -} Swf_RemoveObject2
         | {- 32 -} Swf_DefineShape3
         | {- 33 -} Swf_DefineText2
         | {- 34 -} Swf_DefineButton2
         | {- 35 -} Swf_DefineBitsJPEG3
         | {- 36 -} Swf_DefineBitsLossless2
         | {- 37 -} Swf_DefineEditText
         | {- 39 -} Swf_DefineSprite
         | {-!41!-} Swf_ProductInfo
         | {- 43 -} Swf_FrameLabel
         | {- 45 -} Swf_SoundbufHead2
         | {- 46 -} Swf_DefineMorphShape
         | {- 48 -} Swf_DefineFont2
         | {- 56 -} Swf_ExportAssets
         | {- 57 -} Swf_ImportAssets
         | {- 58 -} Swf_EnableDebugger
         | {- 59 -} Swf_DoInitAction
         | {- 60 -} Swf_DefineVideobuf
         | {- 61 -} Swf_VideoFrame
         | {- 62 -} Swf_DefineFontInfo2
         | {-!63!-} Swf_DebugId
         | {- 64 -} Swf_EnableDebugger2
         | {- 65 -} Swf_ScriptLimits
         | {- 66 -} Swf_SetTabIndex
         | {- 69 -} Swf_FileAttributes
         | {- 70 -} Swf_PlaceObject3
         | {- 71 -} Swf_ImportAssets2
         | {- 73 -} Swf_DefineFontAlignZones
         | {- 74 -} Swf_CSMTextSettings
         | {- 75 -} Swf_DefineFont3
         | {- 76 -} Swf_SymbolClass [(Word16, String)]
         | {- 77 -} Swf_Metadata
         | {- 78 -} Swf_DefineScalingGrid
         | {- 82 -} Swf_DoABC Word32 String DBL.ByteString
         | {- 83 -} Swf_DefineShape4
         | {- 84 -} Swf_DefineMorphShape2
         | {- 86 -} Swf_DefineSceneAndFrameLabelData
         | {- 87 -} Swf_DefineBinaryData
         | {- 88 -} Swf_DefineFontName
         | {- 89 -} Swf_StartSound2
         | {- 90 -} Swf_DefineBitsJPEG4
         | {- 91 -} Swf_DefineFont4
         deriving (Show)

fromSwfLoop :: ([Swf], DBL.ByteString) -> Either String ([Swf], DBL.ByteString)
fromSwfLoop (swfs, bs) =
    let (RecordHeader tag len, bs') = recordHeader bs in
    let (half1, half2) = DBL.splitAt (fromIntegral len) bs' in
    case tag of
        0  {-                          End -} -> Right (Swf_End : swfs, half2)
        1  {-                    ShowFrame -} -> Right (Swf_ShowFrame : swfs, half2)
        2  {-                  DefineShape -} -> Right (Swf_DefineShape : swfs, half2)
        4  {-                  PlaceObject -} -> Right (Swf_PlaceObject : swfs, half2)
        5  {-                 RemoveObject -} -> Right (Swf_RemoveObject : swfs, half2)
        6  {-                   DefineBits -} -> Right (Swf_DefineBits : swfs, half2)
        7  {-                 DefineButton -} -> Right (Swf_DefineButton : swfs, half2)
        8  {-                   JPEGTables -} -> Right (Swf_JPEGTables : swfs, half2)
        9  {-           SetBackgroundColor -} -> Right (Swf_SetBackgroundColor : swfs, half2)
        10 {-                   DefineFont -} -> Right (Swf_DefineFont : swfs, half2)
        11 {-                   DefineText -} -> Right (Swf_DefineText : swfs, half2)
        12 {-                     DoAction -} -> Right (Swf_DoAction : swfs, half2)
        13 {-               DefineFontInfo -} -> Right (Swf_DefineFontInfo : swfs, half2)
        14 {-                  DefineSound -} -> Right (Swf_DefineSound : swfs, half2)
        15 {-                   StartSound -} -> Right (Swf_StartSound : swfs, half2)
        17 {-            DefineButtonSound -} -> Right (Swf_DefineButtonSound : swfs, half2)
        18 {-                 SoundbufHead -} -> Right (Swf_SoundbufHead : swfs, half2)
        19 {-                SoundbufBlock -} -> Right (Swf_SoundbufBlock : swfs, half2)
        20 {-           DefineBitsLossless -} -> Right (Swf_DefineBitsLossless : swfs, half2)
        21 {-              DefineBitsJPEG2 -} -> Right (Swf_DefineBitsJPEG2 : swfs, half2)
        22 {-                 DefineShape2 -} -> Right (Swf_DefineShape2 : swfs, half2)
        23 {-           DefineButtonCxform -} -> Right (Swf_DefineButtonCxform : swfs, half2)
        24 {-                      Protect -} -> Right (Swf_Protect : swfs, half2)
        26 {-                 PlaceObject2 -} -> Right (Swf_PlaceObject2 : swfs, half2)
        28 {-                RemoveObject2 -} -> Right (Swf_RemoveObject2 : swfs, half2)
        32 {-                 DefineShape3 -} -> Right (Swf_DefineShape3 : swfs, half2)
        33 {-                  DefineText2 -} -> Right (Swf_DefineText2 : swfs, half2)
        34 {-                DefineButton2 -} -> Right (Swf_DefineButton2 : swfs, half2)
        35 {-              DefineBitsJPEG3 -} -> Right (Swf_DefineBitsJPEG3 : swfs, half2)
        36 {-          DefineBitsLossless2 -} -> Right (Swf_DefineBitsLossless2 : swfs, half2)
        37 {-               DefineEditText -} -> Right (Swf_DefineEditText : swfs, half2)
        39 {-                 DefineSprite -} -> Right (Swf_DefineSprite : swfs, half2)
        41 {-                  ProductInfo -} -> Right (Swf_ProductInfo : swfs, half2)
        43 {-                   FrameLabel -} -> Right (Swf_FrameLabel : swfs, half2)
        45 {-                SoundbufHead2 -} -> Right (Swf_SoundbufHead2 : swfs, half2)
        46 {-             DefineMorphShape -} -> Right (Swf_DefineMorphShape : swfs, half2)
        48 {-                  DefineFont2 -} -> Right (Swf_DefineFont2 : swfs, half2)
        56 {-                 ExportAssets -} -> Right (Swf_ExportAssets : swfs, half2)
        57 {-                 ImportAssets -} -> Right (Swf_ImportAssets : swfs, half2)
        58 {-               EnableDebugger -} -> Right (Swf_EnableDebugger : swfs, half2)
        59 {-                 DoInitAction -} -> Right (Swf_DoInitAction : swfs, half2)
        60 {-               DefineVideobuf -} -> Right (Swf_DefineVideobuf : swfs, half2)
        61 {-                   VideoFrame -} -> Right (Swf_VideoFrame : swfs, half2)
        62 {-              DefineFontInfo2 -} -> Right (Swf_DefineFontInfo2 : swfs, half2)
        63 {-                      DebugId -} -> Right (Swf_DebugId : swfs, half2)
        64 {-              EnableDebugger2 -} -> Right (Swf_EnableDebugger2 : swfs, half2)
        65 {-                 ScriptLimits -} -> Right (Swf_ScriptLimits : swfs, half2)
        66 {-                  SetTabIndex -} -> Right (Swf_SetTabIndex : swfs, half2)
        69 {-               FileAttributes -} -> Right (Swf_FileAttributes : swfs, half2)
        70 {-                 PlaceObject3 -} -> Right (Swf_PlaceObject3 : swfs, half2)
        71 {-                ImportAssets2 -} -> Right (Swf_ImportAssets2 : swfs, half2)
        73 {-         DefineFontAlignZones -} -> Right (Swf_DefineFontAlignZones : swfs, half2)
        74 {-              CSMTextSettings -} -> Right (Swf_CSMTextSettings : swfs, half2)
        75 {-                  DefineFont3 -} -> Right (Swf_DefineFont3 : swfs, half2)
        76 {-                  SymbolClass -} -> fromSymbolClass half1 >>= \swf -> Right (swf : swfs, half2)
        77 {-                     Metadata -} -> Right (Swf_Metadata : swfs, half2)
        78 {-            DefineScalingGrid -} -> Right (Swf_DefineScalingGrid : swfs, half2)
        82 {-                        DoABC -} -> fromAbc half1 >>= \swf -> Right (swf : swfs, half2)
        83 {-                 DefineShape4 -} -> Right (Swf_DefineShape4 : swfs, half2)
        84 {-            DefineMorphShape2 -} -> Right (Swf_DefineMorphShape2 : swfs, half2)
        86 {- DefineSceneAndFrameLabelData -} -> Right (Swf_DefineSceneAndFrameLabelData : swfs, half2)
        87 {-             DefineBinaryData -} -> Right (Swf_DefineBinaryData : swfs, half2)
        88 {-               DefineFontName -} -> Right (Swf_DefineFontName : swfs, half2)
        89 {-                  StartSound2 -} -> Right (Swf_StartSound2 : swfs, half2)
        90 {-              DefineBitsJPEG4 -} -> Right (Swf_DefineBitsJPEG4 : swfs, half2)
        91 {-                  DefineFont4 -} -> Right (Swf_DefineFont4 : swfs, half2)
        otherwise -> Left $ "fromSwfLoop - unrecognized tag: " ++ show tag ++ "\n"
                     ++ (unlines $ map show swfs)

