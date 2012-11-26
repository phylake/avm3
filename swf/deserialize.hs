module Swf.Deserialize where

import           Codec.Compression.Zlib
import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Control.Monad
import           Data.Binary.IEEE754 (wordToFloat)
import           Data.Bits
import           Data.Char (digitToInt, intToDigit)
import           Data.Int (Int64)
import           Data.Maybe (listToMaybe)
import           Data.Word
import           Swf.Def
import           Swf.Util
import           System.Environment (getArgs)
import           Util.Misc
import           Util.Words (word8ToChar, shuffle_bits, toWord32, toWord16)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

test_file = do
  args <- getArgs
  let (Just file) = listToMaybe args <|> Just "swf/file.swf"
  BS.readFile file >>= evalStateT parse_swf

{-unzipSwf = do
  (file:[]) <- getArgs
  let (name, ext) = span (\c -> c /= '.') file
  bs <- BS.readFile file
  case (parse_header $ nWords 8 bs) of
    Left err -> putStrLn err
    Right bs' -> let combined = BS.append (BS.take 8 bs) bs' in
           BS.writeFile (name ++ "_uncompressed" ++ ext) combined-}

{-fromByteString :: BS.ByteString -> Either String [Swf]
fromByteString bs =
  case (parse_header $ nWords 8 bs) >>= parseHeader2 >>= \bs' -> consumeAll ([], bs') of
    Left err -> Left err
    Right (swfs, _) -> Right swfs-}

parse_swf :: Parser [Swf]
parse_swf = liftM2 (:) parse_header $ allBytes parse_tag

parse_header :: Parser Swf
parse_header = do
  (cf:w:s:version:[]) <- nWords 4
  file_length <- fromU32LE
  if not ((w8e cf 'C' || w8e cf 'F') && w8e w 'W' && w8e s 'S')
    --version `elem` [10..14] -> return $ Left $ "parse_header - invalid version: " ++ show version
    then fail "parse_header - invalid header"
    else do
      case word8ToChar cf of
        'C' -> get >>= put . decompress
        'F' -> return ()
        otherwise -> fail "parse_header - unknown error"
  frame_size <- parse_rect
  frame_rate <- fromU16LE
  frame_count <- fromU16LE
  return $ Swf_Header version file_length frame_size frame_rate frame_count
  where
    w8e = (==) . word8ToChar

parse_record_header :: Parser RecordHeader
parse_record_header = do
  w16 <- fromU16LE
  let tag = fromIntegral $ (w16 `shiftR` 6) .&. 0x3ff
  let shortLen = fromIntegral $ w16 .&. 0x3f
  if shortLen == 0x3f
    then liftM (RecordHeader tag) fromU32LE
    else return $ RecordHeader tag shortLen

parse_rect :: Parser Rect
parse_rect = do
  bs <- get
  let nbits = BS.head bs `shiftR` (8-5)
  let wordWidth = ceiling $ ((fromIntegral nbits)*4 + 5)/8
  ws <- nWords wordWidth
  let rect = evalState (rect_parser $ fromIntegral nbits) (5,ws)
  return rect
  where
    rect_parser :: Float -> BitParser Rect
    rect_parser nbits = liftM4 Rect rvw32 rvw32 rvw32 rvw32
      where
        rvw32 = read_variable_w32 nbits

parse_matrix :: Parser Matrix
parse_matrix = do
  bs <- get
  let (m,(p,_)) = runState matrix_parser (0, BS.unpack$ BS.take max_bytes bs)
  put$ BS.drop (ceiling$ p/8) bs
  return m
  where
    max_bytes :: Int64
    max_bytes = ceiling$ (1+5+31*2)*3/8
    
    matrix_parser :: BitParser Matrix
    matrix_parser = do
      (scale_x, scale_y) <- matrix_field
      (rotate0, rotate1) <- matrix_field
      (translate_x, translate_y) <- matrix_field
      return $ Matrix scale_x scale_y rotate0 rotate1 translate_x translate_y
    
    matrix_field :: BitParser (Word32, Word32)
    matrix_field = do
      unless_flag (return (0,0)) $ do
        nbits <- read_variable_w8 5
        a <- read_variable_w32$ fromIntegral nbits
        b <- read_variable_w32$ fromIntegral nbits
        return (a,b)

parse_string :: Parser String
parse_string = do
  bs <- get
  stringBytes <- StateT $ return . BS.span (/= 0x00)
  StateT $ \s -> return ((), BS.drop 1 bs)
  return $ BSC.unpack stringBytes

parse_abc :: Parser Swf
parse_abc = liftM3 Swf_DoABC fromU32LE parse_string get

parse_symbol_class :: Parser Swf
parse_symbol_class = liftM Swf_SymbolClass $ fromU16LE >>= forNState tagNamePair
  where
    tagNamePair :: Parser (Word16, String)
    tagNamePair = liftM2 (,) fromU16LE parse_string

parse_tag :: Parser Swf
parse_tag = do
  (RecordHeader tag len) <- parse_record_header
  --liftIO.putStrLn$ "tag " ++ show tag ++ " len " ++ show len
  tag_bs <- StateT $ return . BS.splitAt (fromIntegral len)
  bs <- get
  --get >>= liftIO.putStrLn.show
  swf <- withStateT (\_ -> tag_bs) $ case tag of
    0  {-                          End -} -> return Swf_End
    1  {-                    ShowFrame -} -> return Swf_ShowFrame
    2  {-                  DefineShape -} -> return Swf_DefineShape
    4  {-                  PlaceObject -} -> return Swf_PlaceObject
    5  {-                 RemoveObject -} -> return Swf_RemoveObject
    6  {-                   DefineBits -} -> return Swf_DefineBits
    7  {-                 DefineButton -} -> return Swf_DefineButton
    8  {-                   JPEGTables -} -> return Swf_JPEGTables
    9  {-           SetBackgroundColor -} -> return Swf_SetBackgroundColor
    10 {-                   DefineFont -} -> return Swf_DefineFont
    11 {-                   DefineText -} -> return Swf_DefineText
    12 {-                     DoAction -} -> return Swf_DoAction
    13 {-               DefineFontInfo -} -> return Swf_DefineFontInfo
    14 {-                  DefineSound -} -> return Swf_DefineSound
    15 {-                   StartSound -} -> return Swf_StartSound
    17 {-            DefineButtonSound -} -> return Swf_DefineButtonSound
    18 {-                 SoundbufHead -} -> return Swf_SoundbufHead
    19 {-                SoundbufBlock -} -> return Swf_SoundbufBlock
    20 {-           DefineBitsLossless -} -> return Swf_DefineBitsLossless
    21 {-              DefineBitsJPEG2 -} -> return Swf_DefineBitsJPEG2
    22 {-                 DefineShape2 -} -> return Swf_DefineShape2
    23 {-           DefineButtonCxform -} -> return Swf_DefineButtonCxform
    24 {-                      Protect -} -> return Swf_Protect
    26 {-                 PlaceObject2 -} -> return Swf_PlaceObject2
    28 {-                RemoveObject2 -} -> return Swf_RemoveObject2
    32 {-                 DefineShape3 -} -> return Swf_DefineShape3
    33 {-                  DefineText2 -} -> return Swf_DefineText2
    34 {-                DefineButton2 -} -> return Swf_DefineButton2
    35 {-              DefineBitsJPEG3 -} -> return Swf_DefineBitsJPEG3
    36 {-          DefineBitsLossless2 -} -> return Swf_DefineBitsLossless2
    37 {-               DefineEditText -} -> return Swf_DefineEditText
    39 {-                 DefineSprite -} -> return Swf_DefineSprite
    41 {-                  ProductInfo -} -> return Swf_ProductInfo
    43 {-                   FrameLabel -} -> return Swf_FrameLabel
    45 {-                SoundbufHead2 -} -> return Swf_SoundbufHead2
    46 {-             DefineMorphShape -} -> return Swf_DefineMorphShape
    48 {-                  DefineFont2 -} -> return Swf_DefineFont2
    56 {-                 ExportAssets -} -> return Swf_ExportAssets
    57 {-                 ImportAssets -} -> return Swf_ImportAssets
    58 {-               EnableDebugger -} -> return Swf_EnableDebugger
    59 {-                 DoInitAction -} -> return Swf_DoInitAction
    60 {-               DefineVideobuf -} -> return Swf_DefineVideobuf
    61 {-                   VideoFrame -} -> return Swf_VideoFrame
    62 {-              DefineFontInfo2 -} -> return Swf_DefineFontInfo2
    63 {-                      DebugId -} -> return Swf_DebugId
    64 {-              EnableDebugger2 -} -> return Swf_EnableDebugger2
    65 {-                 ScriptLimits -} -> return Swf_ScriptLimits
    66 {-                  SetTabIndex -} -> return Swf_SetTabIndex
    69 {-               FileAttributes -} -> return Swf_FileAttributes
    70 {-                 PlaceObject3 -} -> return Swf_PlaceObject3
    71 {-                ImportAssets2 -} -> return Swf_ImportAssets2
    73 {-         DefineFontAlignZones -} -> return Swf_DefineFontAlignZones
    74 {-              CSMTextSettings -} -> return Swf_CSMTextSettings
    75 {-                  DefineFont3 -} -> return Swf_DefineFont3
    76 {-                  SymbolClass -} -> parse_symbol_class
    77 {-                     Metadata -} -> return Swf_Metadata
    78 {-            DefineScalingGrid -} -> return Swf_DefineScalingGrid
    82 {-                        DoABC -} -> parse_abc
    83 {-                 DefineShape4 -} -> return Swf_DefineShape4
    84 {-            DefineMorphShape2 -} -> return Swf_DefineMorphShape2
    86 {- DefineSceneAndFrameLabelData -} -> return Swf_DefineSceneAndFrameLabelData
    87 {-             DefineBinaryData -} -> return Swf_DefineBinaryData
    88 {-               DefineFontName -} -> return Swf_DefineFontName
    89 {-                  StartSound2 -} -> return Swf_StartSound2
    90 {-              DefineBitsJPEG4 -} -> return Swf_DefineBitsJPEG4
    91 {-                  DefineFont4 -} -> return Swf_DefineFont4
    otherwise -> fail $ "parse_tag - unrecognized tag: " ++ show tag
  put bs
  return swf
