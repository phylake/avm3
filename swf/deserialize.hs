{-# LANGUAGE ScopedTypeVariables #-}
module Swf.Deserialize (deserialize, deserializeBs) where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Bits
import           Data.Conduit
import           Data.Conduit.Binary as CB
import           Data.Conduit.List as CL
import           Data.Conduit.Zlib
import           Data.Int (Int64)
import           Data.Maybe (listToMaybe)
import           Data.Word
import           Swf.Def
import           Swf.Util
import           System.Environment (getArgs)
import           Util.Misc
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified MonadLib as ML

deserialize :: FilePath -> IO [Swf]
deserialize file = runResourceT $ CB.sourceFile file $$ parse_swf

deserializeBs :: BL.ByteString -> IO [Swf]
deserializeBs lbs = runResourceT $ CB.sourceLbs lbs $$ parse_swf

data Compression = None | Zlib | LZMA

test :: IO ()
test = do
  let file = "swf/file.swf"
  swfs <- deserialize file
  Prelude.mapM_ (putStrLn . show) swfs
  return ()

p :: String -> Parser ()
p = liftIO . putStrLn

parse_swf :: Parser [Swf]
parse_swf = do
  (version, file_length, compression) <- parse_header1
  case compression of
    None -> parse_swf2 version file_length
    Zlib -> decompress (WindowBits 15) =$ parse_swf2 version file_length
    LZMA -> error "LZMA unsupported"

parse_swf2 :: Word8 -> Word32 -> Parser [Swf]
parse_swf2 version file_length = do
  header <- parse_header2 version file_length
  tags <- parse_tags
  return$ header:tags
  where
    parse_tags :: Parser [Swf]
    parse_tags = do
      tag <- parse_tag
      case tag of
        Swf_End -> return [Swf_End]
        otherwise -> do
          tags <- parse_tags
          return$ tag:tags

parse_header1 :: Parser (Word8, Word32, Compression)
parse_header1 = do
  (cf:w:s:version:[]) <- takeWords 4
  file_length <- readU32LE
  if w == 87 && s == 83
    then case cf of
      0x46 -> return (version, file_length, None)
      0x43 -> return (version, file_length, Zlib)
      0x5A -> return (version, file_length, LZMA)
    else error "parse_header - invalid header"

parse_header2 :: Word8 -> Word32 -> Parser Swf
parse_header2 version file_length = do
  frame_size <- parse_rect
  frame_rate <- readFixed8
  frame_count <- readU16LE
  return$ Swf_Header version file_length frame_size frame_rate frame_count

parse_record_header :: Parser RecordHeader
parse_record_header = do
  w16 <- readU16LE
  let tag = fromIntegral $ (w16 `shiftR` 6) .&. 0x3ff
  let shortLen = fromIntegral$ w16 .&. 0x3f
  if shortLen == 0x3f
    then liftM (RecordHeader tag) readU32LE
    else return$ RecordHeader tag shortLen

parse_rect :: Parser Rect
parse_rect = do
  Just w <- CB.head
  let nbits = w `shiftR` (8-5)
  ws <- takeWords $ ceiling $ ((fromIntegral nbits)*4 + 0)/8
  (rect :: Rect, _) <- liftIO$ ML.runStateT (5,w:ws) (rect_parser$ fromIntegral nbits)
  return rect
  where
    rect_parser :: Float -- nbits
                -> BitParser Rect
    rect_parser n = liftM4 Rect (rv_w32 n) (rv_w32 n) (rv_w32 n) (rv_w32 n)

parse_matrix :: Parser Matrix
parse_matrix = parse_common max_bytes matrix_parser
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
    matrix_field = unless_flag (return (0,0)) $ do
      nbits <- rv_w8 5
      a <- rv_w32$ fromIntegral nbits
      b <- rv_w32$ fromIntegral nbits
      return (a,b)

parse_colorxform :: Bool -- alpha
                 -> Parser ColorXForm
parse_colorxform alpha = parse_common max_bytes $ cxform_parser alpha
  where
    max_bytes :: Int64
    max_bytes = if alpha
                  then ceiling$ (1+1+4+15*8)/8
                  else ceiling$ (1+1+4+15*6)/8

    cxform_parser :: Bool -> BitParser ColorXForm
    cxform_parser alpha = do
      has_add_terms <- rv_bool
      has_mult_terms <- rv_bool
      nbits <- rv_w16 4
      let nbits2 = fromIntegral nbits
      (rM,gM,bM) <- if has_mult_terms
                      then liftM3 (,,)
                        (rv_w16 nbits2)
                        (rv_w16 nbits2)
                        (rv_w16 nbits2)
                      else return (0,0,0)
      aM <- if alpha then liftM Just (rv_w16 nbits2) else return Nothing
      (rA,gA,bA) <- if has_add_terms
                      then liftM3 (,,)
                        (rv_w16 nbits2)
                        (rv_w16 nbits2)
                        (rv_w16 nbits2)
                      else return (0,0,0)
      aA <- if alpha then liftM Just (rv_w16 nbits2) else return Nothing
      return ColorXForm {
        rM = rM
      , gM = gM
      , bM = bM
      , aM = aM
      , rA = rA
      , gA = gA
      , bA = bA
      , aA = aA
      }

parse_common :: Int64 -> BitParser a -> Parser a
parse_common max_bytes parser =
  inject_double max_bytes =$ parse_common_impl max_bytes parser
  where
    inject_double :: Int64 -> Conduit B.ByteString (ResourceT IO) B.ByteString
    inject_double i = do
      bs <- CB.take $ fromIntegral i
      CB.sourceLbs bs
      CB.sourceLbs bs

    parse_common_impl :: Int64 -> BitParser a -> Parser a
    parse_common_impl max_bytes parser = do
      (bs :: BL.ByteString) <- CB.take (fromIntegral max_bytes)
      (m,(p,_)) <- liftIO$ ML.runStateT (0, BL.unpack bs) parser
      CB.drop (ceiling$ p/8) -- ceiling for padding
      return m

parse_string :: Parser String
parse_string = do
  stringBytes <- CB.takeWhile (/= 0x00) =$ CL.consume
  CB.drop 1
  return$ BC.unpack $ Prelude.foldl B.append B.empty stringBytes

parse_abc :: Parser Swf
parse_abc = liftM3 Swf_DoABC readU32LE parse_string abc
  where
    abc :: Parser B.ByteString
    abc = await >>= maybe (fail "parse_abc") return

parse_symbol_class :: Parser Swf
parse_symbol_class = liftM Swf_SymbolClass $ readU16LE >>= replicateM' tagNamePair
  where
    tagNamePair :: Parser (Word16, String)
    tagNamePair = liftM2 (,) readU16LE parse_string

parse_tag :: Parser Swf
parse_tag = do
  (RecordHeader tag len) <- parse_record_header
  nextBytes <- CB.take$ fromIntegral len
  liftIO$ runResourceT (CB.sourceLbs nextBytes $$ tagChoice tag)
  where
    tagChoice :: Word16 -> Parser Swf
    tagChoice tag
      | tag == 0  {-                          End -} = return Swf_End
      | tag == 1  {-                    ShowFrame -} = return Swf_ShowFrame
      | tag == 2  {-                  DefineShape -} = return Swf_DefineShape
      | tag == 4  {-                  PlaceObject -} = return Swf_PlaceObject
      | tag == 5  {-                 RemoveObject -} = return Swf_RemoveObject
      | tag == 6  {-                   DefineBits -} = return Swf_DefineBits
      | tag == 7  {-                 DefineButton -} = return Swf_DefineButton
      | tag == 8  {-                   JPEGTables -} = return Swf_JPEGTables
      | tag == 9  {-           SetBackgroundColor -} = return Swf_SetBackgroundColor
      | tag == 10 {-                   DefineFont -} = return Swf_DefineFont
      | tag == 11 {-                   DefineText -} = return Swf_DefineText
      | tag == 12 {-                     DoAction -} = return Swf_DoAction
      | tag == 13 {-               DefineFontInfo -} = return Swf_DefineFontInfo
      | tag == 14 {-                  DefineSound -} = return Swf_DefineSound
      | tag == 15 {-                   StartSound -} = return Swf_StartSound
      | tag == 17 {-            DefineButtonSound -} = return Swf_DefineButtonSound
      | tag == 18 {-                 SoundbufHead -} = return Swf_SoundbufHead
      | tag == 19 {-                SoundbufBlock -} = return Swf_SoundbufBlock
      | tag == 20 {-           DefineBitsLossless -} = return Swf_DefineBitsLossless
      | tag == 21 {-              DefineBitsJPEG2 -} = return Swf_DefineBitsJPEG2
      | tag == 22 {-                 DefineShape2 -} = return Swf_DefineShape2
      | tag == 23 {-           DefineButtonCxform -} = return Swf_DefineButtonCxform
      | tag == 24 {-                      Protect -} = return Swf_Protect
      | tag == 26 {-                 PlaceObject2 -} = return Swf_PlaceObject2
      | tag == 28 {-                RemoveObject2 -} = return Swf_RemoveObject2
      | tag == 32 {-                 DefineShape3 -} = return Swf_DefineShape3
      | tag == 33 {-                  DefineText2 -} = return Swf_DefineText2
      | tag == 34 {-                DefineButton2 -} = return Swf_DefineButton2
      | tag == 35 {-              DefineBitsJPEG3 -} = return Swf_DefineBitsJPEG3
      | tag == 36 {-          DefineBitsLossless2 -} = return Swf_DefineBitsLossless2
      | tag == 37 {-               DefineEditText -} = return Swf_DefineEditText
      | tag == 39 {-                 DefineSprite -} = return Swf_DefineSprite
      | tag == 41 {-                  ProductInfo -} = return Swf_ProductInfo
      | tag == 43 {-                   FrameLabel -} = return Swf_FrameLabel
      | tag == 45 {-                SoundbufHead2 -} = return Swf_SoundbufHead2
      | tag == 46 {-             DefineMorphShape -} = return Swf_DefineMorphShape
      | tag == 48 {-                  DefineFont2 -} = return Swf_DefineFont2
      | tag == 56 {-                 ExportAssets -} = return Swf_ExportAssets
      | tag == 57 {-                 ImportAssets -} = return Swf_ImportAssets
      | tag == 58 {-               EnableDebugger -} = return Swf_EnableDebugger
      | tag == 59 {-                 DoInitAction -} = return Swf_DoInitAction
      | tag == 60 {-               DefineVideobuf -} = return Swf_DefineVideobuf
      | tag == 61 {-                   VideoFrame -} = return Swf_VideoFrame
      | tag == 62 {-              DefineFontInfo2 -} = return Swf_DefineFontInfo2
      | tag == 63 {-                      DebugId -} = return Swf_DebugId
      | tag == 64 {-              EnableDebugger2 -} = return Swf_EnableDebugger2
      | tag == 65 {-                 ScriptLimits -} = return Swf_ScriptLimits
      | tag == 66 {-                  SetTabIndex -} = return Swf_SetTabIndex
      | tag == 69 {-               FileAttributes -} = return Swf_FileAttributes
      | tag == 70 {-                 PlaceObject3 -} = return Swf_PlaceObject3
      | tag == 71 {-                ImportAssets2 -} = return Swf_ImportAssets2
      | tag == 73 {-         DefineFontAlignZones -} = return Swf_DefineFontAlignZones
      | tag == 74 {-              CSMTextSettings -} = return Swf_CSMTextSettings
      | tag == 75 {-                  DefineFont3 -} = return Swf_DefineFont3
      | tag == 76 {-                  SymbolClass -} = parse_symbol_class
      | tag == 77 {-                     Metadata -} = return Swf_Metadata
      | tag == 78 {-            DefineScalingGrid -} = return Swf_DefineScalingGrid
      | tag == 82 {-                        DoABC -} = parse_abc
      | tag == 83 {-                 DefineShape4 -} = return Swf_DefineShape4
      | tag == 84 {-            DefineMorphShape2 -} = return Swf_DefineMorphShape2
      | tag == 86 {- DefineSceneAndFrameLabelData -} = return Swf_DefineSceneAndFrameLabelData
      | tag == 87 {-             DefineBinaryData -} = return Swf_DefineBinaryData
      | tag == 88 {-               DefineFontName -} = return Swf_DefineFontName
      | tag == 89 {-                  StartSound2 -} = return Swf_StartSound2
      | tag == 90 {-              DefineBitsJPEG4 -} = return Swf_DefineBitsJPEG4
      | tag == 91 {-                  DefineFont4 -} = return Swf_DefineFont4
      | otherwise = error $ "parse_tag - unrecognized tag: " ++ show tag
