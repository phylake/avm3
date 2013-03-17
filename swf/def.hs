module Swf.Def where

import           Data.Conduit
import           Data.Int
import           Data.Word
import qualified Data.ByteString as B
import qualified MonadLib as ML

type Parser a = Sink B.ByteString (ResourceT IO) a
type State = ML.StateT
type BitParser = State (Float, [Word8]) IO

type U8 = Word8
type U30 = Word32
type U32 = Word32
type S24 = Int32

{-instance NFData NSInfo where
    rnf (NSInfo_Namespace a) = a `deepseq` ()-}

{-instance NFData Abc where
    rnf (Abc a b) = a
        `deepseq` b
        `deepseq` ()-}

class SwfPrim a where
    --fromSwf :: DBL.ByteString -> Either String (a, DBL.ByteString)
    fromSwf :: Parser a

data RGB = RGB Word8 Word8 Word8
         | RGBA Word8 Word8 Word8 Word8
         | ARGB Word8 Word8 Word8 Word8
         deriving (Show)

data Rect = Rect {
                   xMin :: Word32
                 , xMax :: Word32
                 , yMin :: Word32
                 , yMax :: Word32
                 }
                 deriving (Show)

data Matrix = Matrix {
                       scaleX :: Word32
                     , scaleY :: Word32
                     , rotateSkew0 :: Word32
                     , rotateSkew1 :: Word32
                     , translateX :: Word32
                     , translateY :: Word32
                     }
                     deriving (Show)

data ColorXForm = ColorXForm {
                               rM :: Word16
                             , gM :: Word16
                             , bM :: Word16
                             , aM :: Maybe Word16
                             , rA :: Word16
                             , gA :: Word16
                             , bA :: Word16
                             , aA :: Maybe Word16
                             }
                             deriving (Show)

{-data SwfFile = SwfFile {
                         version :: Word8
                       , file_length :: Word32
                       , frame_size :: Rect
                       , frame_rate :: Word8
                       , frame_count :: Word8
                       , tags :: [Tag]
                       }
                       deriving (Show)-}

data RecordHeader = RecordHeader {
                                   rhTag :: Word16    -- tag
                                 , rhLength :: Word32 -- tag length
                                 }
                                 deriving (Show)

data Tag = Definition RecordHeader Swf
         | Control
         deriving (Show)

{- chapter 3 The Display List -}
data PlaceObject = PlaceObject {
                                 poCharId :: Word16
                               , poDepth :: Word16
                               , poMatrix :: Matrix
                               , poCxform :: ColorXForm
                               }
                               deriving (Show)

data PlaceObject2 = PlaceObject2 {
                                   po2Depth :: Word16
                                 , po2CharId :: Maybe Word16
                                 , po2Matrix :: Maybe Matrix
                                 , po2Cxform :: Maybe ColorXForm
                                 , po2Ratio :: Maybe Word16
                                 , po2Name :: Maybe String
                                 , po2ClipDepth :: Maybe Word16
                                 , po2ClipActions :: Maybe ClipActions
                                 }
                                 deriving (Show)

data ClipEventFlags = ClipEventConstruct
                    | ClipEventData
                    | ClipEventDragOut
                    | ClipEventDragOver
                    | ClipEventEnterFrame
                    | ClipEventInitialize
                    | ClipEventKeyDown
                    | ClipEventKeyPress
                    | ClipEventKeyUp
                    | ClipEventLoad
                    | ClipEventMouseDown
                    | ClipEventMouseMove
                    | ClipEventMouseUp
                    | ClipEventPress
                    | ClipEventRelease
                    | ClipEventReleaseOutside
                    | ClipEventRollOut
                    | ClipEventRollOver
                    | ClipEventUnload
                    deriving (Show)

data ClipActions = ClipActions {
                                 caAllEventFlags :: ClipEventFlags
                               , caActionRecords :: [ClipActionRecord]
                               }
                               deriving (Show)

data ClipActionRecord = ClipActionRecord {
                                           carEventFlags :: ClipEventFlags
                                         , carKeycode :: Maybe Word8
                                         , carActions :: [ActionRecord]
                                         }
                                         deriving (Show)

data BlendMode = {-0,1 -} Normal
               | {-  2 -} Layer
               | {-  3 -} Multiply
               | {-  4 -} Screen
               | {-  5 -} Lighten
               | {-  6 -} Darken
               | {-  7 -} Difference
               | {-  8 -} Add
               | {-  9 -} Subtract
               | {- 10 -} Invert
               | {- 11 -} Alpha
               | {- 12 -} Erase
               | {- 13 -} Overlay
               | {- 14 -} Hardlight
               deriving (Show)

data PlaceObject3 = PlaceObject3 {
                                   po3Depth :: Word16
                                 , po3ClassName :: Maybe String
                                 , po3CharId :: Maybe Word16
                                 , po3Matrix :: Maybe Matrix
                                 , po3Cxform :: Maybe ColorXForm
                                 , po3Ratio :: Maybe Word16
                                 , po3Name :: Maybe String
                                 , po3ClipDepth :: Maybe Word16
                                 , po3SurfaceFilterList :: [Filter]
                                 , po3BlendMode :: BlendMode
                                 , po3BitmapCache :: Bool
                                 , po3ClipActions :: Maybe ClipActions
                                 }
                                 deriving (Show)

type ColorMatrixFilter = [Word8]

data ConvolutionFilter = ConvolutionFilter {
                                             convfDivisor :: Float
                                           , convfBias :: Float
                                           , convfMatrix :: [Word8]
                                           , convfDefaultColor :: RGB
                                           , convfClamp :: Bool
                                           , convfPreserveAlpha :: Bool
                                           }
                                           deriving (Show)

data BlurFilter = BlurFilter {
                               blurfBlurX :: Float
                             , blurfBlurY :: Float
                             , blurfPasses :: Word8
                             }
                             deriving (Show)

data DropShadowFilter = DropShadowFilter {
                                           dsfColor :: RGB
                                         , dsfBlurX :: Float
                                         , dsfBlurY :: Float
                                         , dsfAngle :: Float
                                         , dsfDistance :: Float
                                         , dsfStrength :: Float
                                         , dsfInnerShadow :: Bool
                                         , dsfKnockout :: Bool
                                         , dsfPasses :: Word8
                                         }
                                         deriving (Show)

data GlowFilter = GlowFilter {
                               gfColor :: RGB
                             , gfBlurX :: Float
                             , gfBlurY :: Float
                             , gfStrength :: Float
                             , gfInnerGlow :: Bool
                             , gfKnockout :: Bool
                             , gfPasses :: Word8
                             }
                             deriving (Show)

data BevelFilter = BevelFilter {
                                 bevfShadowColor :: RGB
                               , bevfHighlightColor :: RGB
                               , bevfBlurX :: Float
                               , bevfBlurY :: Float
                               , bevfAngle :: Float
                               , bevfDistance :: Float
                               , bevfStrength :: Float
                               , bevfInnerShadow :: Bool
                               , bevfKnockout :: Bool
                               , bevfOnTop :: Bool
                               , bevfPasses :: Word8
                               }
                               deriving (Show)

data GradientFilter = GradientFilter deriving (Show)
data GradientBevelFilter = GradientBevelFilter deriving (Show)

data Filter = {- 0 -} DropShadow DropShadowFilter
            | {- 1 -} Blur BlurFilter
            | {- 2 -} Glow GlowFilter
            | {- 3 -} Bevel BevelFilter
            | {- 4 -} Gradient GradientFilter
            | {- 5 -} Convolution ConvolutionFilter
            | {- 6 -} ColorMatrix ColorMatrixFilter
            | {- 7 -} GradientBevel GradientBevelFilter
            deriving (Show)


{- chapter 5 Actions -}
data ActionRecord = ActionRecord deriving (Show)

{- !undocumented tag! -}
data Swf = Swf_Header Word8 Word32 Rect Float Word16
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
         | {- 82 -} Swf_DoABC Word32 String B.ByteString
         | {- 83 -} Swf_DefineShape4
         | {- 84 -} Swf_DefineMorphShape2
         | {- 86 -} Swf_DefineSceneAndFrameLabelData
         | {- 87 -} Swf_DefineBinaryData
         | {- 88 -} Swf_DefineFontName
         | {- 89 -} Swf_StartSound2
         | {- 90 -} Swf_DefineBitsJPEG4
         | {- 91 -} Swf_DefineFont4
         deriving (Show)

