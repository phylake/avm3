module Data.Amf.Def (
  U8
, U30
, U32
, S24
, U29
, Assoc_Value
, Tables
, Traits
, Amf(..)
, U29B(..)
, U29X
, U29O(U29O_Ref, U29O_TraitsRef, U29O_Traits)
, U29A(..)
, UTF_8_vr(..)
) where

import           Control.DeepSeq
import           Data.Int (Int32)
import           Data.Word (Word8, Word32)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

type U8 = Word8
type U30 = Word32
type U32 = Word32
type S24 = Int32
type U29 = Int
type Assoc_Value = (UTF_8_vr, Amf)

-- (Strings, Complex Object, Traits)
type Tables = ([String], [Amf], [Traits])

-- (class name, [properties], dynamic)
type Traits = (UTF_8_vr, [UTF_8_vr], Bool)

data Amf = {- 0x00 -} AmfUndefined -- ^ undefined-marker
         | {- 0x01 -} AmfNull      -- ^ null-marker
         | {- 0x02 -} AmfFalse     -- ^ false-marker
         | {- 0x03 -} AmfTrue      -- ^ true-marker
         | {- 0x04 -} AmfInt Int32
         | {- 0x05 -} AmfDouble Double
         | {- 0x06 -} AmfString UTF_8_vr
         | {- 0x07 -} AmfXmlDoc U29X
         | {- 0x08 -} AmfDate String
         | {- 0x09 -} AmfArray U29A
         | {- 0x0A -} AmfObject U29O
         | {- 0x0B -} AmfXml U29X
         | {- 0x0C -} AmfByteArray U29B
         | {- 0x0D -} AmfVecInt [Int32]
         | {- 0x0E -} AmfVecUInt [Word32]
         | {- 0x0F -} AmfVecDouble [Double]
         | {- 0x10 -} AmfVecObject [Amf]
         | {- 0x11 -} AmfDictionary [Amf]
         deriving (Eq)

instance NFData Amf where
  rnf (AmfInt a) = a `deepseq` ()
  rnf (AmfDouble a) = a `deepseq` ()
  rnf (AmfString a) = a `deepseq` ()
  rnf (AmfXmlDoc a) = a `deepseq` ()
  rnf (AmfDate a) = a `deepseq` ()
  rnf (AmfArray a) = a `deepseq` ()
  rnf (AmfObject a) = a `deepseq` ()
  rnf (AmfXml a) = a `deepseq` ()
  rnf (AmfByteArray a) = a `deepseq` ()
  rnf (AmfVecInt a) = a `deepseq` ()
  rnf (AmfVecUInt a) = a `deepseq` ()
  rnf (AmfVecDouble a) = a `deepseq` ()
  rnf (AmfVecObject a) = a `deepseq` ()
  rnf (AmfDictionary a) = a `deepseq` ()
  rnf _ = ()

instance Show Amf where
  show AmfUndefined      = "undefined"
  show AmfNull           = "null"
  show AmfFalse          = "false"
  show AmfTrue           = "true"
  show (AmfInt a)        = show a
  show (AmfDouble a)     = show a
  show (AmfString a)     = show a
  show (AmfXmlDoc a)     = show a
  show (AmfDate a)       = show a
  show (AmfArray xs)     = show xs
  --show (AmfObject hs) = show $ map (\(k,v) -> show k ++ ": " ++ show v) hs
  show (AmfObject a)     = show a
  show (AmfXml a)        = show a
  show (AmfByteArray _)  = "bytearray"
  show (AmfVecInt a)     = "Vector.<int>[" ++ show a ++ "]"
  show (AmfVecUInt a)    = "Vector.<uint>[" ++ show a ++ "]"
  show (AmfVecDouble a)  = "Vector.<Number>[" ++ show a ++ "]"
  show (AmfVecObject a)  = "Vector.<Object>[" ++ show a ++ "]"
  show (AmfDictionary a) = "AmfDictionary"

{-
    AmfByteArray

    U29O-ref |
    U29B-value U8*
-}

data U29B = U29B_Ref U29
          | U29B_Value [Word8]
          deriving (Show, Eq)

instance NFData U29B where
  rnf (U29B_Ref a) = a `deepseq` ()
  rnf (U29B_Value a) = a `deepseq` ()

{-
    AmfXml and AmfXmlDoc

    U29O-ref |
    U29X-value UTF8-char*
-}

type U29X = UTF_8_vr

{-
    AmfObject

    (
        U29O-ref |
        U29O-traits-ref |
        U29O-traits-ext class-name U8* |
        U29O-traits class-name UTF-8-vr*
    )
    value-type* dynamic-member*
-}

data U29O = U29O_Ref U29
          | U29O_TraitsRef U29 [Amf] [Assoc_Value] -- trait ref, list of fixed member values, dynamic members
          | U29O_TraitsExt UTF_8_vr [Word8] {-[Amf] [Assoc_Value]-} --TODO handle this
          | U29O_Traits UTF_8_vr [Assoc_Value] [Assoc_Value] -- class name, fixed member keys and values, dynamic members
          deriving (Eq)

instance NFData U29O where
  rnf (U29O_Ref a) = a `deepseq` ()
  rnf (U29O_TraitsRef a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (U29O_TraitsExt a b) = a `deepseq` b `deepseq` ()
  rnf (U29O_Traits a b c) = a `deepseq` b `deepseq` c `deepseq` ()

instance Show U29O where
  show (U29O_Ref u29) = "U29O-ref " ++ show u29
  show (U29O_TraitsRef u29 fixed dynam) = "U29O-traits-ref " ++ show u29 ++ " " ++ show fixed ++ " " ++ show dynam
  show (U29O_Traits klass fixed dynam) = "U29O-traits "
    ++ show klass ++ " "
    ++ show fixed ++ " "
    ++ show dynam
  show (U29O_TraitsExt _ _) = show "U29O-traits-ext"

{-
    AmfArray

        U29O-ref |
    (
        U29A-value
        (UTF-8-empty | assoc-value* UTF-8-empty)
        value-type*
    )
-}

data U29A = U29A_Ref U29
          | U29A_Value [Assoc_Value] [Amf]
          deriving (Eq)

instance Show U29A where
  show (U29A_Ref a) = "U29A-ref " ++ show a
  show (U29A_Value a b) = "U29A-value " ++ show a ++ " " ++ show b

instance NFData U29A where
  rnf (U29A_Ref a) = a `deepseq` ()
  rnf (U29A_Value a b) = a `deepseq` b `deepseq` ()

{-
    Char
-}

data UTF_8_vr = U29S_Ref U29
              | U29S_Value String
              deriving (Eq)

instance Show UTF_8_vr where
  show (U29S_Ref a) = "U29S-ref " ++ show a
  show (U29S_Value a) = "U29S-value " ++ show a

instance NFData UTF_8_vr where
  rnf (U29S_Ref a) = a `deepseq` ()
  rnf (U29S_Value a) = a `deepseq` ()
