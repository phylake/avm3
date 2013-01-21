module Amf.Def where

import           Control.DeepSeq
import           Control.Monad.State (StateT)
import           Control.Monad.Writer
import           Data.Enumerator as E
import           Data.Enumerator.Binary as EB
import           Data.Enumerator.List as EL
import           Data.Int
import           Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified MonadLib as ML

type Parser = Iteratee B.ByteString (ML.StateT Tables IO)

type U8 = Word8
type U30 = Word32
type U32 = Word32
type S24 = Int32

type AmfErr = String
type U29 = Int
type Assoc_Value = (UTF_8_vr, Amf)
type AmfT = (Acc Word8, [Amf])

-- (Strings, Complex Object, Traits)
type Tables = ([String], [Amf], [Traits])

-- (class name, [properties], dynamic)
type Traits = (UTF_8_vr, [UTF_8_vr], Bool)

class AmfPrim a where
  fromAmf :: Acc Word8 -> Either AmfErr (Acc Word8, a)
  fromAmf2 :: Parser a
  fromAmf2 = undefined

{-toAmf :: Acc Amf -> Either AmfErr (Acc Amf, DiffList Word8)
toAmf = undefined-}

data Acc a = Acc {
                   accAs  :: [a]
                 , accBs  :: Int
                 , accTs  :: Tables
                 , accLog :: [String]
                 }

instance (NFData a) => NFData (Acc a) where
  rnf (Acc a b c d) = a
    `deepseq` b
    `deepseq` c
    `deepseq` d
    `deepseq` ()

data Amf = {- 0x0 -} AmfUndefined {- undefined-marker -}
         | {- 0x1 -} AmfNull      {- null-marker -}
         | {- 0x2 -} AmfFalse     {- false-marker -}
         | {- 0x3 -} AmfTrue      {- true-marker -}
         | {- 0x4 -} AmfInt U29
         | {- 0x5 -} AmfDouble Double
         | {- 0x6 -} AmfString UTF_8_vr
         | {- 0x7 -} AmfXmlDoc U29X
         | {- 0x8 -} AmfDate String
         | {- 0x9 -} AmfArray U29A
         | {- 0xA -} AmfObject U29O
         | {- 0xB -} AmfXml U29X
         | {- 0xC -} AmfByteArray U29B
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
  rnf _ = ()

instance Show Amf where
  show AmfUndefined      = "undefined"
  show AmfNull           = "null"
  show AmfFalse          = "false"
  show AmfTrue           = "true"
  show (AmfInt v)        = show v
  show (AmfDouble v)     = show v
  show (AmfString v)     = show v
  show (AmfXmlDoc v)     = show v
  show (AmfDate v)       = show v
  show (AmfArray xs)     = show xs
  --show (AmfObject hs) = show $ map (\(k,v) -> show k ++ ": " ++ show v) hs
  show (AmfObject v)     = show v
  show (AmfXml v)        = show v
  show (AmfByteArray ba) = "bytearray"

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
          | U29O_TraitsRef [Assoc_Value]
          | U29O_TraitsExt UTF_8_vr [Word8] {-[Amf] [Assoc_Value]-} --TODO handle this
          | U29O_Traits [Assoc_Value]
          deriving (Eq)

instance NFData U29O where
  rnf (U29O_Ref a) = a `deepseq` ()
  rnf (U29O_TraitsRef a) = a `deepseq` ()
  rnf (U29O_TraitsExt a b) = a `deepseq` b `deepseq` ()
  rnf (U29O_Traits a) = a `deepseq` ()

instance Show U29O where
  show (U29O_Ref u29) = "U29O-ref " ++ show u29
  show (U29O_TraitsRef assocs) = "U29O-traits-ref " ++ show assocs
  show (U29O_Traits assocs)    = "U29O-traits " ++ show assocs
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
          deriving (Show, Eq)

instance NFData U29A where
  rnf (U29A_Ref a) = a `deepseq` ()
  rnf (U29A_Value a b) = a `deepseq` b `deepseq` ()

{-
    Char
-}

data UTF_8_vr = U29S_Ref U29
              | U29S_Value String
              deriving (Show, Eq)

instance NFData UTF_8_vr where
  rnf (U29S_Ref a) = a `deepseq` ()
  rnf (U29S_Value a) = a `deepseq` ()
