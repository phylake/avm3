module Data.Amf.Def (
  U8
, U30
, U32
, S24
, U29
, Assoc_Value
, Tables
, emptyTables
, Traits
, Amf(..)
, U29B(..)
, U29X
, U29O(U29O_Ref, U29O_TraitsRef, U29O_Traits)
, U29A(..)
, UTF_8_vr(..)
, AmfReference(..)
) where

import           Control.DeepSeq
import           Data.Int (Int32)
import           Data.Word (Word8, Word32)
import qualified Data.ByteString.Lazy as BL

class AmfReference a where
  toValue :: Tables -> a -> a

type U8 = Word8
type U30 = Word32
type U32 = Word32
type S24 = Int32
-- | Variable length integer
type U29 = Int
type Assoc_Value = (UTF_8_vr, Amf)

assocValue :: Tables -> Assoc_Value -> Assoc_Value
assocValue t (a,b) = (toValue t a, toValue t b)

-- | (Strings, Complex Object, Traits)
type Tables = ([String], [Amf], [Traits])

emptyTables :: Tables
emptyTables = ([], [], [])

-- | (class name, [properties], dynamic)
type Traits = (UTF_8_vr, [UTF_8_vr], Bool)

data Amf = {- 0x00 -} AmfUndefined          -- ^> undefined
         | {- 0x01 -} AmfNull               -- ^> null
         | {- 0x02 -} AmfFalse              -- ^> false
         | {- 0x03 -} AmfTrue               -- ^> true
         | {- 0x04 -} AmfInt Int32          -- ^> int
         | {- 0x05 -} AmfNumber Double      -- ^> Number
         | {- 0x06 -} AmfString UTF_8_vr    -- ^> String
         | {- 0x07 -} AmfXmlDoc U29X
         | {- 0x08 -} AmfDate String        -- ^> Date
         | {- 0x09 -} AmfArray U29A         -- ^> []
         | {- 0x0A -} AmfObject U29O        -- ^> {}
         | {- 0x0B -} AmfXml U29X           -- ^> XML
         | {- 0x0C -} AmfByteArray U29B     -- ^> flash.utils.ByteArray
         | {- 0x0D -} AmfVecInt [Int32]     -- ^> Vector.<int>
         | {- 0x0E -} AmfVecUInt [Word32]   -- ^> Vector.<uint>
         | {- 0x0F -} AmfVecNumber [Double] -- ^> Vector.<Number>
         | {- 0x10 -} AmfVecObject [Amf]    -- ^> Vector.<Object>
         | {- 0x11 -} AmfDictionary [Amf]   -- ^> flash.utils.Dictionary
         deriving (Eq)

instance NFData Amf where
  rnf (AmfInt a) = a `deepseq` ()
  rnf (AmfNumber a) = a `deepseq` ()
  rnf (AmfString a) = a `deepseq` ()
  rnf (AmfXmlDoc a) = a `deepseq` ()
  rnf (AmfDate a) = a `deepseq` ()
  rnf (AmfArray a) = a `deepseq` ()
  rnf (AmfObject a) = a `deepseq` ()
  rnf (AmfXml a) = a `deepseq` ()
  rnf (AmfByteArray a) = a `deepseq` ()
  rnf (AmfVecInt a) = a `deepseq` ()
  rnf (AmfVecUInt a) = a `deepseq` ()
  rnf (AmfVecNumber a) = a `deepseq` ()
  rnf (AmfVecObject a) = a `deepseq` ()
  rnf (AmfDictionary a) = a `deepseq` ()
  rnf _ = ()

instance Show Amf where
  show AmfUndefined      = "undefined"
  show AmfNull           = "null"
  show AmfFalse          = "false"
  show AmfTrue           = "true"
  show (AmfInt a)        = show a
  show (AmfNumber a)     = show a
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
  show (AmfVecNumber a)  = "Vector.<Number>[" ++ show a ++ "]"
  show (AmfVecObject a)  = "Vector.<Object>[" ++ show a ++ "]"
  show (AmfDictionary a) = "AmfDictionary"

instance AmfReference Amf where
  toValue _ a@AmfUndefined = a
  toValue _ a@AmfNull = a
  toValue _ a@AmfFalse = a
  toValue _ a@AmfTrue = a
  toValue _ a@(AmfInt _) = a
  toValue _ a@(AmfNumber _) = a
  toValue t (AmfString a) = AmfString $ toValue t a
  toValue _ a@(AmfXmlDoc _) = a
  toValue _ a@(AmfDate _) = a
  toValue t (AmfArray (U29A_Value a b)) = AmfArray (U29A_Value a2 b2) where
    a2 = map (assocValue t) a
    b2 = map (toValue t) b
  toValue t@(_,cot,_) (AmfArray (U29A_Ref a)) = toValue t $ cot !! a
  toValue t@(_,cot,_) (AmfObject (U29O_Ref a)) = toValue t $ cot !! a
  toValue t@(_,cot,tt) (AmfObject (U29O_TraitsRef a b c)) = obj where
    obj = AmfObject $ U29O_Traits a2 b2 c2
    (klass, keys, _) = tt !! a
    a2 = klass
    b2 = zip keys $ map (toValue t) b
    c2 = map (assocValue t) c
  toValue _ a@(AmfObject (U29O_TraitsExt _ _)) = a
  toValue t (AmfObject (U29O_Traits a b c)) = obj where
    obj = AmfObject $ U29O_Traits a2 b2 c2
    a2 = toValue t a
    b2 = map (assocValue t) b
    c2 = map (assocValue t) c
  toValue t (AmfXml a) = AmfXml $ toValue t a
  toValue (_,cot,_) (AmfByteArray (U29B_Ref a)) = cot !! a
  toValue _ a@(AmfByteArray (U29B_Value _)) = a
  toValue _ a@(AmfVecInt _ ) = a
  toValue _ a@(AmfVecUInt _ ) = a
  toValue _ a@(AmfVecNumber _ ) = a
  toValue t (AmfVecObject a) = AmfVecObject $ map (toValue t) a
  toValue t (AmfDictionary a) = AmfDictionary $ map (toValue t) a

-- | AmfByteArray
-- 
-- > U29O-ref |
-- > U29B-value U8*
data U29B = U29B_Ref U29
          | U29B_Value BL.ByteString
          deriving (Show, Eq)

instance NFData U29B where
  rnf (U29B_Ref a) = a `deepseq` ()
  rnf (U29B_Value a) = ()

-- | AmfXml and AmfXmlDoc
-- 
-- > U29O-ref |
-- > U29X-value UTF8-char*
type U29X = UTF_8_vr

-- | AmfObject
-- 
-- > (
-- >     U29O-ref |
-- >     U29O-traits-ref |
-- >     U29O-traits-ext class-name U8* |
-- >     U29O-traits class-name UTF-8-vr*
-- > )
-- > value-type* dynamic-member*
data U29O = U29O_Ref U29
          | U29O_TraitsRef U29 [Amf] [Assoc_Value] -- ^ trait ref, fixed member values, dynamic member keys and values
          | U29O_TraitsExt UTF_8_vr [Word8] {-[Amf] [Assoc_Value]-} --TODO handle this
          | U29O_Traits UTF_8_vr [Assoc_Value] [Assoc_Value] -- ^ class name, fixed member keys and values, dynamic member keys and values
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

-- | AmfArray
-- 
-- >     U29O-ref |
-- > (
-- >     U29A-value
-- >     (UTF-8-empty | assoc-value* UTF-8-empty)
-- >     value-type*
-- > )
data U29A = U29A_Ref U29
          | U29A_Value [Assoc_Value] [Amf] -- ^ associative portion, dense portion
          deriving (Eq)

instance Show U29A where
  show (U29A_Ref a) = "U29A-ref " ++ show a
  show (U29A_Value a b) = "U29A-value " ++ show a ++ " " ++ show b

instance NFData U29A where
  rnf (U29A_Ref a) = a `deepseq` ()
  rnf (U29A_Value a b) = a `deepseq` b `deepseq` ()

data UTF_8_vr = U29S_Ref U29
              | U29S_Value String
              deriving (Eq)

instance Show UTF_8_vr where
  show (U29S_Ref a) = "U29S-ref " ++ show a
  show (U29S_Value a) = "U29S-value " ++ show a

instance NFData UTF_8_vr where
  rnf (U29S_Ref a) = a `deepseq` ()
  rnf (U29S_Value a) = a `deepseq` ()

instance AmfReference UTF_8_vr where
  toValue _ a@(U29S_Value _) = a
  toValue (st,_,_) (U29S_Ref a) = U29S_Value $ st !! a
