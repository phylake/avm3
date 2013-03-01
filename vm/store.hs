module Vm.Store (
  build_cp
, get_int
, get_uint
, get_double
, get_string
, get_nsInfo
, get_nsSet
, get_multiname
, get_methodSig
, get_metadata
, get_instance
, get_class
, get_script
, get_methodBody
) where

import           Abc.Def
import           Control.Applicative ((<$>))
import           Data.Bits
import           Data.Int
import           Data.Word
import           MonadLib hiding (get, set)
import           Util.Misc (t44)
import           Vm.Def
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashTable.IO as H

key_int :: B.ByteString
key_int = BC.pack "_int"

key_uint :: B.ByteString
key_uint = BC.pack "_uint"

key_double :: B.ByteString
key_double = BC.pack "_double"

key_string :: B.ByteString
key_string = BC.pack "_string"

key_nsInfo :: B.ByteString
key_nsInfo = BC.pack "_nsInfo"

key_nsSet :: B.ByteString
key_nsSet = BC.pack "_nsSet"

key_multiname :: B.ByteString
key_multiname = BC.pack "_multiname"

key_methodSig :: B.ByteString
key_methodSig = BC.pack "_methodSig"

key_metadata :: B.ByteString
key_metadata = BC.pack "_metadata"

key_instance :: B.ByteString
key_instance = BC.pack "_instance"

key_class :: B.ByteString
key_class = BC.pack "_class"

key_script :: B.ByteString
key_script = BC.pack "_script"

key_methodBody :: B.ByteString
key_methodBody = BC.pack "_methodBody"

build_cp :: Abc -> IO ConstantPool
build_cp abc = do
  cp <- H.new
  mapM_ (\(idx, a) -> put_int cp idx a) $ zip (map fromIntegral [0..length ints]) ints
  mapM_ (\(idx, a) -> put_uint cp idx a) $ zip (map fromIntegral [0..length uints]) uints
  mapM_ (\(idx, a) -> put_double cp idx a) $ zip (map fromIntegral [0..length doubles]) doubles
  mapM_ (\(idx, a) -> put_string cp idx a) $ zip (map fromIntegral [0..length strings]) strings
  mapM_ (\(idx, a) -> put_nsInfo cp idx a) $ zip (map fromIntegral [0..length nsInfo]) nsInfo
  mapM_ (\(idx, a) -> put_nsSet cp idx a) $ zip (map fromIntegral [0..length nsSet]) nsSet
  mapM_ (\(idx, a) -> put_multiname cp idx a) $ zip (map fromIntegral [0..length multinames]) multinames
  mapM_ (\(idx, a) -> put_methodSig cp idx a) $ zip (map fromIntegral [0..length methodSigs]) methodSigs
  mapM_ (\(idx, a) -> put_metadata cp idx a) $ zip (map fromIntegral [0..length metadata]) metadata
  mapM_ (\(idx, a) -> put_instance cp idx a) $ zip (map fromIntegral [0..length instances]) instances
  mapM_ (\(idx, a) -> put_class cp idx a) $ zip (map fromIntegral [0..length classes]) classes
  mapM_ (\(idx, a) -> put_script cp idx a) $ zip (map fromIntegral [0..length scripts]) scripts
  -- reverse lookup: get_methodBody expects a method signature index since a
  -- method body index doesn't exist
  mapM_ (\(idx, a) -> put_methodBody cp idx a) $ zip (map mbMethod methodBodies) methodBodies
  return cp
  where
    ints = abcInts abc
    uints = abcUints abc
    doubles = abcDoubles abc
    strings = xform_strings$ abcStrings abc
    nsInfo = abcNsInfo abc
    nsSet = abcNsSet abc
    multinames = abcMultinames abc
    methodSigs = abcMethodSigs abc
    metadata = abcMetadata abc
    instances = abcInstances abc
    classes = abcClasses abc
    scripts = abcScripts abc
    methodBodies = xform_methodBodies$ abcMethodBodies abc

xform_strings :: [String] -> [B.ByteString]
xform_strings = map BC.pack

xform_methodBodies :: [MethodBody] -> [MethodBody]
xform_methodBodies = map f where
  f (MethodBody a b c d e code f g) = MethodBody a b c d e newCode f g
    where
      newCode = foldr replaceGetLex [] code
  replaceGetLex (GetLex idx) acc = [FindPropStrict idx, GetProperty idx] ++ acc
  replaceGetLex op acc = op:acc

get_int :: ConstantPool -> U30 -> IO Int32
get_int cp u30 = do VmAbc_Int a <- get_ht cp key_int u30;return a

put_int :: ConstantPool -> U30 -> Int32 -> IO ()
put_int cp k v = put_ht cp key_int k $ VmAbc_Int v

get_uint :: ConstantPool -> U30 -> IO U30
get_uint cp u30 = do VmAbc_Uint a <- get_ht cp key_uint u30;return a

put_uint :: ConstantPool -> U30 -> U30 -> IO ()
put_uint cp k v = put_ht cp key_uint k $ VmAbc_Uint v

get_double :: ConstantPool -> U30 -> IO Double
get_double cp u30 = do VmAbc_Double a <- get_ht cp key_double u30;return a

put_double :: ConstantPool -> U30 -> Double -> IO ()
put_double cp k v = put_ht cp key_double k $ VmAbc_Double v

get_string :: ConstantPool -> U30 -> IO B.ByteString
get_string cp u30 = do VmAbc_String a <- get_ht cp key_string u30;return a

put_string :: ConstantPool -> U30 -> B.ByteString -> IO ()
put_string cp k v = put_ht cp key_string k $ VmAbc_String v

get_nsInfo :: ConstantPool -> U30 -> IO NSInfo
get_nsInfo cp u30 = do VmAbc_NsInfo a <- get_ht cp key_nsInfo u30;return a

put_nsInfo :: ConstantPool -> U30 -> NSInfo -> IO ()
put_nsInfo cp k v = put_ht cp key_nsInfo k $ VmAbc_NsInfo v

get_nsSet :: ConstantPool -> U30 -> IO NSSet
get_nsSet cp u30 = do VmAbc_NsSet a <- get_ht cp key_nsSet u30;return a

put_nsSet :: ConstantPool -> U30 -> NSSet -> IO ()
put_nsSet cp k v = put_ht cp key_nsSet k $ VmAbc_NsSet v

get_multiname :: ConstantPool -> U30 -> IO Multiname
get_multiname cp u30 = do VmAbc_Multiname a <- get_ht cp key_multiname u30;return a

put_multiname :: ConstantPool -> U30 -> Multiname -> IO ()
put_multiname cp k v = put_ht cp key_multiname k $ VmAbc_Multiname v

get_methodSig :: ConstantPool -> U30 -> IO MethodSignature
get_methodSig cp u30 = do VmAbc_MethodSig a <- get_ht cp key_methodSig u30;return a

put_methodSig :: ConstantPool -> U30 -> MethodSignature -> IO ()
put_methodSig cp k v = put_ht cp key_methodSig k $ VmAbc_MethodSig v

get_metadata :: ConstantPool -> U30 -> IO Metadata
get_metadata cp u30 = do VmAbc_Metadata a <- get_ht cp key_metadata u30;return a

put_metadata :: ConstantPool -> U30 -> Metadata -> IO ()
put_metadata cp k v = put_ht cp key_metadata k $ VmAbc_Metadata v

get_instance :: ConstantPool -> U30 -> IO InstanceInfo
get_instance cp u30 = do VmAbc_Instance a <- get_ht cp key_instance u30;return a

put_instance :: ConstantPool -> U30 -> InstanceInfo -> IO ()
put_instance cp k v = put_ht cp key_instance k $ VmAbc_Instance v

get_class :: ConstantPool -> U30 -> IO ClassInfo
get_class cp u30 = do VmAbc_Class a <- get_ht cp key_class u30;return a

put_class :: ConstantPool -> U30 -> ClassInfo -> IO ()
put_class cp k v = put_ht cp key_class k $ VmAbc_Class v

get_script :: ConstantPool -> U30 -> IO ScriptInfo
get_script cp u30 = do VmAbc_Script a <- get_ht cp key_script u30;return a

put_script :: ConstantPool -> U30 -> ScriptInfo -> IO ()
put_script cp k v = put_ht cp key_script k $ VmAbc_Script v

get_methodBody :: ConstantPool -> U30 -> IO MethodBody
get_methodBody cp u30 = do VmAbc_MethodBody a <- get_ht cp key_methodBody u30;return a

put_methodBody :: ConstantPool -> U30 -> MethodBody -> IO ()
put_methodBody cp k v = put_ht cp key_methodBody k $ VmAbc_MethodBody v

get_ht :: ConstantPool -> B.ByteString -> U30 -> IO VmAbc
get_ht ht prefix k = do
  --liftIO.putStrLn$ "prefix " ++ show prefix ++ show k
  m <- H.lookup ht fullKey
  case m of
    Nothing -> fail$ "get_ht - " ++ (show k ++ BC.unpack prefix)
    Just ret -> return ret
  where
    fullKey = foldr B.cons prefix$ u30ToWord8 k

put_ht :: ConstantPool -> B.ByteString -> U30 -> VmAbc -> IO ()
put_ht cp prefix k v = do
  --liftIO.putStrLn$ "prefix " ++ show prefix ++ show k
  H.insert cp fullKey v
  where
    fullKey = foldr B.cons prefix$ u30ToWord8 k

u30ToWord8 :: U30 -> [Word8]
u30ToWord8 u30 = [msb0, msb1, msb2, msb3]
  where
    msb0 = fromIntegral$ u30 `shiftR` 24 .&. 0xff
    msb1 = fromIntegral$ u30 `shiftR` 16 .&. 0xff
    msb2 = fromIntegral$ u30 `shiftR`  8 .&. 0xff
    msb3 = fromIntegral$ u30 `shiftR`  0 .&. 0xff
{-# INLINE u30ToWord8 #-}
