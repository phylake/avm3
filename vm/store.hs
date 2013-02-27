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

build_cp :: Abc -> AVM3 ()
build_cp abc = do
  mapM_ (\(idx, a) -> put_int idx a) $ zip (map fromIntegral [0..length ints]) ints
  mapM_ (\(idx, a) -> put_uint idx a) $ zip (map fromIntegral [0..length uints]) uints
  mapM_ (\(idx, a) -> put_double idx a) $ zip (map fromIntegral [0..length doubles]) doubles
  mapM_ (\(idx, a) -> put_string idx a) $ zip (map fromIntegral [0..length strings]) strings
  mapM_ (\(idx, a) -> put_nsInfo idx a) $ zip (map fromIntegral [0..length nsInfo]) nsInfo
  mapM_ (\(idx, a) -> put_nsSet idx a) $ zip (map fromIntegral [0..length nsSet]) nsSet
  mapM_ (\(idx, a) -> put_multiname idx a) $ zip (map fromIntegral [0..length multinames]) multinames
  mapM_ (\(idx, a) -> put_methodSig idx a) $ zip (map fromIntegral [0..length methodSigs]) methodSigs
  mapM_ (\(idx, a) -> put_metadata idx a) $ zip (map fromIntegral [0..length metadata]) metadata
  mapM_ (\(idx, a) -> put_instance idx a) $ zip (map fromIntegral [0..length instances]) instances
  mapM_ (\(idx, a) -> put_class idx a) $ zip (map fromIntegral [0..length classes]) classes
  mapM_ (\(idx, a) -> put_script idx a) $ zip (map fromIntegral [0..length scripts]) scripts
  -- reverse lookup: get_methodBody expects a method signature index since a
  -- method body index doesn't exist
  mapM_ (\(idx, a) -> put_methodBody idx a) $ zip (map mbMethod methodBodies) methodBodies
  where
    ints = abcInts abc
    uints = abcUints abc
    doubles = abcDoubles abc
    strings = abcStrings abc
    nsInfo = abcNsInfo abc
    nsSet = abcNsSet abc
    multinames = abcMultinames abc
    methodSigs = abcMethodSigs abc
    metadata = abcMetadata abc
    instances = abcInstances abc
    classes = abcClasses abc
    scripts = abcScripts abc
    methodBodies = xform_methodBodies$ abcMethodBodies abc

xform_methodBodies :: [MethodBody] -> [MethodBody]
xform_methodBodies = map f where
  f (MethodBody a b c d e code f g) = MethodBody a b c d e newCode f g
    where
      newCode = foldr replaceGetLex [] code
  replaceGetLex (GetLex idx) acc = [FindPropStrict idx, GetProperty idx] ++ acc
  replaceGetLex op acc = op:acc

get_int :: U30 -> AVM3 Int32
get_int u30 = do VmAbc_Int a <- get_ht key_int u30;return a

put_int :: U30 -> Int32 -> AVM3 ()
put_int k v = put_ht key_int k $ VmAbc_Int v

get_uint :: U30 -> AVM3 U30
get_uint u30 = do VmAbc_Uint a <- get_ht key_uint u30;return a

put_uint :: U30 -> U30 -> AVM3 ()
put_uint k v = put_ht key_uint k $ VmAbc_Uint v

get_double :: U30 -> AVM3 Double
get_double u30 = do VmAbc_Double a <- get_ht key_double u30;return a

put_double :: U30 -> Double -> AVM3 ()
put_double k v = put_ht key_double k $ VmAbc_Double v

get_string :: U30 -> AVM3 String
get_string u30 = do VmAbc_String a <- get_ht key_string u30;return a

put_string :: U30 -> String -> AVM3 ()
put_string k v = put_ht key_string k $ VmAbc_String v

get_nsInfo :: U30 -> AVM3 NSInfo
get_nsInfo u30 = do VmAbc_NsInfo a <- get_ht key_nsInfo u30;return a

put_nsInfo :: U30 -> NSInfo -> AVM3 ()
put_nsInfo k v = put_ht key_nsInfo k $ VmAbc_NsInfo v

get_nsSet :: U30 -> AVM3 NSSet
get_nsSet u30 = do VmAbc_NsSet a <- get_ht key_nsSet u30;return a

put_nsSet :: U30 -> NSSet -> AVM3 ()
put_nsSet k v = put_ht key_nsSet k $ VmAbc_NsSet v

get_multiname :: U30 -> AVM3 Multiname
get_multiname u30 = do VmAbc_Multiname a <- get_ht key_multiname u30;return a

put_multiname :: U30 -> Multiname -> AVM3 ()
put_multiname k v = put_ht key_multiname k $ VmAbc_Multiname v

get_methodSig :: U30 -> AVM3 MethodSignature
get_methodSig u30 = do VmAbc_MethodSig a <- get_ht key_methodSig u30;return a

put_methodSig :: U30 -> MethodSignature -> AVM3 ()
put_methodSig k v = put_ht key_methodSig k $ VmAbc_MethodSig v

get_metadata :: U30 -> AVM3 Metadata
get_metadata u30 = do VmAbc_Metadata a <- get_ht key_metadata u30;return a

put_metadata :: U30 -> Metadata -> AVM3 ()
put_metadata k v = put_ht key_metadata k $ VmAbc_Metadata v

get_instance :: U30 -> AVM3 InstanceInfo
get_instance u30 = do VmAbc_Instance a <- get_ht key_instance u30;return a

put_instance :: U30 -> InstanceInfo -> AVM3 ()
put_instance k v = put_ht key_instance k $ VmAbc_Instance v

get_class :: U30 -> AVM3 ClassInfo
get_class u30 = do VmAbc_Class a <- get_ht key_class u30;return a

put_class :: U30 -> ClassInfo -> AVM3 ()
put_class k v = put_ht key_class k $ VmAbc_Class v

get_script :: U30 -> AVM3 ScriptInfo
get_script u30 = do VmAbc_Script a <- get_ht key_script u30;return a

put_script :: U30 -> ScriptInfo -> AVM3 ()
put_script k v = put_ht key_script k $ VmAbc_Script v

get_methodBody :: U30 -> AVM3 MethodBody
get_methodBody u30 = do VmAbc_MethodBody a <- get_ht key_methodBody u30;return a

put_methodBody :: U30 -> MethodBody -> AVM3 ()
put_methodBody k v = put_ht key_methodBody k $ VmAbc_MethodBody v

get_ht :: B.ByteString -> U30 -> AVM3 VmAbc
get_ht prefix k = do
  ht <- get_cp
  --liftIO.putStrLn$ "prefix " ++ show prefix ++ show k
  m <- liftIO $ H.lookup ht fullKey
  case m of
    Nothing -> raise$ "get_ht - " ++ (BC.unpack prefix ++ show k)
    Just ret -> return ret
  where
    fullKey = foldr B.cons prefix$ u30ToWord8 k

put_ht :: B.ByteString -> U30 -> VmAbc -> AVM3 ()
put_ht prefix k v = do
  ht <- get_cp
  --liftIO.putStrLn$ "prefix " ++ show prefix ++ show k
  liftIO $ H.insert ht fullKey v
  set_cp ht
  where
    fullKey = foldr B.cons prefix$ u30ToWord8 k

u30ToWord8 :: U30 -> [Word8]
u30ToWord8 u30 = [msb0, msb1, msb2, msb3]
  where
    msb0 = fromIntegral$ u30 `shiftR` 24 .&. 0xff
    msb1 = fromIntegral$ u30 `shiftR` 16 .&. 0xff
    msb2 = fromIntegral$ u30 `shiftR`  8 .&. 0xff
    msb3 = fromIntegral$ u30 `shiftR`  0 .&. 0xff
