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
import           Data.Int
import           MonadLib hiding (get, set)
import           Util.Misc (t44)
import           Vm.Def
import qualified Data.HashTable.IO as H

data HTPrefix = Int_
              | Uint_
              | Double_
              | String_
              | NsInfo_
              | NsSet_
              | Multiname_
              | MethodSig_
              | Metadata_
              | Instance_
              | Class_
              | Script_
              | MethodBody_
              deriving (Show)

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
  replaceGetLex (GetLex idx) acc = [GetProperty idx, FindPropStrict idx] ++ acc
  replaceGetLex op acc = op:acc

get_int :: U30 -> AVM3 Int32
get_int u30 = do VmAbc_Int a <- get_ht Int_ u30;return a

put_int :: U30 -> Int32 -> AVM3 ()
put_int k v = put_ht Int_ k $ VmAbc_Int v

get_uint :: U30 -> AVM3 U30
get_uint u30 = do VmAbc_Uint a <- get_ht Uint_ u30;return a

put_uint :: U30 -> U30 -> AVM3 ()
put_uint k v = put_ht Uint_ k $ VmAbc_Uint v

get_double :: U30 -> AVM3 Double
get_double u30 = do VmAbc_Double a <- get_ht Double_ u30;return a

put_double :: U30 -> Double -> AVM3 ()
put_double k v = put_ht Double_ k $ VmAbc_Double v

get_string :: U30 -> AVM3 String
get_string u30 = do VmAbc_String a <- get_ht String_ u30;return a

put_string :: U30 -> String -> AVM3 ()
put_string k v = put_ht String_ k $ VmAbc_String v

get_nsInfo :: U30 -> AVM3 NSInfo
get_nsInfo u30 = do VmAbc_NsInfo a <- get_ht NsInfo_ u30;return a

put_nsInfo :: U30 -> NSInfo -> AVM3 ()
put_nsInfo k v = put_ht NsInfo_ k $ VmAbc_NsInfo v

get_nsSet :: U30 -> AVM3 NSSet
get_nsSet u30 = do VmAbc_NsSet a <- get_ht NsSet_ u30;return a

put_nsSet :: U30 -> NSSet -> AVM3 ()
put_nsSet k v = put_ht NsSet_ k $ VmAbc_NsSet v

get_multiname :: U30 -> AVM3 Multiname
get_multiname u30 = do VmAbc_Multiname a <- get_ht Multiname_ u30;return a

put_multiname :: U30 -> Multiname -> AVM3 ()
put_multiname k v = put_ht Multiname_ k $ VmAbc_Multiname v

get_methodSig :: U30 -> AVM3 MethodSignature
get_methodSig u30 = do VmAbc_MethodSig a <- get_ht MethodSig_ u30;return a

put_methodSig :: U30 -> MethodSignature -> AVM3 ()
put_methodSig k v = put_ht MethodSig_ k $ VmAbc_MethodSig v

get_metadata :: U30 -> AVM3 Metadata
get_metadata u30 = do VmAbc_Metadata a <- get_ht Metadata_ u30;return a

put_metadata :: U30 -> Metadata -> AVM3 ()
put_metadata k v = put_ht Metadata_ k $ VmAbc_Metadata v

get_instance :: U30 -> AVM3 InstanceInfo
get_instance u30 = do VmAbc_Instance a <- get_ht Instance_ u30;return a

put_instance :: U30 -> InstanceInfo -> AVM3 ()
put_instance k v = put_ht Instance_ k $ VmAbc_Instance v

get_class :: U30 -> AVM3 ClassInfo
get_class u30 = do VmAbc_Class a <- get_ht Class_ u30;return a

put_class :: U30 -> ClassInfo -> AVM3 ()
put_class k v = put_ht Class_ k $ VmAbc_Class v

get_script :: U30 -> AVM3 ScriptInfo
get_script u30 = do VmAbc_Script a <- get_ht Script_ u30;return a

put_script :: U30 -> ScriptInfo -> AVM3 ()
put_script k v = put_ht Script_ k $ VmAbc_Script v

get_methodBody :: U30 -> AVM3 MethodBody
get_methodBody u30 = do VmAbc_MethodBody a <- get_ht MethodBody_ u30;return a

put_methodBody :: U30 -> MethodBody -> AVM3 ()
put_methodBody k v = put_ht MethodBody_ k $ VmAbc_MethodBody v

get_ht :: HTPrefix -> U30 -> AVM3 VmAbc
get_ht prefix k = do
  ht <- get_cp
  --liftIO.putStrLn$ "prefix " ++ show prefix ++ show k
  m <- liftIO $ H.lookup ht (show prefix ++ show k)
  case m of
    Nothing -> raise$ "get_ht - " ++ (show prefix ++ show k)
    Just ret -> return ret

put_ht :: HTPrefix -> U30 -> VmAbc -> AVM3 ()
put_ht prefix k v = do
  ht <- get_cp
  --liftIO.putStrLn$ "prefix " ++ show prefix ++ show k
  liftIO $ H.insert ht (show prefix ++ show k) v
  set_cp ht
