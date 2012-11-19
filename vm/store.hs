module Vm.Store (
    build_ht
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

import           ABC.Def
import           Control.Applicative ((<$>))
import           Control.Monad.State
import           Data.Int
import           Util.Misc (t33)
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

build_ht :: Abc -> AVM3 ()
build_ht abc = do
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
    -- reverse lookup: get_methodBody expects a method signature index
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
        methodBodies = abcMethodBodies abc

get_int :: U30 -> AVM3 (Maybe Int32)
get_int = liftM (liftM (\(VmAbc_Int a) -> a)) <$> get_ht Int_

put_int :: U30 -> Int32 -> AVM3 ()
put_int k v = put_ht Int_ k $ VmAbc_Int v

get_uint :: U30 -> AVM3 (Maybe U30)
get_uint = liftM (liftM (\(VmAbc_Uint a) -> a)) <$> get_ht Uint_

put_uint :: U30 -> U30 -> AVM3 ()
put_uint k v = put_ht Uint_ k $ VmAbc_Uint v

get_double :: U30 -> AVM3 (Maybe Double)
get_double = liftM (liftM (\(VmAbc_Double a) -> a)) <$> get_ht Double_

put_double :: U30 -> Double -> AVM3 ()
put_double k v = put_ht Double_ k $ VmAbc_Double v

get_string :: U30 -> AVM3 (Maybe String)
get_string = liftM (liftM (\(VmAbc_String a) -> a)) <$> get_ht String_

put_string :: U30 -> String -> AVM3 ()
put_string k v = put_ht String_ k $ VmAbc_String v

get_nsInfo :: U30 -> AVM3 (Maybe NSInfo)
get_nsInfo = liftM (liftM (\(VmAbc_NsInfo a) -> a)) <$> get_ht NsInfo_

put_nsInfo :: U30 -> NSInfo -> AVM3 ()
put_nsInfo k v = put_ht NsInfo_ k $ VmAbc_NsInfo v

get_nsSet :: U30 -> AVM3 (Maybe NSSet)
get_nsSet = liftM (liftM (\(VmAbc_NsSet a) -> a)) <$> get_ht NsSet_

put_nsSet :: U30 -> NSSet -> AVM3 ()
put_nsSet k v = put_ht NsSet_ k $ VmAbc_NsSet v

get_multiname :: U30 -> AVM3 (Maybe Multiname)
get_multiname = liftM (liftM (\(VmAbc_Multiname a) -> a)) <$> get_ht Multiname_

put_multiname :: U30 -> Multiname -> AVM3 ()
put_multiname k v = put_ht Multiname_ k $ VmAbc_Multiname v

get_methodSig :: U30 -> AVM3 (Maybe MethodSignature)
get_methodSig = liftM (liftM (\(VmAbc_MethodSig a) -> a)) <$> get_ht MethodSig_

put_methodSig :: U30 -> MethodSignature -> AVM3 ()
put_methodSig k v = put_ht MethodSig_ k $ VmAbc_MethodSig v

get_metadata :: U30 -> AVM3 (Maybe Metadata)
get_metadata = liftM (liftM (\(VmAbc_Metadata a) -> a)) <$> get_ht Metadata_

put_metadata :: U30 -> Metadata -> AVM3 ()
put_metadata k v = put_ht Metadata_ k $ VmAbc_Metadata v

get_instance :: U30 -> AVM3 (Maybe InstanceInfo)
get_instance = liftM (liftM (\(VmAbc_Instance a) -> a)) <$> get_ht Instance_

put_instance :: U30 -> InstanceInfo -> AVM3 ()
put_instance k v = put_ht Instance_ k $ VmAbc_Instance v

get_class :: U30 -> AVM3 (Maybe ClassInfo)
get_class = liftM (liftM (\(VmAbc_Class a) -> a)) <$> get_ht Class_

put_class :: U30 -> ClassInfo -> AVM3 ()
put_class k v = put_ht Class_ k $ VmAbc_Class v

get_script :: U30 -> AVM3 (Maybe ScriptInfo)
get_script = liftM (liftM (\(VmAbc_Script a) -> a)) <$> get_ht Script_

put_script :: U30 -> ScriptInfo -> AVM3 ()
put_script k v = put_ht Script_ k $ VmAbc_Script v

get_methodBody :: U30 -> AVM3 (Maybe MethodBody)
get_methodBody = liftM (liftM (\(VmAbc_MethodBody a) -> a)) <$> get_ht MethodBody_

put_methodBody :: U30 -> MethodBody -> AVM3 ()
put_methodBody k v = put_ht MethodBody_ k $ VmAbc_MethodBody v

get_ht :: HTPrefix -> U30 -> AVM3 (Maybe VmAbc)
get_ht prefix k = do
    ht <- (gets $ t33.ex) >>= liftIO
    --liftIO.putStrLn$ "prefix " ++ show prefix ++ show k
    liftIO $ H.lookup ht (show prefix ++ show k)

put_ht :: HTPrefix -> U30 -> VmAbc -> AVM3 ()
put_ht prefix k v = do
    (a, b, ioht) <- gets ex
    ht <- liftIO ioht
    --liftIO.putStrLn$ "prefix " ++ show prefix ++ show k
    liftIO $ H.insert ht (show prefix ++ show k) v
    put $ Execution (a, b, return ht)
