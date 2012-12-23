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
  -- reverse lookup: get_methodBody expects a method signature index
  mapM_ (\(idx, a) -> put_methodBody cp idx a) $ zip (map mbMethod methodBodies) methodBodies
  return cp
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

get_int :: ConstantPool -> U30 -> IO (Maybe Int32)
get_int cp idx = liftM (\(VmAbc_Int a) -> a) <$> get_ht Int_ cp idx

{-get_int :: ConstantPool -> U30 -> IO Int32
get_int cp idx = do
  mayb <- get_ht Int_ cp idx
  case mayb of
    Just (VmAbc_Int a) -> return a
    otherwise -> fail$ "get_int " ++ show idx-}

put_int :: ConstantPool -> U30 -> Int32 -> IO ConstantPool
put_int cp k v = put_ht Int_ cp k $ VmAbc_Int v

get_uint :: ConstantPool -> U30 -> IO (Maybe U30)
get_uint cp idx = liftM (\(VmAbc_Uint a) -> a) <$> get_ht Uint_ cp idx

{-get_uint :: ConstantPool -> U30 -> IO U30
get_uint cp idx = do
  mayb <- get_ht Uint_ cp idx
  case mayb of
    Just (VmAbc_Uint a) -> return a
    otherwise -> fail$ "get_uint " ++ show idx-}

put_uint :: ConstantPool -> U30 -> U30 -> IO ConstantPool
put_uint cp k v = put_ht Uint_ cp k $ VmAbc_Uint v

get_double :: ConstantPool -> U30 -> IO (Maybe Double)
get_double cp idx = liftM (\(VmAbc_Double a) -> a) <$> get_ht Double_ cp idx

{-get_double :: ConstantPool -> U30 -> IO Double
get_double cp idx = do
  mayb <- get_ht Double_ cp idx
  case mayb of
    Just (VmAbc_Double a) -> return a
    otherwise -> fail$ "get_double " ++ show idx-}

put_double :: ConstantPool -> U30 -> Double -> IO ConstantPool
put_double cp k v = put_ht Double_ cp k $ VmAbc_Double v

get_string :: ConstantPool -> U30 -> IO (Maybe String)
get_string cp idx = liftM (\(VmAbc_String a) -> a) <$> get_ht String_ cp idx

{-get_string :: ConstantPool -> U30 -> IO String
get_string cp idx = do
  mayb <- get_ht String_ cp idx
  case mayb of
    Just (VmAbc_String a) -> return a
    otherwise -> fail$ "get_string " ++ show idx-}

put_string :: ConstantPool -> U30 -> String -> IO ConstantPool
put_string cp k v = put_ht String_ cp k $ VmAbc_String v

get_nsInfo :: ConstantPool -> U30 -> IO (Maybe NSInfo)
get_nsInfo cp idx = liftM (\(VmAbc_NsInfo a) -> a) <$> get_ht NsInfo_ cp idx

{-get_nsInfo :: ConstantPool -> U30 -> IO NSInfo
get_nsInfo cp idx = do
  mayb <- get_ht NsInfo_ cp idx
  case mayb of
    Just (VmAbc_NsInfo a) -> return a
    otherwise -> fail$ "get_nsInfo " ++ show idx-}

put_nsInfo :: ConstantPool -> U30 -> NSInfo -> IO ConstantPool
put_nsInfo cp k v = put_ht NsInfo_ cp k $ VmAbc_NsInfo v

get_nsSet :: ConstantPool -> U30 -> IO (Maybe NSSet)
get_nsSet cp idx = liftM (\(VmAbc_NsSet a) -> a) <$> get_ht NsSet_ cp idx

{-get_nsSet :: ConstantPool -> U30 -> IO NSSet
get_nsSet cp idx = do
  mayb <- get_ht NsSet_ cp idx
  case mayb of
    Just (VmAbc_NsSet a) -> return a
    otherwise -> fail$ "get_nsSet " ++ show idx-}

put_nsSet :: ConstantPool -> U30 -> NSSet -> IO ConstantPool
put_nsSet cp k v = put_ht NsSet_ cp k $ VmAbc_NsSet v

get_multiname :: ConstantPool -> U30 -> IO (Maybe Multiname)
get_multiname cp idx = liftM (\(VmAbc_Multiname a) -> a) <$> get_ht Multiname_ cp idx

{-get_multiname :: ConstantPool -> U30 -> IO Multiname
get_multiname cp idx = do
  mayb <- get_ht Multiname_ cp idx
  case mayb of
    Just (VmAbc_Multiname a) -> return a
    otherwise -> fail$ "get_multiname " ++ show idx-}

put_multiname :: ConstantPool -> U30 -> Multiname -> IO ConstantPool
put_multiname cp k v = put_ht Multiname_ cp k $ VmAbc_Multiname v

get_methodSig :: ConstantPool -> U30 -> IO (Maybe MethodSignature)
get_methodSig cp idx = liftM (\(VmAbc_MethodSig a) -> a) <$> get_ht MethodSig_ cp idx

{-get_methodSig :: ConstantPool -> U30 -> IO MethodSignature
get_methodSig cp idx = do
  mayb <- get_ht MethodSig_ cp idx
  case mayb of
    Just (VmAbc_MethodSig a) -> return a
    otherwise -> fail$ "get_methodSig " ++ show idx-}

put_methodSig :: ConstantPool -> U30 -> MethodSignature -> IO ConstantPool
put_methodSig cp k v = put_ht MethodSig_ cp k $ VmAbc_MethodSig v

get_metadata :: ConstantPool -> U30 -> IO (Maybe Metadata)
get_metadata cp idx = liftM (\(VmAbc_Metadata a) -> a) <$> get_ht Metadata_ cp idx

{-get_metadata :: ConstantPool -> U30 -> IO Metadata
get_metadata cp idx = do
  mayb <- get_ht Metadata_ cp idx
  case mayb of
    Just (VmAbc_Metadata a) -> return a
    otherwise -> fail$ "get_metadata " ++ show idx-}

put_metadata :: ConstantPool -> U30 -> Metadata -> IO ConstantPool
put_metadata cp k v = put_ht Metadata_ cp k $ VmAbc_Metadata v

get_instance :: ConstantPool -> U30 -> IO (Maybe InstanceInfo)
get_instance cp idx = liftM (\(VmAbc_Instance a) -> a) <$> get_ht Instance_ cp idx

{-get_instance :: ConstantPool -> U30 -> IO InstanceInfo
get_instance cp idx = do
  mayb <- get_ht Instance_ cp idx
  case mayb of
    Just (VmAbc_Instance a) -> return a
    otherwise -> fail$ "get_instance " ++ show idx-}

put_instance :: ConstantPool -> U30 -> InstanceInfo -> IO ConstantPool
put_instance cp k v = put_ht Instance_ cp k $ VmAbc_Instance v

get_class :: ConstantPool -> U30 -> IO (Maybe ClassInfo)
get_class cp idx = liftM (\(VmAbc_Class a) -> a) <$> get_ht Class_ cp idx

{-get_class :: ConstantPool -> U30 -> IO ClassInfo
get_class cp idx = do
  mayb <- get_ht Class_ cp idx
  case mayb of
    Just (VmAbc_Class a) -> return a
    otherwise -> fail$ "get_class " ++ show idx-}

put_class :: ConstantPool -> U30 -> ClassInfo -> IO ConstantPool
put_class cp k v = put_ht Class_ cp k $ VmAbc_Class v

get_script :: ConstantPool -> U30 -> IO (Maybe ScriptInfo)
get_script cp idx = liftM (\(VmAbc_Script a) -> a) <$> get_ht Script_ cp idx

{-get_script :: ConstantPool -> U30 -> IO ScriptInfo
get_script cp idx = do
  mayb <- get_ht Script_ cp idx
  case mayb of
    Just (VmAbc_Script a) -> return a
    otherwise -> fail$ "get_script " ++ show idx-}

put_script :: ConstantPool -> U30 -> ScriptInfo -> IO ConstantPool
put_script cp k v = put_ht Script_ cp k $ VmAbc_Script v

get_methodBody :: ConstantPool -> U30 -> IO (Maybe MethodBody)
get_methodBody cp idx = liftM (\(VmAbc_MethodBody a) -> a) <$> get_ht MethodBody_ cp idx

{-get_methodBody :: ConstantPool -> U30 -> IO MethodBody
get_methodBody cp idx = do
  mayb <- get_ht MethodBody_ cp idx
  case mayb of
    Just (VmAbc_MethodBody a) -> return a
    otherwise -> fail$ "get_methodBody " ++ show idx-}

put_methodBody :: ConstantPool -> U30 -> MethodBody -> IO ConstantPool
put_methodBody cp k v = put_ht MethodBody_ cp k $ VmAbc_MethodBody v

get_ht :: HTPrefix -> ConstantPool -> U30 -> IO (Maybe VmAbc)
get_ht prefix ht k = do
  --liftIO.putStrLn$ "prefix " ++ show prefix ++ show k
  H.lookup ht (show prefix ++ show k)

put_ht :: HTPrefix -> ConstantPool -> U30 -> VmAbc -> IO ConstantPool
put_ht prefix ht k v = do
  --liftIO.putStrLn$ "prefix " ++ show prefix ++ show k
  H.insert ht (show prefix ++ show k) v
  return ht
