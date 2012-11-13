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

import ABC.Def
import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Int
import Util.Words (t33)
import Vm.Def
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
    mapM_ (\(idx, i) -> put_int idx i) $ zip [0..length ints] ints
    mapM_ (\(idx, i) -> put_uint idx i) $ zip [0..length uints] uints
    mapM_ (\(idx, i) -> put_double idx i) $ zip [0..length doubles] doubles
    mapM_ (\(idx, i) -> put_string idx i) $ zip [0..length strings] strings
    mapM_ (\(idx, i) -> put_nsInfo idx i) $ zip [0..length nsInfo] nsInfo
    mapM_ (\(idx, i) -> put_nsSet idx i) $ zip [0..length nsSet] nsSet
    mapM_ (\(idx, i) -> put_multiname idx i) $ zip [0..length multinames] multinames
    mapM_ (\(idx, i) -> put_methodSig idx i) $ zip [0..length methodSigs] methodSigs
    mapM_ (\(idx, i) -> put_metadata idx i) $ zip [0..length metadata] metadata
    mapM_ (\(idx, i) -> put_instance idx i) $ zip [0..length instances] instances
    mapM_ (\(idx, i) -> put_class idx i) $ zip [0..length classes] classes
    mapM_ (\(idx, i) -> put_script idx i) $ zip [0..length scripts] scripts
    mapM_ (\(idx, i) -> put_methodBody idx i) $ zip [0..length methodBodies] methodBodies
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

get_int :: Int -> AVM3 (Maybe Int32)
get_int = liftM (liftM (\(Abc2_Int a) -> a)) <$> get_ht Int_

put_int :: Int -> Int32 -> AVM3 ()
put_int k v = put_ht Int_ k $ Abc2_Int v

get_uint :: Int -> AVM3 (Maybe U30)
get_uint = liftM (liftM (\(Abc2_Uint a) -> a)) <$> get_ht Uint_

put_uint :: Int -> U30 -> AVM3 ()
put_uint k v = put_ht Uint_ k $ Abc2_Uint v

get_double :: Int -> AVM3 (Maybe Double)
get_double = liftM (liftM (\(Abc2_Double a) -> a)) <$> get_ht Double_

put_double :: Int -> Double -> AVM3 ()
put_double k v = put_ht Double_ k $ Abc2_Double v

get_string :: Int -> AVM3 (Maybe String)
get_string = liftM (liftM (\(Abc2_String a) -> a)) <$> get_ht String_

put_string :: Int -> String -> AVM3 ()
put_string k v = put_ht String_ k $ Abc2_String v

get_nsInfo :: Int -> AVM3 (Maybe NSInfo)
get_nsInfo = liftM (liftM (\(Abc2_NsInfo a) -> a)) <$> get_ht NsInfo_

put_nsInfo :: Int -> NSInfo -> AVM3 ()
put_nsInfo k v = put_ht NsInfo_ k $ Abc2_NsInfo v

get_nsSet :: Int -> AVM3 (Maybe NSSet)
get_nsSet = liftM (liftM (\(Abc2_NsSet a) -> a)) <$> get_ht NsSet_

put_nsSet :: Int -> NSSet -> AVM3 ()
put_nsSet k v = put_ht NsSet_ k $ Abc2_NsSet v

get_multiname :: Int -> AVM3 (Maybe Multiname)
get_multiname = liftM (liftM (\(Abc2_Multiname a) -> a)) <$> get_ht Multiname_

put_multiname :: Int -> Multiname -> AVM3 ()
put_multiname k v = put_ht Multiname_ k $ Abc2_Multiname v

get_methodSig :: Int -> AVM3 (Maybe MethodSignature)
get_methodSig = liftM (liftM (\(Abc2_MethodSig a) -> a)) <$> get_ht MethodSig_

put_methodSig :: Int -> MethodSignature -> AVM3 ()
put_methodSig k v = put_ht MethodSig_ k $ Abc2_MethodSig v

get_metadata :: Int -> AVM3 (Maybe Metadata)
get_metadata = liftM (liftM (\(Abc2_Metadata a) -> a)) <$> get_ht Metadata_

put_metadata :: Int -> Metadata -> AVM3 ()
put_metadata k v = put_ht Metadata_ k $ Abc2_Metadata v

get_instance :: Int -> AVM3 (Maybe InstanceInfo)
get_instance = liftM (liftM (\(Abc2_Instance a) -> a)) <$> get_ht Instance_

put_instance :: Int -> InstanceInfo -> AVM3 ()
put_instance k v = put_ht Instance_ k $ Abc2_Instance v

get_class :: Int -> AVM3 (Maybe ClassInfo)
get_class = liftM (liftM (\(Abc2_Class a) -> a)) <$> get_ht Class_

put_class :: Int -> ClassInfo -> AVM3 ()
put_class k v = put_ht Class_ k $ Abc2_Class v

get_script :: Int -> AVM3 (Maybe ScriptInfo)
get_script = liftM (liftM (\(Abc2_Script a) -> a)) <$> get_ht Script_

put_script :: Int -> ScriptInfo -> AVM3 ()
put_script k v = put_ht Script_ k $ Abc2_Script v

get_methodBody :: Int -> AVM3 (Maybe MethodBody)
get_methodBody = liftM (liftM (\(Abc2_MethodBody a) -> a)) <$> get_ht MethodBody_

put_methodBody :: Int -> MethodBody -> AVM3 ()
put_methodBody k v = put_ht MethodBody_ k $ Abc2_MethodBody v

get_ht :: HTPrefix -> Int -> AVM3 (Maybe Abc2)
get_ht prefix k = do
    ht <- (gets $ t33.ex) >>= liftIO
    --liftIO.putStrLn$ "prefix " ++ show prefix ++ show k
    liftIO $ H.lookup ht (show prefix ++ show k)

put_ht :: HTPrefix -> Int -> Abc2 -> AVM3 ()
put_ht prefix k v = do
    (a, b, ioht) <- gets ex
    ht <- liftIO ioht
    --liftIO.putStrLn$ "prefix " ++ show prefix ++ show k
    liftIO $ H.insert ht (show prefix ++ show k) v
    put $ Execution (a, b, return ht)
