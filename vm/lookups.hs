module Vm.Lookups where

import           Abc.Def
import           Vm.Def
import           Vm.Store

resolve_nsinfo :: ConstantPool -> MultinameIdx -> Ops -> IO String
resolve_nsinfo cp idx ops = do
  nsinfo <- get_nsInfo cp idx
  case nsinfo of
    Nothing -> fail "resolve_nsinfo fail"
    Just (NSInfo_Namespace stringIdx) -> do
      maybStr <- get_string cp stringIdx
      case maybStr of
        Nothing -> fail "NSInfo_Namespace fail"
        Just str -> return str
    Just (NSInfo_PackageNamespace stringIdx) -> do
      maybStr <- get_string cp stringIdx
      case maybStr of
        Nothing -> fail "NSInfo_PackageNamespace fail"
        Just str -> return str
    Just (NSInfo_PackageInternalNs stringIdx) -> do
      maybStr <- get_string cp stringIdx
      case maybStr of
        Nothing -> fail "NSInfo_PackageInternalNs fail"
        Just str -> return str
    Just (NSInfo_ProtectedNamespace stringIdx) -> do
      maybStr <- get_string cp stringIdx
      case maybStr of
        Nothing -> fail "NSInfo_ProtectedNamespace fail"
        Just str -> return str
    Just (NSInfo_ExplicitNamespace stringIdx) -> do
      maybStr <- get_string cp stringIdx
      case maybStr of
        Nothing -> fail "NSInfo_ExplicitNamespace fail"
        Just str -> return str
    Just (NSInfo_StaticProtectedNs stringIdx) -> do
      maybStr <- get_string cp stringIdx
      case maybStr of
        Nothing -> fail "NSInfo_StaticProtectedNs fail"
        Just str -> return str
    Just (NSInfo_PrivateNs stringIdx) -> do
      maybStr <- get_string cp stringIdx
      case maybStr of
        Nothing -> fail "NSInfo_PrivateNs fail"
        Just str -> return str
    Just NSInfo_Any -> return "*"

resolve_multiname :: ConstantPool -> MultinameIdx -> Ops -> IO String
resolve_multiname cp idx ops = do
  --putStrLn "resolve_multiname"
  maybMultiname <- get_multiname cp idx
  case maybMultiname of
    Nothing -> fail "resolve_multiname fail"
    Just (Multiname_QName nSInfoIdx stringIdx) -> do
      info <- resolve_nsinfo cp nSInfoIdx ops
      str <- get_string cp stringIdx
      {-putStrLn "\tin Multiname_QName"
      putStrLn$ "\tinfo - " ++ info
      putStrLn$ "\tstr - " ++ maybe "*" id str-}
      return$ info ++ maybe "*" id str

    Just (Multiname_QNameA nSInfoIdx stringIdx) -> return ""
    Just (Multiname_RTQName stringIdx) -> return ""
    Just (Multiname_RTQNameA stringIdx) -> return ""
    Just Multiname_RTQNameL -> return ""
    Just Multiname_RTQNameLA -> return ""
    Just (Multiname_Multiname stringIdx nSSetIdx) -> return ""
    Just (Multiname_MultinameA stringIdx nSSetIdx) -> return ""
    Just (Multiname_MultinameL nSSetIdx) -> return ""
    Just (Multiname_MultinameLA nSSetIdx) -> return ""
    Just Multiname_Any -> return ""
