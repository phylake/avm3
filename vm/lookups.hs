module Vm.Lookups where

import           Abc.Def
import           MonadLib
import           Util.Misc
import           Vm.Def
import           Vm.Store

get_reg :: AVM3 Registers
get_reg = get >>= return. t41

set_reg :: Registers -> AVM3 ()
set_reg reg = do
  (_,ops,ss,cp) <- get
  set (reg,ops,ss,cp)

mod_reg :: (Registers -> Registers) -> AVM3 ()
mod_reg f = get_reg >>= return.f >>= set_reg

get_ops :: AVM3 Ops
get_ops = get >>= return. t42

mod_ops :: (Ops -> Ops) -> AVM3 ()
mod_ops f = get_ops >>= return.f >>= set_ops

set_ops :: Ops -> AVM3 ()
set_ops ops = do
  (reg,_,ss,cp) <- get
  set (reg,ops,ss,cp)

get_ss :: AVM3 ScopeStack
get_ss = get >>= return. t43

mod_ss :: (ScopeStack -> ScopeStack) -> AVM3 ()
mod_ss f = get_ss >>= return.f >>= set_ss

set_ss :: ScopeStack -> AVM3 ()
set_ss ss = do
  (reg,ops,_,cp) <- get
  set (reg,ops,ss,cp)

get_cp :: AVM3 ConstantPool
get_cp = get >>= return. t44

mod_cp :: (ConstantPool -> ConstantPool) -> AVM3 ()
mod_cp f = get_cp >>= return.f >>= set_cp

set_cp :: ConstantPool -> AVM3 ()
set_cp cp = do
  (reg,ops,ss,_) <- get
  set (reg,ops,ss,cp)

resolve_nsinfo :: MultinameIdx -> AVM3 String
resolve_nsinfo idx = do
  nsinfo <- get_nsInfo idx
  case nsinfo of
    Nothing -> fail "resolve_nsinfo fail"
    Just (NSInfo_Namespace stringIdx) -> do
      maybStr <- get_string stringIdx
      case maybStr of
        Nothing -> fail "NSInfo_Namespace fail"
        Just str -> return str
    Just (NSInfo_PackageNamespace stringIdx) -> do
      maybStr <- get_string stringIdx
      case maybStr of
        Nothing -> fail "NSInfo_PackageNamespace fail"
        Just str -> return str
    Just (NSInfo_PackageInternalNs stringIdx) -> do
      maybStr <- get_string stringIdx
      case maybStr of
        Nothing -> fail "NSInfo_PackageInternalNs fail"
        Just str -> return str
    Just (NSInfo_ProtectedNamespace stringIdx) -> do
      maybStr <- get_string stringIdx
      case maybStr of
        Nothing -> fail "NSInfo_ProtectedNamespace fail"
        Just str -> return str
    Just (NSInfo_ExplicitNamespace stringIdx) -> do
      maybStr <- get_string stringIdx
      case maybStr of
        Nothing -> fail "NSInfo_ExplicitNamespace fail"
        Just str -> return str
    Just (NSInfo_StaticProtectedNs stringIdx) -> do
      maybStr <- get_string stringIdx
      case maybStr of
        Nothing -> fail "NSInfo_StaticProtectedNs fail"
        Just str -> return str
    Just (NSInfo_PrivateNs stringIdx) -> do
      maybStr <- get_string stringIdx
      case maybStr of
        Nothing -> fail "NSInfo_PrivateNs fail"
        Just str -> return str
    Just NSInfo_Any -> return "*"

{-
TODO resolve against
  1. method closures
  2. declared traits
  3. dynamic properties
  4. prototype chain
see findpropstrict
-}
resolve_multiname :: MultinameIdx -> AVM3 String
resolve_multiname idx = do
  --putStrLn "resolve_multiname"
  maybMultiname <- get_multiname idx
  case maybMultiname of
    Nothing -> fail "resolve_multiname fail"
    Just (Multiname_QName nSInfoIdx stringIdx) -> do
      info <- resolve_nsinfo nSInfoIdx
      str <- get_string stringIdx
      {-putStrLn "\tin Multiname_QName"
      putStrLn$ "\tinfo - " ++ info
      putStrLn$ "\tstr - " ++ maybe "*" id str-}
      return$ info ++ maybe "*" id str

    Just (Multiname_QNameA nSInfoIdx stringIdx) -> fail "unsupported lookup: Multiname_QNameA"
    Just (Multiname_RTQName stringIdx) -> fail "unsupported lookup: Multiname_RTQName"
    Just (Multiname_RTQNameA stringIdx) -> fail "unsupported lookup: Multiname_RTQNameA"
    Just Multiname_RTQNameL -> fail "unsupported lookup: Multiname_RTQNameL"
    Just Multiname_RTQNameLA -> fail "unsupported lookup: Multiname_RTQNameLA"
    Just (Multiname_Multiname stringIdx nSSetIdx) -> fail "unsupported lookup: Multiname_Multiname"
    Just (Multiname_MultinameA stringIdx nSSetIdx) -> fail "unsupported lookup: Multiname_MultinameA"
    Just (Multiname_MultinameL nSSetIdx) -> fail "unsupported lookup: Multiname_MultinameL"
    Just (Multiname_MultinameLA nSSetIdx) -> fail "unsupported lookup: Multiname_MultinameLA"
    Just Multiname_Any -> fail "unsupported lookup: Multiname_Any"
