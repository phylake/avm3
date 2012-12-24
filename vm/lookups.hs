module Vm.Lookups where

import           Abc.Def
import           MonadLib
import           Util.Misc
import           Vm.Def
import           Vm.Store

resolve_nsinfo :: MultinameIdx -> AVM3 String
resolve_nsinfo idx = do
  nsinfo <- get_nsInfo idx
  case nsinfo of
    NSInfo_Namespace stringIdx -> get_string stringIdx
    NSInfo_PackageNamespace stringIdx -> get_string stringIdx
    NSInfo_PackageInternalNs stringIdx -> get_string stringIdx
    NSInfo_ProtectedNamespace stringIdx -> get_string stringIdx
    NSInfo_ExplicitNamespace stringIdx -> get_string stringIdx
    NSInfo_StaticProtectedNs stringIdx -> get_string stringIdx
    NSInfo_PrivateNs stringIdx -> get_string stringIdx
    NSInfo_Any -> return "*"

resolve_multiname :: MultinameIdx -> AVM3 String
resolve_multiname idx = do
  --putStrLn "resolve_multiname"
  multiname <- get_multiname idx
  case multiname of
    Multiname_QName nSInfoIdx stringIdx -> do
      info <- resolve_nsinfo nSInfoIdx
      str <- get_string stringIdx
      {-putStrLn "\tin Multiname_QName"
      putStrLn$ "\tinfo - " ++ info
      putStrLn$ "\tstr - " ++ maybe "*" id str-}
      return$ info ++ str

    Multiname_QNameA nSInfoIdx stringIdx -> raise "unsupported lookup: Multiname_QNameA"
    Multiname_RTQName stringIdx -> raise "unsupported lookup: Multiname_RTQName"
    Multiname_RTQNameA stringIdx -> raise "unsupported lookup: Multiname_RTQNameA"
    Multiname_RTQNameL -> raise "unsupported lookup: Multiname_RTQNameL"
    Multiname_RTQNameLA -> raise "unsupported lookup: Multiname_RTQNameLA"
    Multiname_Multiname stringIdx nSSetIdx -> raise "unsupported lookup: Multiname_Multiname"
    Multiname_MultinameA stringIdx nSSetIdx -> raise "unsupported lookup: Multiname_MultinameA"
    Multiname_MultinameL nSSetIdx -> raise "unsupported lookup: Multiname_MultinameL"
    Multiname_MultinameLA nSSetIdx -> raise "unsupported lookup: Multiname_MultinameLA"
    Multiname_Any -> raise "unsupported lookup: Multiname_Any"
