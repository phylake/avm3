module Vm.Lookups (resolve_nsinfo) where

import           Abc.Def
import           MonadLib
import           Util.Misc
import           Vm.Def
import           Vm.Store
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

resolve_nsinfo :: ConstantPool -> MultinameIdx -> IO B.ByteString
resolve_nsinfo cp idx = do
  nsinfo <- get_nsInfo cp idx
  case nsinfo of
    NSInfo_Namespace stringIdx -> get_string cp stringIdx
    NSInfo_PackageNamespace stringIdx -> get_string cp stringIdx
    NSInfo_PackageInternalNs stringIdx -> get_string cp stringIdx
    NSInfo_ProtectedNamespace stringIdx -> get_string cp stringIdx
    NSInfo_ExplicitNamespace stringIdx -> get_string cp stringIdx
    NSInfo_StaticProtectedNs stringIdx -> get_string cp stringIdx
    NSInfo_PrivateNs stringIdx -> get_string cp stringIdx
    NSInfo_Any -> return$ BC.pack "*"

-- finding a need to handle this logic in execute.hs
-- so not exporting for now
resolve_multiname :: ConstantPool -> MultinameIdx -> IO B.ByteString
resolve_multiname cp idx = do
  --putStrLn "resolve_multiname"
  multiname <- get_multiname cp idx
  case multiname of
    Multiname_QName nSInfoIdx stringIdx -> do
      info <- resolve_nsinfo cp nSInfoIdx
      str <- get_string cp stringIdx
      {-putStrLn "\tin Multiname_QName"
      putStrLn$ "\tinfo - " ++ info
      putStrLn$ "\tstr - " ++ maybe "*" id str-}
      return$ B.append info str

    Multiname_QNameA nSInfoIdx stringIdx -> fail "unsupported lookup: Multiname_QNameA"
    Multiname_RTQName stringIdx -> fail "unsupported lookup: Multiname_RTQName"
    Multiname_RTQNameA stringIdx -> fail "unsupported lookup: Multiname_RTQNameA"
    Multiname_RTQNameL -> fail "unsupported lookup: Multiname_RTQNameL"
    Multiname_RTQNameLA -> fail "unsupported lookup: Multiname_RTQNameLA"
    Multiname_Multiname stringIdx nSSetIdx -> fail "unsupported lookup: Multiname_Multiname"
    Multiname_MultinameA stringIdx nSSetIdx -> fail "unsupported lookup: Multiname_MultinameA"
    Multiname_MultinameL nSSetIdx -> fail "unsupported lookup: Multiname_MultinameL"
    Multiname_MultinameLA nSSetIdx -> fail "unsupported lookup: Multiname_MultinameLA"
    Multiname_Any -> fail "unsupported lookup: Multiname_Any"
