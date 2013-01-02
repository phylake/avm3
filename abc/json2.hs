module Abc.Json2 (abcToJson) where

import Abc.Def
import Abc.Json
import Data.Word
import Text.JSON

kvToObject :: [(String, JSValue)] -> JSValue
kvToObject = JSObject. toJSObject

toArray :: JSON a => [a] -> JSValue
toArray = JSArray. map showJSON

abcToJson :: Abc -> JSValue
abcToJson (Abc i ui d s nsi nss names sigs meta inst klas rx bodies) = kvToObject$
  [
    ("ints", toArray i)
  , ("uints", toArray ui)
  , ("doubles", toArray d)
  , ("strings", toArray s)
  , ("ns_infos", JSArray$ map (showJSON. nsinfo s_res) nsi)
  , ("ns_sets", toArray nss)
  , ("multinames", JSArray$ map (showJSON. multiname s_res nsi_res nss_res) names)
  , ("method_signatures", JSArray$ map (method_signature m_res s_res) sigs)
  , ("method_bodies", toArray bodies)
  , ("metadata", toArray meta)
  , ("instance_infos", toArray inst)
  , ("class_infos", JSArray$ map (class_info m_res) klas)
  , ("scripts", toArray rx)
  ]
  where
    m_res :: U30 -> String
    m_res i = multiname s_res nsi_res nss_res$ names !! fromIntegral i
    s_res :: U30 -> String
    s_res i = s !! fromIntegral i
    nsi_res :: U30 -> String
    nsi_res i = nsinfo_raw s_res$ nsi !! fromIntegral i
    nss_res :: U30 -> String
    nss_res i = "NSSET"

nsinfo :: (U30 -> String) -> NSInfo -> String
nsinfo s_res nsi@(NSInfo_Namespace a) = "Namespace[" ++ nsinfo_raw s_res nsi ++ "]"
nsinfo s_res nsi@(NSInfo_PackageNamespace a) = "PackageNamespace[" ++ nsinfo_raw s_res nsi ++ "]"
nsinfo s_res nsi@(NSInfo_PackageInternalNs a) = "PackageInternalNs[" ++ nsinfo_raw s_res nsi ++ "]"
nsinfo s_res nsi@(NSInfo_ProtectedNamespace a) = "ProtectedNamespace[" ++ nsinfo_raw s_res nsi ++ "]"
nsinfo s_res nsi@(NSInfo_ExplicitNamespace a) = "ExplicitNamespace[" ++ nsinfo_raw s_res nsi ++ "]"
nsinfo s_res nsi@(NSInfo_StaticProtectedNs a) = "StaticProtectedNs[" ++ nsinfo_raw s_res nsi ++ "]"
nsinfo s_res nsi@(NSInfo_PrivateNs a) = "PrivateNs[" ++ nsinfo_raw s_res nsi ++ "]"
nsinfo s_res NSInfo_Any = "Any[" ++ nsinfo_raw s_res NSInfo_Any ++ "]"

nsinfo_raw :: (U30 -> String) -> NSInfo -> String
nsinfo_raw s_res (NSInfo_Namespace a)          = s_res a
nsinfo_raw s_res (NSInfo_PackageNamespace a)   = s_res a
nsinfo_raw s_res (NSInfo_PackageInternalNs a)  = s_res a
nsinfo_raw s_res (NSInfo_ProtectedNamespace a) = s_res a
nsinfo_raw s_res (NSInfo_ExplicitNamespace a)  = s_res a
nsinfo_raw s_res (NSInfo_StaticProtectedNs a)  = s_res a
nsinfo_raw s_res (NSInfo_PrivateNs a)          = s_res a
nsinfo_raw s_res NSInfo_Any = "*"

multiname :: (U30 -> String) -- string resolution
          -> (U30 -> String) -- nsinfo resolution
          -> (U30 -> String) -- nsset resolution
          -> Multiname
          -> String
multiname s_res nsi_res nss_res (Multiname_QName a b) = nsi_res a ++ "::" ++ s_res b
multiname s_res nsi_res nss_res (Multiname_QNameA a b) = nsi_res a ++ "::" ++ s_res b
multiname s_res nsi_res nss_res m@(Multiname_RTQName a) = show m
multiname s_res nsi_res nss_res m@(Multiname_RTQNameA a) = show m
multiname s_res nsi_res nss_res m@(Multiname_Multiname a b) = show m
multiname s_res nsi_res nss_res m@(Multiname_MultinameA a b) = show m
multiname s_res nsi_res nss_res m@(Multiname_MultinameL a) = show m
multiname s_res nsi_res nss_res m@(Multiname_MultinameLA a) = show m
multiname s_res nsi_res nss_res Multiname_Any = "*"

method_signature :: (U30 -> String) -- multiname resolution
                 -> (U30 -> String) -- string resolution
                 -> MethodSignature
                 -> JSValue
method_signature m_res s_res (MethodSignature ret ptypes name flags options pnames) = kvToObject$
  [
    ("name", showJSON$ s_res name)
  , ("ret", showJSON$ m_res ret)
  , ("flags", showJSON flags)
  , ("param_types", JSArray$ map (showJSON. m_res) ptypes)
  , ("param_names", JSArray$ maybe [] (map$ showJSON. s_res) pnames)
  , ("options", maybe (JSArray []) toArray options)
  ]

class_info :: (U30 -> String) -- multiname resolution
           -> ClassInfo
           -> JSValue
class_info m_res (ClassInfo init traits) = kvToObject
    [
      ("init", showJSON$ m_res init)
    , ("traits", toArray traits)
    ]

exceptions :: [String] -> Exception -> JSValue
exceptions s (Exception from to target t varname) = kvToObject
  [
    ("from", showJSON from)
  , ("to", showJSON to)
  , ("target", showJSON target)
  , ("type", showJSON$ s !! fromIntegral t)
  , ("varname", showJSON$ s !! fromIntegral varname)
  ]
