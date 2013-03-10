module Abc.Json2 (abcToJson) where

import Abc.Def hiding (trait_var)
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
  , ("method_bodies", JSArray$ map (method_body s_res m_res tt_res) bodies)
  , ("metadata", JSArray$ map (metadata s_res) meta)
  , ("instance_infos", toArray inst)
  , ("class_infos", JSArray$ map (class_info tt_res) klas)
  , ("scripts", JSArray$ map (script_info tt_res) rx)
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
    tt_res :: TraitsInfo -> JSValue
    tt_res = traits_info m_res (trait_type m_res)

nsinfo :: (U30 -> String) -- string resolution
       -> NSInfo
       -> String
nsinfo s_res nsi@(NSInfo_Namespace a) = "Namespace[" ++ nsinfo_raw s_res nsi ++ "]"
nsinfo s_res nsi@(NSInfo_PackageNamespace a) = "PackageNamespace[" ++ nsinfo_raw s_res nsi ++ "]"
nsinfo s_res nsi@(NSInfo_PackageInternalNs a) = "PackageInternalNs[" ++ nsinfo_raw s_res nsi ++ "]"
nsinfo s_res nsi@(NSInfo_ProtectedNamespace a) = "ProtectedNamespace[" ++ nsinfo_raw s_res nsi ++ "]"
nsinfo s_res nsi@(NSInfo_ExplicitNamespace a) = "ExplicitNamespace[" ++ nsinfo_raw s_res nsi ++ "]"
nsinfo s_res nsi@(NSInfo_StaticProtectedNs a) = "StaticProtectedNs[" ++ nsinfo_raw s_res nsi ++ "]"
nsinfo s_res nsi@(NSInfo_PrivateNs a) = "PrivateNs[" ++ nsinfo_raw s_res nsi ++ "]"
nsinfo s_res NSInfo_Any = "Any[" ++ nsinfo_raw s_res NSInfo_Any ++ "]"

nsinfo_raw :: (U30 -> String) -- string resolution
           -> NSInfo
           -> String
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
multiname s_res nsi_res nss_res (Multiname_QName a b) = case nsi_res a of
  "" -> s_res b
  nsinfo -> nsinfo ++ "::" ++ s_res b
multiname s_res nsi_res nss_res (Multiname_QNameA a b) = case nsi_res a of
  "" -> s_res b
  nsinfo -> nsinfo ++ "::" ++ s_res b
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

method_body :: (U30 -> String) -- multiname resolution
            -> (U30 -> String) -- string resolution
            -> (TraitsInfo -> JSValue) -- traits info resolution
            -> MethodBody
            -> JSValue
method_body m_res s_res tt_res (MethodBody init max local initD scopeD code exceptions traits) = kvToObject$
  [
    ("method_id", showJSON init)
  , ("max_stack", showJSON max)
  , ("local_count", showJSON local)
  , ("init_scope_depth", showJSON initD)
  , ("max_scope_depth", showJSON scopeD)
  , ("code", showJSON code)
  , ("exceptions", showJSON exceptions)
  , ("traits", JSArray$ map (showJSON . tt_res) traits)
  ]

metadata :: (U30 -> String) -- string resolution
         -> Metadata
         -> JSValue
metadata s_res (Metadata name kvps) = kvToObject
  [
    ("name", showJSON$ s_res name)
  , ("kvps", kvToObject$ map (\(k,v) -> (s_res k, showJSON$ s_res v)) kvps)
  ]

traits_info :: (U30 -> String) -- multiname resolution
            -> (TraitType -> JSValue) -- trait type resolution
            -> TraitsInfo
            -> JSValue
traits_info m_res tt_res (TraitsInfo name final override ttype meta) = kvToObject
  [
    ("name", showJSON$ m_res name)
  , ("final", showJSON$ final)
  , ("override", showJSON$ override)
  , ("type", showJSON$ tt_res ttype)
  , ("meta", maybe (JSArray []) toArray meta)
  ]

trait_type :: (U30 -> String) -- multiname resolution
           -> TraitType
           -> JSValue
trait_type m_res (TT_Var tvar) = trait_var m_res tvar
trait_type m_res (TT_Const tvar) = trait_var m_res tvar
trait_type m_res (TT_Method (TraitMethod tId meth)) = kvToObject$
  [
    ("id", showJSON tId)
  , ("method_id", showJSON meth)
  ]
trait_type m_res (TT_Getter (TraitMethod tId meth)) = kvToObject$
  [
    ("id", showJSON tId)
  , ("method_id", showJSON meth)
  ]
trait_type m_res (TT_Setter (TraitMethod tId meth)) = kvToObject$
  [
    ("id", showJSON tId)
  , ("method_id", showJSON meth)
  ]
trait_type m_res (TT_Class (TraitClass id init)) = kvToObject$
  [
    ("id", showJSON id)
  , ("init", showJSON init)
  ]
trait_type m_res (TT_Function (TraitFunction dispId func)) = kvToObject$
  [
    ("slot_id", showJSON dispId)
  , ("function_id", showJSON func)
  ]

trait_var :: (U30 -> String) -- multiname resolution
         -> TraitVar
         -> JSValue
trait_var m_res (TraitVar tid name idx kind) = kvToObject$
  [
    ("id", showJSON tid)
  , ("name", showJSON$ m_res name)
  , ("index", showJSON idx)
  , ("kind", showJSON$ maybe 0 id kind)
  ]

class_info :: (TraitsInfo -> JSValue) -- trait info resolution
           -> ClassInfo
           -> JSValue
class_info t_res (ClassInfo init traits) = kvToObject
  [
    ("init", showJSON init)
  , ("traits", JSArray$ map t_res traits)
  ]

script_info :: (TraitsInfo -> JSValue) -- trait info resolution
            -> ScriptInfo
            -> JSValue
script_info t_res (ScriptInfo init traits) = kvToObject
  [
    ("init", showJSON init)
  , ("traits", JSArray$ map t_res traits)
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
