module Abc.Json () where

import Abc.Def
import Data.Word
import Text.JSON

kvToObject :: [(String, JSValue)] -> JSValue
kvToObject = JSObject. toJSObject

toArray :: JSON a => [a] -> JSValue
toArray = JSArray. map showJSON

instance JSON Abc where
  readJSON = undefined
  showJSON (Abc i ui d s nsi nss names sigs meta inst klas rx bodies) = kvToObject
    [
      ("ints", toArray i)
    , ("uints", toArray ui)
    , ("doubles", toArray d)
    , ("strings", toArray s)
    , ("ns_infos", toArray nsi)
    , ("ns_sets", toArray nss)
    , ("multinames", toArray names)
    , ("method_signatures", toArray sigs)
    , ("method_bodies", toArray bodies)
    , ("metadata", toArray meta)
    , ("instance_infos", toArray inst)
    , ("class_infos", toArray klas)
    , ("scripts", toArray rx)
    ]

instance JSON NSInfo where
  readJSON = undefined
  showJSON o = JSString$ toJSString$ show o

instance JSON Multiname where
  readJSON = undefined
  showJSON o = JSString$ toJSString$ show o

instance JSON MethodSignature where
  readJSON = undefined
  showJSON (MethodSignature ret ptypes name flags options pnames) = kvToObject
    [
      ("name", showJSON name)
    , ("ret", showJSON ret)
    , ("flags", showJSON flags)
    , ("param_types", toArray ptypes)
    , ("param_names", maybe (JSArray []) toArray pnames)
    , ("options", maybe (JSArray []) toArray options)
    ]

instance JSON CPC where
  readJSON = undefined
  showJSON o = JSString$ toJSString$ show o

instance JSON Metadata where
  readJSON = undefined
  showJSON (Metadata name kvps) = kvToObject
    [
      ("name", showJSON name)
    , ("kvps", toArray$ map (\(k,v) -> (k, showJSON v)) kvps )
    ]

instance JSON InstanceInfo where
  readJSON = undefined
  showJSON (InstanceInfo name super flags ns ifaces init traits) = kvToObject
    [
      ("name", showJSON name)
    , ("super_name", showJSON super)
    , ("init", showJSON init)
    , ("flags", showJSON flags)
    , ("interfaces", toArray ifaces)
    , ("traits", toArray traits)
    , ("ns_info", maybe JSNull showJSON ns)
    ]

instance JSON TraitsInfo where
  readJSON = undefined
  showJSON (TraitsInfo name final override t meta) = kvToObject
    [
      ("multiname", showJSON name)
    , ("final", showJSON final)
    , ("override", showJSON override)
    , ("type", showJSON t)
    , ("meta", maybe (JSArray []) toArray meta)
    ]

instance JSON TraitType where
  readJSON = undefined
  showJSON (TraitVar id name index kind) = kvToObject
    [
      ("id", showJSON id)
    , ("name", showJSON name)
    , ("index", showJSON index)
    , ("kind", maybe JSNull showJSON kind)
    ]
  showJSON (TraitMethod id msi) = kvToObject
    [
      ("id", showJSON id)
    , ("method_id", showJSON msi)
    ]
  showJSON (TraitGetter id msi) = kvToObject
    [
      ("id", showJSON id)
    , ("method_id", showJSON msi)
    ]
  showJSON (TraitSetter id msi) = kvToObject
    [
      ("id", showJSON id)
    , ("method_id", showJSON msi)
    ]
  showJSON (TraitClass id cii) = kvToObject
    [
      ("id", showJSON id)
    , ("class_info", showJSON cii)
    ]
  showJSON (TraitFunction id msi) = kvToObject
    [
      ("id", showJSON id)
    , ("method_id", showJSON msi)
    ]
  showJSON (TraitConst id name index kind) = kvToObject
    [
      ("id", showJSON id)
    , ("name", showJSON name)
    , ("index", showJSON index)
    , ("kind", maybe JSNull showJSON kind)
    ]

instance JSON ClassInfo where
  readJSON = undefined
  showJSON (ClassInfo init traits) = kvToObject
    [
      ("init", showJSON init)
    , ("traits", toArray traits)
    ]

instance JSON ScriptInfo where
  readJSON = undefined
  showJSON (ScriptInfo init traits) = kvToObject
    [
      ("init", showJSON init)
    , ("traits", toArray traits)
    ]

instance JSON MethodBody where
  readJSON = undefined
  showJSON (MethodBody init max local initD scopeD code exceptions traits) = kvToObject
    [
      ("method_id", showJSON init)
    , ("max_stack", showJSON max)
    , ("local_count", showJSON local)
    , ("init_scope_depth", showJSON initD)
    , ("max_scope_depth", showJSON scopeD)
    , ("code", toArray code)
    , ("exceptions", toArray exceptions)
    , ("traits", toArray traits)
    ]

instance JSON Exception where
  readJSON = undefined
  showJSON (Exception from to target t varname) = kvToObject
    [
      ("from", showJSON from)
    , ("to", showJSON to)
    , ("target", showJSON target)
    , ("type", showJSON t)
    , ("varname", showJSON varname)
    ]

instance JSON OpCode where
  readJSON = undefined
  showJSON o = JSString$ toJSString$ show o
