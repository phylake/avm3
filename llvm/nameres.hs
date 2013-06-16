module LLVM.NameRes (getResolutionMethods) where

import Abc.Def as Abc

getResolutionMethods :: Abc -> (
                                 U30 -> S32          -- int
                               , U30 -> U30          -- uint
                               , U30 -> Double       -- double
                               , U30 -> String       -- string
                               , U30 -> Maybe String -- multiname
                               )
getResolutionMethods (Abc ints uints doubles strings nsInfo nsSet multinames methodSigs metadata instances classes scripts methodBodies) =
    (
      int_res
    , uint_res
    , double_res
    , string_res
    , multiname_res
    )
  where
    int_res :: U30 -> S32
    int_res i = ints !! fromIntegral i

    uint_res :: U30 -> U30
    uint_res i = uints !! fromIntegral i

    double_res :: U30 -> Double
    double_res i = doubles !! fromIntegral i

    string_res :: U30 -> String
    string_res i = strings !! fromIntegral i

    multiname_res :: U30 -> Maybe String
    multiname_res i = multiname_impl string_res nsinfo_res $ multinames !! fromIntegral i

    nsinfo_res :: U30 -> String
    nsinfo_res i = nsinfo_impl string_res $ nsInfo !! fromIntegral i

multiname_impl :: (U30 -> String) -- string resolution
               -> (U30 -> String) -- nsinfo resolution
               -> Multiname
               -> Maybe String
multiname_impl string_res nsinfo_res (Multiname_QName ns str)
  | nsinfo == "" = Just string
  | otherwise = Just $ nsinfo ++ "_" ++ string
  where
    nsinfo = nsinfo_res ns
    string = string_res str
multiname_impl string_res nsinfo_res (Multiname_QNameA ns str)
  | nsinfo == "" = Just string
  | otherwise = Just $ nsinfo ++ "_" ++ string
  where
    nsinfo = nsinfo_res ns
    string = string_res str
multiname_impl string_res nsinfo_res (Multiname_Multiname str ns)
  | nsinfo == "" = Just string
  | otherwise = Just $ nsinfo ++ "_" ++ string
  where
    nsinfo = nsinfo_res ns
    string = string_res str
multiname_impl string_res nsinfo_res (Multiname_MultinameA str ns)
  | nsinfo == "" = Just string
  | otherwise = Just $ nsinfo ++ "_" ++ string
  where
    nsinfo = nsinfo_res ns
    string = string_res str
multiname_impl _ _ (Multiname_RTQName a) = Nothing
multiname_impl _ _ (Multiname_RTQNameA a) = Nothing
multiname_impl _ _ (Multiname_MultinameL a) = Nothing
multiname_impl _ _ (Multiname_MultinameLA a) = Nothing
multiname_impl _ _ Multiname_Any = Nothing {-Just "*"-}

nsinfo_impl :: (U30 -> String) -- string resolution
            -> NSInfo
            -> String
nsinfo_impl string_res (NSInfo_Namespace a)          = string_res a
nsinfo_impl string_res (NSInfo_PackageNamespace a)   = string_res a
nsinfo_impl string_res (NSInfo_PackageInternalNs a)  = string_res a
nsinfo_impl string_res (NSInfo_ProtectedNamespace a) = string_res a
nsinfo_impl string_res (NSInfo_ExplicitNamespace a)  = string_res a
nsinfo_impl string_res (NSInfo_StaticProtectedNs a)  = string_res a
nsinfo_impl string_res (NSInfo_PrivateNs a)          = string_res a
nsinfo_impl string_res NSInfo_Any                    = "*"
