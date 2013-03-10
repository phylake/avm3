{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Vm.Execute where

import           Abc.DeepSeq
import           Abc.Deserialize
import           Abc.Json
import           Abc.Json2
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Int
import           Data.Maybe (listToMaybe)
import           Data.Time.Clock
import           Data.Word
import           Ecma.Prims
import           Prelude hiding (lookup)
import           Text.JSON
import           TFish
import           Util.Misc
import           Util.Words
import           Vm.Def
import           Vm.Ecma
import           Vm.Lookups
import           Vm.Store
import qualified Abc.Def as Abc
import qualified Control.Monad.State as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import qualified Data.HashTable.IO as H
import qualified Data.Map as Map

p :: String -> AVM3 ()
--p = putStrLn
p _ = return ()
{-# INLINE p #-}

po :: D_Ops -> A_Ops -> B_Ops -> AVM3 ()
po dops aops bops = do
  p$ "run_function"
  p$ (unlines$ map (\s -> "\t" ++ show s) dops)
    ++ (unlines$ map (\s -> "\t" ++ show s) (reverse aops))
    ++ "\t--------------------" ++ "\n"
    ++ (unlines$ map (\s -> "\t" ++ show s) bops)
{-# INLINE po #-}

p_length :: B.ByteString
p_length = BC.pack "length"

returnR :: a -> AVM3 (Either AVM3Exception a)
returnR = return . Right
{-# INLINE returnR #-}

returnJ :: a -> AVM3 (Maybe a)
returnJ = return . Just
{-# INLINE returnJ #-}

returnN :: AVM3 (Maybe a)
returnN = return Nothing
{-# INLINE returnN #-}

avm_prefix :: String
avm_prefix = "avm3internal_"

insert :: VmObject -> VmRtP -> VmRt -> AVM3 ()
insert = H.insert
{-# INLINE insert #-}

lookup :: VmObject -> VmRtP -> AVM3 (Maybe VmRt)
lookup = H.lookup
{-# INLINE lookup #-}

pfx_class_info_idx :: VmRtP
pfx_class_info_idx = ClassIdx$ BC.pack$ avm_prefix ++ "class_info_idx"

new_object :: InstanceId -> AVM3 VmRt
new_object iid = do
  obj <- H.new
  return $ VmRt_Object obj $ iid+1

test_file = do
  (abc :: Abc.Abc) <- E.run_ (EB.enumFile "abc/Test.abc" E.$$ parseAbc)
  --writeFile "abc/Test.abc.json"$ encode$ abcToJson abc
  (either::Either AVM3Exception VmRt) <- abc `deepseq` execute_abc abc
  case either of
    Left err -> Prelude.putStrLn$ "EXCEPTION\n" ++ (unlines$ map ("\t"++)$ lines err)
    otherwise -> return ()

execute_abc :: Abc.Abc -> AVM3 (Either AVM3Exception VmRt)
execute_abc abc = do
  t0 <- getCurrentTime
  p$ "-------------------------------------------"
  (global, globalid) <- build_global_scope 0
  let ops = [NewClass idx, ReturnVoid]
  (vmrt, _) <- r_f ([], [], ops, [(global, globalid)], Map.fromList [(0, VmRt_Object global globalid)], cp, globalid+1)
  p$ "-------------------------------------------"
  t1 <- getCurrentTime
  putStrLn$ show$ diffUTCTime t1 t0
  return vmrt
  where
    cp = build_cp abc
    Abc.ScriptInfo _ ((Abc.TraitsInfo _ _ _ (Abc.TT_Class (Abc.TraitClass _ idx)) _):[]) = get_script cp 0

build_global_scope :: InstanceId -> AVM3 (VmObject, InstanceId)
build_global_scope iid = do
  (int :: VmObject) <- H.new
  insert int (Ext$ BC.pack "MAX_VALUE") (VmRt_Int 2147483647)

  (global :: VmObject) <- H.new
  insert global (Ext$ BC.pack "int") (VmRt_Object int next_iid)

  return (global, next_iid)
  where
    next_iid = iid + 1

jump :: Abc.S24 -> (A_Ops, B_Ops) -> (A_Ops, B_Ops)
jump s24 tuple
  | s24 > 0 = pos_jump s24 tuple
  | s24 < 0 = neg_jump (negate s24) tuple
  | otherwise = tuple
{-# INLINE jump #-}

pos_jump :: Abc.S24 -> (A_Ops, B_Ops) -> (A_Ops, B_Ops)
pos_jump _ (aops, []) = (aops, [])
pos_jump s24 t@(aops, bop:bops)
  | s24 > 0 = pos_jump (s24 - (fromIntegral$ toBytes bop)) (bop:aops, bops)
  | otherwise = t

neg_jump :: Abc.S24 -> (A_Ops, B_Ops) -> (A_Ops, B_Ops)
neg_jump _ ([], bops) = ([], bops)
neg_jump s24 t@(aop:aops, bops)
  | s24 > 0 = neg_jump (s24 - (fromIntegral$ toBytes aop)) (aops, aop:bops)
  | otherwise = t

{-
  Since interrupts wait for the stack to wind down I don't have to handle them
  explicitly in run_function
-}

{-
REMEMBER the stack order here is the REVERSE of the docs
  docs
  bottom, mid, top => newvalue

  pattern
  top, mid, bottom => newvalue
-}

r_f :: Execution -> AVM3 (Either AVM3Exception VmRt, InstanceId)
r_f {-0x08-} (dops, aops, Kill regIdx:bops, ss, reg, cp, iid) = do
  --po dops aops$ Kill regIdx:bops
  r_f (dops, Kill regIdx:aops, bops, ss, reg2, cp, iid)
  where
    reg2 = Map.insert (fromIntegral regIdx) VmRt_Undefined reg

r_f {-0x09-} (dops, aops, Label:bops, ss, reg, cp, iid) = do
  --po dops aops$ Label:bops
  r_f (dops, Label:aops, bops, ss, reg, cp, iid)

r_f {-0x10-} (dops, aops, Jump s24:bops, ss, reg, cp, iid) = do
  --po dops aops$ Jump s24:bops
  r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (Jump s24:aops, bops)

r_f {-0x11-} (a:dops, aops, IfTrue s24:bops, ss, reg, cp, iid) = do
  --po (a:dops) aops$ IfTrue s24:bops
  if to_boolean a == True
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfTrue s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfTrue s24:aops, bops)

r_f {-0x12-} (a:dops, aops, IfFalse s24:bops, ss, reg, cp, iid) = do
  --po (a:dops) aops$ IfFalse s24:bops
  if to_boolean a == False
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfFalse s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfFalse s24:aops, bops)

r_f {-0x13-} (a:b:dops, aops, IfEqual s24:bops, ss, reg, cp, iid) = do
  --po (a:b:dops) aops$ IfEqual s24:bops
  if b == a
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfEqual s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfEqual s24:aops, bops)

r_f {-0x14-} (a:b:dops, aops, IfNotEqual s24:bops, ss, reg, cp, iid) = do
  --po (a:b:dops) aops$ IfNotEqual s24:bops
  if b /= a
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfNotEqual s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfNotEqual s24:aops, bops)

r_f {-0x15-} (a:b:dops, aops, IfLessThan s24:bops, ss, reg, cp, iid) = do
  --po (a:b:dops) aops$ IfLessThan s24:bops
  if b < a
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfLessThan s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfLessThan s24:aops, bops)

r_f {-0x16-} (a:b:dops, aops, IfLessEqual s24:bops, ss, reg, cp, iid) = do
  --po (a:b:dops) aops$ IfLessEqual s24:bops
  if b <= a
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfLessEqual s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfLessEqual s24:aops, bops)

r_f {-0x17-} (a:b:dops, aops, IfGreaterThan s24:bops, ss, reg, cp, iid) = do
  --po (a:b:dops) aops$ IfGreaterThan s24:bops
  if b > a
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfGreaterThan s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfGreaterThan s24:aops, bops)

r_f {-0x18-} (a:b:dops, aops, IfGreaterEqual s24:bops, ss, reg, cp, iid) = do
  --po (a:b:dops) aops$ IfGreaterEqual s24:bops
  if b >= a
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfGreaterEqual s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfGreaterEqual s24:aops, bops)

r_f {-0x19-} (a:b:dops, aops, IfStrictEqual s24:bops, ss, reg, cp, iid) = do
  --po (a:b:dops) aops$ IfStrictEqual s24:bops
  if a == b -- TODO class StrictEq ===
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfStrictEqual s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfStrictEqual s24:aops, bops)

r_f {-0x1A-} (a:b:dops, aops, IfStrictNotEqual s24:bops, ss, reg, cp, iid) = do
  --po (a:b:dops) aops$ IfStrictNotEqual s24:bops
  if a /= b -- TODO class StrictEq /==
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfStrictNotEqual s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfStrictNotEqual s24:aops, bops)

r_f {-0x1D-} (dops, aops, PopScope:bops, (_:ss), reg, cp, iid) = do
  r_f (dops, PopScope:aops, bops, ss, reg, cp, iid)

r_f {-0x24-} (dops, aops, PushByte u8:bops, ss, reg, cp, iid) = do
  --po dops aops$ PushByte u8:bops
  r_f (VmRt_Int (fromIntegral u8):dops, PushByte u8:aops, bops, ss, reg, cp, iid)

r_f {-0x25-} (dops, aops, PushShort u30:bops, ss, reg, cp, iid) = do
  --po dops aops$ PushShort u30:bops
  r_f (VmRt_Uint u30:dops, PushShort u30:aops, bops, ss, reg, cp, iid)

r_f {-0x26-} (dops, aops, PushTrue:bops, ss, reg, cp, iid) = do
  --po dops aops$ PushTrue:bops
  r_f (VmRt_Boolean True:dops, PushTrue:aops, bops, ss, reg, cp, iid)

r_f {-0x27-} (dops, aops, PushFalse:bops, ss, reg, cp, iid) = do
  --po dops aops$ PushFalse:bops
  r_f (VmRt_Boolean False:dops, PushFalse:aops, bops, ss, reg, cp, iid)

r_f {-0x28-} (dops, aops, PushNaN:bops, ss, reg, cp, iid) = do
  --po dops aops$ PushNaN:bops
  r_f (VmRt_Number nan:dops, PushNaN:aops, bops, ss, reg, cp, iid)

r_f {-0x2A-} (a:dops, aops, Dup:bops, ss, reg, cp, iid) = do
  r_f (a:a:dops, Dup:aops, bops, ss, reg, cp, iid)

r_f {-0x2B-} (a:b:dops, aops, Swap:bops, ss, reg, cp, iid) = do
  r_f (b:a:dops, Swap:aops, bops, ss, reg, cp, iid)

r_f {-0x2C-} (dops, aops, PushString idx a:bops, ss, reg, cp, iid) = do
  --po dops aops$ PushString idx a:bops
  --a <- liftM VmRt_Int$ get_string cp idx
  r_f (VmRt_String a:dops, PushString idx a:aops, bops, ss, reg, cp, iid)

r_f {-0x2D-} (dops, aops, PushInt idx a:bops, ss, reg, cp, iid) = do
  --po dops aops$ PushInt idx a:bops
  --a <- liftM VmRt_Int$ get_int cp idx
  r_f (VmRt_Int a:dops, PushInt idx a:aops, bops, ss, reg, cp, iid)

r_f {-0x2E-} (dops, aops, PushUInt idx a:bops, ss, reg, cp, iid) = do
  --po dops aops$ PushUInt idx a:bops
  --a <- liftM VmRt_Uint$ get_uint cp idx
  r_f (VmRt_Uint a:dops, PushUInt idx a:aops, bops, ss, reg, cp, iid)

r_f {-0x2F-} (dops, aops, PushDouble idx a:bops, ss, reg, cp, iid) = do
  --po dops aops$ PushDouble idx a:bops
  --a <- liftM VmRt_Number$ get_double cp idx
  r_f (VmRt_Number a:dops, PushDouble idx a:aops, bops, ss, reg, cp, iid)

r_f {-0x30-} (VmRt_Object v i:dops, aops, PushScope:bops, ss, reg, cp, iid) = do
  --po (VmRt_Object v i:dops) aops$ PushScope:bops
  r_f (dops, PushScope:aops, bops, (v, i):ss, reg, cp, iid)

r_f {-0x47-} (dops, aops, ReturnVoid:bops, ss, reg, cp, iid) = return (Right VmRt_Undefined, iid)

r_f {-0x48-} (a:dops, aops, ReturnValue:bops, ss, reg, cp, iid) = return (Right a, iid)

-- TODO check for [ns] [name]
r_f {-0x4F-} (dops, aops, CallPropVoid idx args maybeName:bops, ss, reg, cp, iid) = do
  --po dops aops$ CallPropVoid idx args maybeName:bops
  
  maybeClassInfoIdx <- lookup this pfx_class_info_idx
  Abc.TraitsInfo _ _ _ (Abc.TT_Method (Abc.TraitMethod _ methodId)) _ <- case maybeClassInfoIdx of
    Just (VmRtInternalInt classIdx) -> do
      let Abc.ClassInfo _ traits = get_class cp classIdx
      let matchingTraits = filter (\(Abc.TraitsInfo idx2 _ _ _ _) -> idx2 == idx) traits
      p$ "got class info of pfx_class_info_idx" ++ show matchingTraits
      case length matchingTraits of
        0 -> fail "CallPropVoid - couldn't find matchingTraits"
        1 -> return$ head matchingTraits
        otherwise -> fail "CallPropVoid - too many matching traits"
    otherwise -> do
      name <- case maybeName of
        Nothing -> do
          let Abc.Multiname_QName nSInfoIdx stringIdx = get_multiname cp idx
          return$ B.append (resolve_nsinfo cp nSInfoIdx) (get_string cp stringIdx)
        Just n -> return n
      proplist <- H.toList this
      fail$ "couldn't find " ++ BC.unpack name ++ " on " ++ show proplist

  let MethodBody _ _ _ _ _ code _ _ = get_methodBody cp methodId
  p$ show code

  -- CallPropVoid, only need the latest instance id counter
  (_, iid2) <- r_f ([], [], code, [last ss], registers, cp, iid)

  r_f (dopsNew, CallPropVoid idx args maybeName:aops, bops, ss, reg, cp, iid2)
  where
    (nArgs, (VmRt_Object this iidThis):dopsNew) = splitAt (fromIntegral args) dops
    registers = Map.fromList$ (0, VmRt_Object this iidThis):zip [1..length nArgs] nArgs

r_f {-0x55-} (dops, aops, NewObject args:bops, ss, reg, cp, iid) = do
  --po dops aops$ NewObject args:bops
  vmrto@(VmRt_Object obj iid2) <- new_object iid
  forM_ (kvps nArgs)$ \(v, VmRt_String k) -> insert obj (Ext k) v
  r_f (vmrto:dopsNew, NewObject args:aops, bops, ss, reg, cp, iid2)
  where
    (nArgs, dopsNew) = splitAt (fromIntegral args*2) dops
    kvps (v:k:kvs) = (v, k):kvps kvs
    kvps [] = []

r_f {-0x56-} (dops, aops, NewArray args:bops, ss, reg, cp, iid) = do
  r_f (newArray:dopsNew, NewArray args:aops, bops, ss, reg, cp, iid2)
  where
    iid2 = iid + 1
    (nArgs, dopsNew) = splitAt (fromIntegral args) dops
    newArray = VmRt_Array (reverse nArgs) iid2

r_f {-0x58-} (dops, aops, NewClass idx:bops, ss, reg, cp, iid) = do
  p$ "########## NewClass " ++ show idx
  --p$ "\tops: " ++ show code

  vmrto@(VmRt_Object klass iid2) <- new_object iid
  -- store this class's identity in it
  insert klass pfx_class_info_idx (VmRtInternalInt idx)

  -- NewClass, only need the latest instance id counter
  (_, iid3) <- r_f ([], [], code, [(global, globalid)], Map.fromList [(0,vmrto)], cp, iid2)

  insert global (Ext$ BC.pack "Test") vmrto
  p$ "########## " ++ show idx

  r_f (dops, NewClass idx:aops, bops, ss, reg, cp, iid3)
  where
    Abc.ClassInfo msi _ = get_class cp idx
    MethodBody _ _ _ _ _ code _ _ = get_methodBody cp msi
    (global, globalid) = last ss

r_f {-0x5D-} (dops, aops, FindPropStrict idx maybeName:bops, ss, reg, cp, iid) = do
  --po dops aops$ FindPropStrict idx maybeName:bops
  
  name <- case maybeName of
    Nothing -> do
      let Abc.Multiname_QName nSInfoIdx stringIdx = get_multiname cp idx
      return$ B.append (resolve_nsinfo cp nSInfoIdx) (get_string cp stringIdx)
    Just n -> return n
  
  vmrt <- find_property name idx cp ss -- TODO this need error checking
  r_f (vmrt:dops, FindPropStrict idx maybeName:aops, bops, ss, reg, cp, iid)

r_f {-0x5E-} (dops, aops, FindProperty idx maybeName:bops, ss, reg, cp, iid) = do
  --po dops aops$ FindProperty idx maybeName:bops
  name <- case maybeName of
    Nothing -> do
      let Abc.Multiname_QName nSInfoIdx stringIdx = get_multiname cp idx
      return$ B.append (resolve_nsinfo cp nSInfoIdx) (get_string cp stringIdx)
    Just n -> return n
  vmrt <- find_property name idx cp ss -- TODO this need error checking
  r_f (vmrt:dops, FindProperty idx maybeName:aops, bops, ss, reg, cp, iid)

r_f {-0x61-} (value:dops, aops, SetProperty idx Nothing:bops, ss, reg, cp, iid) = do
  (VmRt_String prop, (VmRt_Object this iidThis):dopsNew, rewrite) <- case multiname of
    Abc.Multiname_QName _ stringIdx -> do
      let str = get_string cp stringIdx
      return (VmRt_String str, dops, True)
    Abc.Multiname_Multiname stringIdx _ -> do
      let str = get_string cp stringIdx
      return (VmRt_String str, dops, True)
    otherwise -> return (head dops, tail dops, False)
  insert this (Ext prop) value
  r_f (dopsNew, SetProperty idx Nothing:aops, bops, ss, reg, cp, iid)
  where
    multiname = get_multiname cp idx

r_f {-0x61-} (value:VmRt_Object this _:dops, aops, SetProperty idx (Just prop):bops, ss, reg, cp, iid) = do
  {-# SCC "SetProperty_idx_Just" #-} insert this (Ext prop) value
  r_f (dops, SetProperty idx (Just prop):aops, bops, ss, reg, cp, iid)

r_f {-0x62-} (dops, aops, GetLocal u30:bops, ss, reg, cp, iid) = do
  case maybeR of
    Nothing -> fail$ "GetLocal" ++ show u30 ++ " - register doesn't exist"
    Just r -> r_f (r:dops, GetLocal u30:aops, bops, ss, reg, cp, iid)
  where
    maybeR = Map.lookup (fromIntegral u30) reg

r_f {-0x63-} (new:dops, aops, SetLocal u30:bops, ss, reg, cp, iid) = do
  r_f (dops, SetLocal u30:aops, bops, ss, reg2, cp, iid)
  where
    reg2 = Map.insert (fromIntegral u30) new reg

r_f {-0x65-} (dops, aops, GetScopeObject idx:bops, ss, reg, cp, iid) =
  r_f (scopedObject:dops, GetScopeObject idx:aops, bops, ss, reg, cp, iid)
  where
    (obj, iid) = ss !! fromIntegral idx
    scopedObject = VmRt_Object obj iid

r_f {-0x66-} (dops, aops, GetProperty idx Nothing:bops, ss, reg, cp, iid) = do
  --po dops aops$ GetProperty idx Nothing:bops
  p$ show multiname
  (prop, vmrt:dopsNew) <- case multiname of
    Abc.Multiname_QName _ stringIdx -> do
      let str = get_string cp stringIdx
      return (VmRt_String str, dops)
    Abc.Multiname_Multiname stringIdx _ -> do
      let str = get_string cp stringIdx
      return (VmRt_String str, dops)
    otherwise -> return (head dops, tail dops)

  d <- case vmrt of
    VmRt_Undefined -> return VmRt_Undefined
    VmRt_Null -> return VmRt_Undefined
    VmRt_Boolean bool -> return VmRt_Undefined
    VmRt_Int v -> return VmRt_Undefined
    VmRt_Uint v -> return VmRt_Undefined
    VmRt_Number v -> return VmRt_Undefined
    VmRt_String v -> return VmRt_Undefined
    VmRt_Array v _ -> case prop of
      VmRt_String name -> case name of
        p_length -> return . VmRt_Int . fromIntegral$ length v
        otherwise -> fail$ "GetProperty - VmRt_Array - can't get property " ++ BC.unpack name
      VmRt_Int i -> return$ v !! fromIntegral i
    VmRt_Object this _ -> case prop of
      VmRt_String name -> do
        maybeProp <- lookup this$ Ext name
        case maybeProp of
          Just prop -> return prop
          Nothing -> return VmRt_Undefined
      otherwise -> fail "GetProperty - VmRt_Object - only strings"
  
  r_f (d:dopsNew, GetProperty idx Nothing:aops, bops, ss, reg, cp, iid)
  where
    multiname = get_multiname cp idx

r_f {-0x66-} (vmrt:dops, aops, GetProperty idx (Just prop):bops, ss, reg, cp, iid) = do
  --po dops aops$ GetProperty idx (Just prop):bops

  d <- {-# SCC "GetProperty_idx_Just" #-} case vmrt of
    VmRt_Undefined -> return VmRt_Undefined
    VmRt_Null -> return VmRt_Undefined
    VmRt_Boolean bool -> return VmRt_Undefined
    VmRt_Int v -> return VmRt_Undefined
    VmRt_Uint v -> return VmRt_Undefined
    VmRt_Number v -> return VmRt_Undefined
    VmRt_String v -> return VmRt_Undefined
    VmRt_Array v _ -> case prop of
      p_length -> return . VmRt_Int . fromIntegral$ length v
      otherwise -> fail$ "GetProperty - VmRt_Array - can't get property " ++ BC.unpack prop
    VmRt_Object this _ -> do
      maybeProp <- lookup this$ Ext prop
      case maybeProp of
        Just prop -> return prop
        Nothing -> return VmRt_Undefined

  r_f (d:dops, GetProperty idx (Just prop):aops, bops, ss, reg, cp, iid)

r_f {-0x68-} (value:VmRt_Object this iidThis:dops, aops, InitProperty idx maybeName:bops, ss, reg, cp, iid) = do
  --po (value:VmRt_Object this iidThis:dops) aops (InitProperty idx maybeName:bops)
  name <- case maybeName of
    Nothing -> do
      let Abc.Multiname_QName nSInfoIdx stringIdx = get_multiname cp idx
      return$ B.append (resolve_nsinfo cp nSInfoIdx) (get_string cp stringIdx)
    Just n -> return n
  p$ "InitProperty [" ++ show idx ++ " " ++ BC.unpack name ++ "]"
  maybeProp <- lookup this (Ext name)
  case maybeProp of
    Nothing -> do
      insert this (Ext name) value
      return ()
    otherwise -> fail$ "init_property - property [" ++ BC.unpack name ++ "] exists"
  r_f (dops, InitProperty idx maybeName:aops, bops, ss, reg, cp, iid)

r_f {-0x70-} (v:dops, aops, ConvertString:bops, ss, reg, cp, iid) = do
  r_f (convert_string v:dops, ConvertString:aops, bops, ss, reg, cp, iid)

r_f {-0x73-} (v:dops, aops, ConvertInt:bops, ss, reg, cp, iid) = do
  r_f (convert_int v:dops, ConvertInt:aops, bops, ss, reg, cp, iid)

r_f {-0x74-} (v:dops, aops, ConvertUInt:bops, ss, reg, cp, iid) = do
  r_f (convert_uint v:dops, ConvertUInt:aops, bops, ss, reg, cp, iid)

r_f {-0x75-} (v:dops, aops, ConvertDouble:bops, ss, reg, cp, iid) = do
  r_f (convert_double v:dops, ConvertDouble:aops, bops, ss, reg, cp, iid)

r_f {-0x76-} (v:dops, aops, ConvertBoolean:bops, ss, reg, cp, iid) = do
  r_f (convert_boolean v:dops, ConvertBoolean:aops, bops, ss, reg, cp, iid)

r_f {-0x77-} (v:dops, aops, ConvertObject:bops, ss, reg, cp, iid) = fail "NI: ConvertObject"

r_f {-0x90-} (a:dops, aops, Negate:bops, ss, reg, cp, iid) = do
  vmrt <- case a of
    VmRt_Int i -> return$ VmRt_Int$ negate i
    VmRt_Uint i -> return$ VmRt_Uint$ negate i
    VmRt_Number i -> return$ VmRt_Number$ negate i
    otherwise -> fail$ "can't negate " ++ show a
  r_f (vmrt:dops, Negate:aops, bops, ss, reg, cp, iid)

r_f {-0x91-} (a:dops, aops, Increment:bops, ss, reg, cp, iid) =
  r_f (a + 1:dops, Increment:aops, bops, ss, reg, cp, iid)

r_f {-0x93-} (a:dops, aops, Decrement:bops, ss, reg, cp, iid) =
  r_f (a - 1:dops, Decrement:aops, bops, ss, reg, cp, iid)

r_f {-0xA0-} (a:b:dops, aops, Add:bops, ss, reg, cp, iid) =
  r_f (b + a:dops, Add:aops, bops, ss, reg, cp, iid)

r_f {-0xA1-} (a:b:dops, aops, Subtract:bops, ss, reg, cp, iid) =
  r_f (b - a:dops, Subtract:aops, bops, ss, reg, cp, iid)

r_f {-0xA2-} (a:b:dops, aops, Multiply:bops, ss, reg, cp, iid) =
  r_f (b * a:dops, Multiply:aops, bops, ss, reg, cp, iid)

r_f {-0xA3-} (a:b:dops, aops, Divide:bops, ss, reg, cp, iid) =
  r_f (b / a:dops, Divide:aops, bops, ss, reg, cp, iid)

r_f {-0xC0-} (a:dops, aops, IncrementInt:bops, ss, reg, cp, iid) =
  r_f (a + 1:dops, IncrementInt:aops, bops, ss, reg, cp, iid)

r_f {-0xC1-} (a:dops, aops, DecrementInt:bops, ss, reg, cp, iid) =
  r_f (a - 1:dops, DecrementInt:aops, bops, ss, reg, cp, iid)

r_f {-0xC2-} (dops, aops, IncLocalInt regIdx:bops, ss, reg, cp, iid) = do
  case maybeReg of
    Nothing -> fail$ "register " ++ show regIdx ++ " didn't exist"
    Just r -> do
      let reg2 = Map.insert (fromIntegral regIdx) (r + 1) reg
      r_f (dops, IncLocalInt regIdx:aops, bops, ss, reg2, cp, iid)
  where
    maybeReg = Map.lookup (fromIntegral regIdx) reg

r_f {-0xC3-} (dops, aops, DecLocalInt regIdx:bops, ss, reg, cp, iid) = do
  case maybeReg of
    Nothing -> fail$ "register " ++ show regIdx ++ " didn't exist"
    Just r -> do
      let reg2 = Map.insert (fromIntegral regIdx) (r - 1) reg
      r_f (dops, DecLocalInt regIdx:aops, bops, ss, reg2, cp, iid)
  where
    maybeReg = Map.lookup (fromIntegral regIdx) reg

r_f {-0xD0-} (dops, aops, GetLocal0:bops, ss, reg, cp, iid) = do
  --po dops aops$ GetLocal0:bops
  case maybeR of
    Nothing -> fail "GetLocal0 - register doesn't exist"
    Just r -> r_f (r:dops, GetLocal0:aops, bops, ss, reg, cp, iid)
  where
    maybeR = Map.lookup 0 reg

r_f {-0xD1-} (dops, aops, GetLocal1:bops, ss, reg, cp, iid) = do
  --po dops aops$ GetLocal1:bops
  case maybeR of
    Nothing -> fail "GetLocal1 - register doesn't exist"
    Just r -> r_f (r:dops, GetLocal1:aops, bops, ss, reg, cp, iid)
  where
    maybeR = Map.lookup 1 reg

r_f {-0xD2-} (dops, aops, GetLocal2:bops, ss, reg, cp, iid) = do
  --po dops aops$ GetLocal2:bops
  case maybeR of
    Nothing -> fail "GetLocal2 - register doesn't exist"
    Just r -> r_f (r:dops, GetLocal2:aops, bops, ss, reg, cp, iid)
  where
    maybeR = Map.lookup 2 reg

r_f {-0xD3-} (dops, aops, GetLocal3:bops, ss, reg, cp, iid) = do
  --po dops aops$ GetLocal3:bops
  case maybeR of
    Nothing -> fail "GetLocal3 - register doesn't exist"
    Just r -> r_f (r:dops, GetLocal3:aops, bops, ss, reg, cp, iid)
  where
    maybeR = Map.lookup 3 reg

r_f {-0xD4-} (new:dops, aops, SetLocal0:bops, ss, reg, cp, iid) =
  r_f (dops, SetLocal0:aops, bops, ss, Map.insert 0 new reg, cp, iid)

r_f {-0xD5-} (new:dops, aops, SetLocal1:bops, ss, reg, cp, iid) =
  r_f (dops, SetLocal1:aops, bops, ss, Map.insert 1 new reg, cp, iid)

r_f {-0xD6-} (new:dops, aops, SetLocal2:bops, ss, reg, cp, iid) =
  r_f (dops, SetLocal2:aops, bops, ss, Map.insert 2 new reg, cp, iid)

r_f {-0xD7-} (new:dops, aops, SetLocal3:bops, ss, reg, cp, iid) =
  r_f (dops, SetLocal3:aops, bops, ss, Map.insert 3 new reg, cp, iid)

r_f {-0xD7-} (dops, aops, op:bops, ss, reg, cp, iid) = fail$ "didn't match opcode " ++ show op

convert_string :: VmRt -> VmRt
convert_string = VmRt_String . BC.pack . to_string

convert_double :: VmRt -> VmRt
convert_double = VmRt_Number . to_number

convert_boolean :: VmRt -> VmRt
convert_boolean = VmRt_Boolean . to_boolean

convert_int :: VmRt -> VmRt
convert_int = VmRt_Int . to_int32

convert_uint :: VmRt -> VmRt
convert_uint = VmRt_Uint . to_uint32

new_function :: AVM3 ()
new_function = undefined

{- TODO dynamic multinames (pattern match Ops) -}
{-
TODO resolve against
  1. method closures
  2. declared traits (on the method body, instance info?, class info?)
  3. dynamic properties
  4. prototype chain
  5. script traits (global object (for classes))
-}
find_property :: B.ByteString -> Abc.MultinameIdx -> ConstantPool -> ScopeStack -> AVM3 VmRt
find_property name idx cp ss = do
  attempt1 <- search_stack (Ext name) ss
  case attempt1 of
    Just obj -> return obj
    Nothing -> do
      attempt2 <- traits_ref cp idx ss
      case attempt2 of
        Just obj -> return obj
        Nothing -> fail "find_property - couldn't find_property"
  where
    search_stack :: VmRtP -> ScopeStack -> AVM3 (Maybe VmRt)
    search_stack key [] = returnN
    search_stack key@(Ext str) ((top, iid):stack) = do
      maybeValue <- lookup top key
      case maybeValue of
        Nothing -> search_stack key stack
        Just value -> returnJ$ VmRt_Object top iid

    traits_ref :: ConstantPool -> Abc.MultinameIdx -> ScopeStack -> AVM3 (Maybe VmRt)
    traits_ref cp idx [] = returnN
    traits_ref cp idx ((top, iid):stack) = do
      maybeClassInfoIdx <- lookup top pfx_class_info_idx
      case maybeClassInfoIdx of
        Just (VmRtInternalInt classIdx) -> do
          let Abc.ClassInfo _ traits = get_class cp classIdx
          let matchingTraits = filter (\(Abc.TraitsInfo idx2 _ _ _ _) -> idx2 == idx) traits
          --p$ "got class info of pfx_class_info_idx" ++ show matchingTraits
          case length matchingTraits of
            0 -> returnN
            1 -> returnJ$ VmRt_Object top iid
            otherwise -> fail "traits_ref - too many matching traits"
        otherwise -> traits_ref cp idx stack

{-
  "The indexing of elements on the local scope stack is the reverse of the
  indexing of elements on the local operand stack."
-}

{-get_scoped_object :: ScopeStack -> U8 -> AVM3 Ops
get_scoped_object ss idx = undefined-}
