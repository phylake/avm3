{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Vm.Execute where

import           Control.Monad.Identity

import           Abc.DeepSeq
import           Abc.Deserialize
import           Abc.Json
import           Abc.Json2
import           Control.Applicative ((<|>))
import           Control.DeepSeq
import           Control.Monad
import           Data.Int
import           Data.Maybe (listToMaybe)
import           Data.Time.Clock
import           Data.Vector ((//), (!))
import           Data.Word
import           Ecma.Prims
import           Prelude hiding (lookup)
import           Text.JSON
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
import qualified Data.Vector as V

p :: String -> AVM3 ()
p = putStrLn
--p _ = return ()
{-# INLINE p #-}

po :: D_Ops -> A_Ops -> B_Ops -> AVM3 ()
po dops aops bops = do
  p$ "run_function"
  p$ (unlines$ map (\s -> "\t" ++ show s) dops)
    ++ (unlines$ map (\s -> "\t" ++ show s) (reverse aops))
    ++ "\t--------------------" ++ "\n"
    ++ (unlines$ map (\s -> "\t" ++ show s) bops)
{-# INLINE po #-}

ic_length :: B.ByteString
ic_length = BC.pack "length"

ic_function :: B.ByteString
ic_function = BC.pack "Function"

returnR :: a -> AVM3 (Either AVM3Exception a)
returnR = return . Right
{-# INLINE returnR #-}

returnJ :: a -> AVM3 (Maybe a)
returnJ = return . Just
{-# INLINE returnJ #-}

returnN :: AVM3 (Maybe a)
returnN = return Nothing
{-# INLINE returnN #-}

insert :: VmObject -> VmRtP -> VmRt -> AVM3 ()
insert = H.insert
{-# INLINE insert #-}

lookup :: VmObject -> VmRtP -> AVM3 (Maybe VmRt)
lookup = H.lookup
{-# INLINE lookup #-}

avm_prefix :: String
avm_prefix = "avm3internal_"

pfx_class_info_idx :: VmRtP
pfx_class_info_idx = ClassIdx$ BC.pack$ avm_prefix ++ "class_info_idx"
{-# INLINE pfx_class_info_idx #-}

new_object :: InstanceId -> AVM3 VmRt
new_object iid = do
  obj <- H.new
  return$ VmRt_Object obj$ iid+1

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
  cp <- build_cp abc
  Abc.ScriptInfo _ ((Abc.TraitsInfo _ _ _ (Abc.TraitClass _ idx) _):[]) <- get_script cp 0
  p$ "-------------------------------------------"
  (global, globalid) <- build_global_scope 0
  let ops = [NewClass idx, ReturnVoid]
  (vmrt, _) <- r_f ([], [], ops, [(global, globalid)], V.fromList [VmRt_Object global globalid], cp, globalid+1)
  p$ "-------------------------------------------"
  t1 <- getCurrentTime
  putStrLn$ show$ diffUTCTime t1 t0
  return vmrt

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

{-Pushshort       10000
Setlocal2       
Pushbyte        0
Setlocal3       
Jump            L1


L2: 
Label           
GetLocal2       
Decrement_i     
SetLocal2       
IncLocal_i      3

L1: 
GetLocal2       
GetLocal3       
IfGreaterThan            L2-}

theops :: [OpCode]
theops =
  [
    {-PushByte 1000000
  , SetLocal2
  , -}PushByte 0
  , SetLocal3
  , Jump 5
  , Label
  , DecLocalInt 2
  , IncLocalInt 3
  , GetLocal2
  , GetLocal3
  , IfGreaterThan (-11)
  , GetLocal2
  , GetLocal3
  , Add
  , ReturnValue
  ]

foo :: IO ()
foo = do
  (Right (VmRt_Int i), _) <- r_f2 ([], [], theops, [], V.fromList [VmRt_Undefined, VmRt_Undefined, VmRt_Int 1000000, VmRt_Undefined], V.empty, 0)
  putStrLn $ show i
  return ()
  --where
  --  (Right (VmRt_Int i), _) = r_f2 ([], [], theops, [], V.fromList [VmRt_Undefined, VmRt_Undefined, VmRt_Int 1000000, VmRt_Undefined], V.empty, 0)

r_f2 :: (D_Ops, A_Ops, B_Ops, ScopeStack, Registers, V.Vector VmRt, InstanceId) -> IO (Either AVM3Exception VmRt, InstanceId)
--r_f2 :: (D_Ops, A_Ops, B_Ops, ScopeStack, Registers, V.Vector VmRt, InstanceId) -> (Either AVM3Exception VmRt, InstanceId)
r_f2 {-0x48-} (a:dops, aops, ReturnValue:bops, ss, reg, cp, iid) = {-do-} --doo
  --po (a:dops) aops$ ReturnValue:bops
  return (Right a, iid)
  --(Right a, iid)

r_f2 {-0x24-} (dops, aops, PushByte u8:bops, ss, reg, cp, iid) = {-do-} --doo
  --po dops aops$ PushByte u8:bops
  r_f2 (VmRt_Int (fromIntegral u8):dops, PushByte u8:aops, bops, ss, reg, cp, iid)

r_f2 {-0xD2-} (dops, aops, GetLocal2:bops, ss, reg, cp, iid) = {-do-} --doo
  --po dops aops$ GetLocal2:bops
  r_f2 (reg ! 2:dops, GetLocal2:aops, bops, ss, reg, cp, iid)

r_f2 {-0xD3-} (dops, aops, GetLocal3:bops, ss, reg, cp, iid) = {-do-} --doo
  --po dops aops$ GetLocal3:bops
  r_f2 (reg ! 3:dops, GetLocal3:aops, bops, ss, reg, cp, iid)

r_f2 {-0xD6-} (new:dops, aops, SetLocal2:bops, ss, reg, cp, iid) = {-do-} --doo
  --po (new:dops) aops$ SetLocal2:bops
  r_f2 (dops, SetLocal2:aops, bops, ss, reg // [(2, new)], cp, iid)

r_f2 {-0xD7-} (new:dops, aops, SetLocal3:bops, ss, reg, cp, iid) = {-do-} --doo
  --po (new:dops) aops$ SetLocal3:bops
  r_f2 (dops, SetLocal3:aops, bops, ss, reg // [(3, new)], cp, iid)

r_f2 {-0x09-} (dops, aops, Label:bops, ss, reg, cp, iid) = {-do-} --doo
  --po dops aops$ Label:bops
  r_f2 (dops, Label:aops, bops, ss, reg, cp, iid)

r_f2 {-0xA0-} (a:b:dops, aops, Add:bops, ss, reg, cp, iid) = {-do-} --doo
  --po (a:b:dops) aops$ Add:bops
  r_f2 (b + a:dops, Add:aops, bops, ss, reg, cp, iid)

r_f2 {-0x17-} (a:b:dops, aops, IfGreaterThan s24:bops, ss, reg, cp, iid) = {-do-} --doo
  --po (a:b:dops) aops$ IfGreaterThan s24:bops
  if b > a
    then r_f2 (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f2 (dops, IfGreaterThan s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfGreaterThan s24:aops, bops)

r_f2 {-0x10-} (dops, aops, Jump s24:bops, ss, reg, cp, iid) = {-do-} --doo
  --po dops aops$ Jump s24:bops
  r_f2 (dops, aopsNew, bopsNew, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (Jump s24:aops, bops)

r_f2 {-0xC2-} (dops, aops, IncLocalInt regIdx:bops, ss, reg, cp, iid) = {-do-} --doo
  --po dops aops$ IncLocalInt regIdx:bops
  r_f2 (dops, IncLocalInt regIdx:aops, bops, ss, reg2, cp, iid)
  where
    reg2 = reg // [(iidx, (reg ! iidx) + 1)]
    iidx = fromIntegral regIdx

r_f2 {-0xC2-} (dops, aops, DecLocalInt regIdx:bops, ss, reg, cp, iid) = {-do-} --doo
  --po dops aops$ DecLocalInt regIdx:bops
  r_f2 (dops, DecLocalInt regIdx:aops, bops, ss, reg2, cp, iid)
  where
    reg2 = reg // [(iidx, (reg ! iidx) - 1)]
    iidx = fromIntegral regIdx

r_f :: Execution -> AVM3 (Either AVM3Exception VmRt, InstanceId)
r_f {-0x08-} (dops, aops, Kill regIdx:bops, ss, reg, cp, iid) = do
  po dops aops$ Kill regIdx:bops
  r_f (dops, Kill regIdx:aops, bops, ss, reg2, cp, iid)
  where
    reg2 = reg // [(fromIntegral regIdx, VmRt_Undefined)]

r_f {-0x09-} (dops, aops, Label:bops, ss, reg, cp, iid) = do
  po dops aops$ Label:bops
  r_f (dops, Label:aops, bops, ss, reg, cp, iid)

r_f {-0x10-} (dops, aops, Jump s24:bops, ss, reg, cp, iid) = do
  po dops aops$ Jump s24:bops
  r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (Jump s24:aops, bops)

r_f {-0x11-} (a:dops, aops, IfTrue s24:bops, ss, reg, cp, iid) = do
  po (a:dops) aops$ IfTrue s24:bops
  if to_boolean a == True
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfTrue s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfTrue s24:aops, bops)

r_f {-0x12-} (a:dops, aops, IfFalse s24:bops, ss, reg, cp, iid) = do
  po (a:dops) aops$ IfFalse s24:bops
  if to_boolean a == False
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfFalse s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfFalse s24:aops, bops)

r_f {-0x13-} (a:b:dops, aops, IfEqual s24:bops, ss, reg, cp, iid) = do
  po (a:b:dops) aops$ IfEqual s24:bops
  if b == a
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfEqual s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfEqual s24:aops, bops)

r_f {-0x14-} (a:b:dops, aops, IfNotEqual s24:bops, ss, reg, cp, iid) = do
  po (a:b:dops) aops$ IfNotEqual s24:bops
  if b /= a
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfNotEqual s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfNotEqual s24:aops, bops)

r_f {-0x15-} (a:b:dops, aops, IfLessThan s24:bops, ss, reg, cp, iid) = do
  po (a:b:dops) aops$ IfLessThan s24:bops
  if b < a
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfLessThan s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfLessThan s24:aops, bops)

r_f {-0x16-} (a:b:dops, aops, IfLessEqual s24:bops, ss, reg, cp, iid) = do
  po (a:b:dops) aops$ IfLessEqual s24:bops
  if b <= a
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfLessEqual s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfLessEqual s24:aops, bops)

r_f {-0x17-} (a:b:dops, aops, IfGreaterThan s24:bops, ss, reg, cp, iid) = do
  po (a:b:dops) aops$ IfGreaterThan s24:bops
  if b > a
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfGreaterThan s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = {-# SCC "IfGreaterThan.jump" #-} jump s24 (IfGreaterThan s24:aops, bops)

r_f {-0x18-} (a:b:dops, aops, IfGreaterEqual s24:bops, ss, reg, cp, iid) = do
  po (a:b:dops) aops$ IfGreaterEqual s24:bops
  if b >= a
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfGreaterEqual s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfGreaterEqual s24:aops, bops)

r_f {-0x19-} (a:b:dops, aops, IfStrictEqual s24:bops, ss, reg, cp, iid) = do
  po (a:b:dops) aops$ IfStrictEqual s24:bops
  if a == b -- TODO class StrictEq ===
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfStrictEqual s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfStrictEqual s24:aops, bops)

r_f {-0x1A-} (a:b:dops, aops, IfStrictNotEqual s24:bops, ss, reg, cp, iid) = do
  po (a:b:dops) aops$ IfStrictNotEqual s24:bops
  if a /= b -- TODO class StrictEq /==
    then r_f (dops, aopsNew, bopsNew, ss, reg, cp, iid)
    else r_f (dops, IfStrictNotEqual s24:aops, bops, ss, reg, cp, iid)
  where
    (aopsNew, bopsNew) = jump s24 (IfStrictNotEqual s24:aops, bops)

r_f {-0x1D-} (dops, aops, PopScope:bops, (_:ss), reg, cp, iid) = do
  po dops aops$ PopScope:bops
  r_f (dops, PopScope:aops, bops, ss, reg, cp, iid)

r_f {-0x24-} (dops, aops, PushByte u8:bops, ss, reg, cp, iid) = do
  po dops aops$ PushByte u8:bops
  r_f (VmRt_Int (fromIntegral u8):dops, PushByte u8:aops, bops, ss, reg, cp, iid)

r_f {-0x25-} (dops, aops, PushShort u30:bops, ss, reg, cp, iid) = do
  po dops aops$ PushShort u30:bops
  r_f (VmRt_Uint u30:dops, PushShort u30:aops, bops, ss, reg, cp, iid)

r_f {-0x26-} (dops, aops, PushTrue:bops, ss, reg, cp, iid) = do
  po dops aops$ PushTrue:bops
  r_f (VmRt_Boolean True:dops, PushTrue:aops, bops, ss, reg, cp, iid)

r_f {-0x27-} (dops, aops, PushFalse:bops, ss, reg, cp, iid) = do
  po dops aops$ PushFalse:bops
  r_f (VmRt_Boolean False:dops, PushFalse:aops, bops, ss, reg, cp, iid)

r_f {-0x28-} (dops, aops, PushNaN:bops, ss, reg, cp, iid) = do
  po dops aops$ PushNaN:bops
  r_f (VmRt_Number nan:dops, PushNaN:aops, bops, ss, reg, cp, iid)

r_f {-0x29-} (a:dops, aops, Pop:bops, ss, reg, cp, iid) = do
  po (a:dops) aops$ Pop:bops
  r_f (dops, Pop:aops, bops, ss, reg, cp, iid)

r_f {-0x2A-} (a:dops, aops, Dup:bops, ss, reg, cp, iid) = do
  po (a:dops) aops$ Dup:bops
  r_f (a:a:dops, Dup:aops, bops, ss, reg, cp, iid)

r_f {-0x2B-} (a:b:dops, aops, Swap:bops, ss, reg, cp, iid) = do
  r_f (b:a:dops, Swap:aops, bops, ss, reg, cp, iid)

r_f {-0x2C-} (dops, aops, PushString idx a:bops, ss, reg, cp, iid) = do
  po dops aops$ PushString idx a:bops
  r_f (VmRt_String a:dops, PushString idx a:aops, bops, ss, reg, cp, iid)

r_f {-0x2D-} (dops, aops, PushInt idx a:bops, ss, reg, cp, iid) = do
  po dops aops$ PushInt idx a:bops
  r_f (VmRt_Int a:dops, PushInt idx a:aops, bops, ss, reg, cp, iid)

r_f {-0x2E-} (dops, aops, PushUInt idx a:bops, ss, reg, cp, iid) = do
  po dops aops$ PushUInt idx a:bops
  r_f (VmRt_Uint a:dops, PushUInt idx a:aops, bops, ss, reg, cp, iid)

r_f {-0x2F-} (dops, aops, PushDouble idx a:bops, ss, reg, cp, iid) = do
  po dops aops$ PushDouble idx a:bops
  r_f (VmRt_Number a:dops, PushDouble idx a:aops, bops, ss, reg, cp, iid)

r_f {-0x30-} (VmRt_Object v i:dops, aops, PushScope:bops, ss, reg, cp, iid) = do
  po (VmRt_Object v i:dops) aops$ PushScope:bops
  r_f (dops, PushScope:aops, bops, (v, i):ss, reg, cp, iid)

r_f {-0x30-} (VmRt_Activation v:dops, aops, PushScope:bops, ss, reg, cp, iid) = do
  po (VmRt_Activation v:dops) aops$ PushScope:bops
  r_f (dops, PushScope:aops, bops, (v, -1):ss, reg, cp, iid)

r_f {-0x40-} (dops, aops, NewFunction idx:bops, ss, reg, cp, iid) = do
  po dops aops$ NewFunction idx:bops
  MethodBody _ _ _ _ code _ _ <- get_methodBody cp idx
  let function = VmRt_Function code ss iid2
  r_f (function:dops, NewFunction idx:aops, bops, ss, reg, cp, iid2)
  where
    iid2 = iid+1

r_f {-0x41-} (dops, aops, Call args:bops, ss, reg, cp, iid) = do
  po dops aops$ Call args:bops
  (ret, iid2) <- r_f ([], [], fops, fss, registers, cp, iid)
  case ret of
    Right vmrt -> r_f (vmrt:dopsNew, Call args:aops, bops, ss, reg, cp, iid2)
    Left err -> return (ret, -1)
  where
    (nArgs, (this:VmRt_Function fops fss _:dopsNew)) = splitAt (fromIntegral args) dops
    registers = V.fromList$ this:nArgs

r_f {-0x46-} (dops, aops, CallProperty idx args maybeName:bops, ss, reg, cp, iid) = do
  po dops aops$ CallPropVoid idx args maybeName:bops
  
  maybeClassInfoIdx <- H.lookup this pfx_class_info_idx
  Abc.TraitsInfo _ _ _ (Abc.TraitMethod _ methodId) _ <- case maybeClassInfoIdx of
    Just (VmRtInternalInt classIdx) -> do
      Abc.ClassInfo _ traits <- get_class cp classIdx
      let matchingTraits = filter (\(Abc.TraitsInfo idx2 _ _ _ _) -> idx2 == idx) traits
      p$ "got class info of pfx_class_info_idx" ++ show matchingTraits
      case length matchingTraits of
        0 -> fail "CallProperty - couldn't find matchingTraits"
        1 -> return$ head matchingTraits
        otherwise -> fail "CallProperty - too many matching traits"
    otherwise -> do
      name <- case maybeName of
        Nothing -> do
          Abc.Multiname_QName nSInfoIdx stringIdx <- get_multiname cp idx
          liftM2 B.append (resolve_nsinfo cp nSInfoIdx) (get_string cp stringIdx)
        Just n -> return n
      proplist <- H.toList this
      fail$ "couldn't find " ++ BC.unpack name ++ " on " ++ show proplist

  MethodBody _ _ _ _ code _ registers <- get_methodBody cp methodId

  -- CallProperty
  let registers2 = registers // ((0, VmRt_Object this iidThis):zip [1..length nArgs] nArgs)
  (either, iid2) <- r_f ([], [], code, [last ss], registers2, cp, iid)
  case either of
    Left err -> return (either, iid2)
    Right ret -> r_f (ret:dopsNew, CallProperty idx args maybeName:aops, bops, ss, reg, cp, iid2)
  where
    (nArgs, (VmRt_Object this iidThis):dopsNew) = splitAt (fromIntegral args) dops

r_f {-0x47-} (dops, aops, ReturnVoid:bops, ss, reg, cp, iid) = do
  return (Right VmRt_Undefined, iid)

r_f {-0x48-} (a:dops, aops, ReturnValue:bops, ss, reg, cp, iid) = do
  return (Right a, iid)

-- TODO check for [ns] [name]
r_f {-0x4F-} (dops, aops, CallPropVoid idx args maybeName:bops, ss, reg, cp, iid) = do
  po dops aops$ CallPropVoid idx args maybeName:bops
  
  maybeClassInfoIdx <- H.lookup this pfx_class_info_idx
  Abc.TraitsInfo _ _ _ (Abc.TraitMethod _ methodId) _ <- case maybeClassInfoIdx of
    Just (VmRtInternalInt classIdx) -> do
      Abc.ClassInfo _ traits <- get_class cp classIdx
      let matchingTraits = filter (\(Abc.TraitsInfo idx2 _ _ _ _) -> idx2 == idx) traits
      p$ "got class info of pfx_class_info_idx" ++ show matchingTraits
      case length matchingTraits of
        0 -> fail "CallPropVoid - couldn't find matchingTraits"
        1 -> return$ head matchingTraits
        otherwise -> fail "CallPropVoid - too many matching traits"
    otherwise -> do
      name <- case maybeName of
        Nothing -> do
          Abc.Multiname_QName nSInfoIdx stringIdx <- get_multiname cp idx
          liftM2 B.append (resolve_nsinfo cp nSInfoIdx) (get_string cp stringIdx)
        Just n -> return n
      proplist <- H.toList this
      fail$ "couldn't find " ++ BC.unpack name ++ " on " ++ show proplist

  MethodBody _ _ _ _ code _ registers <- get_methodBody cp methodId

  -- CallPropVoid, only need the latest instance id counter
  let registers2 = registers // ((0, VmRt_Object this iidThis):zip [1..length nArgs] nArgs)
  (_, iid2) <- r_f ([], [], code, [last ss], registers2, cp, iid)

  r_f (dopsNew, CallPropVoid idx args maybeName:aops, bops, ss, reg, cp, iid2)
  where
    (nArgs, (VmRt_Object this iidThis):dopsNew) = splitAt (fromIntegral args) dops

r_f {-0x55-} (dops, aops, NewObject args:bops, ss, reg, cp, iid) = do
  po dops aops$ NewObject args:bops
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

r_f {-0x57-} (dops, aops, NewActivation:bops, ss, reg, cp, iid) = do
  po dops aops$ NewActivation:bops
  obj <- H.new
  r_f (VmRt_Activation obj:dops, NewActivation:aops, bops, ss, reg, cp, iid)

r_f {-0x58-} (dops, aops, NewClass idx:bops, ss, reg, cp, iid) = do
  p$ "########## NewClass " ++ show idx
  Abc.ClassInfo msi traits <- get_class cp idx

  MethodBody _ _ _ _ code _ registers <- get_methodBody cp msi
  --p$ "\tops: " ++ show code
  --p$ "\tmaxReg: " ++ show maxReg

  vmrto@(VmRt_Object klass iid2) <- new_object iid
  -- store this class's identity in it
  insert klass pfx_class_info_idx (VmRtInternalInt idx)

  -- NewClass, only need the latest instance id counter
  (_, iid3) <- r_f ([], [], code, [(global, globalid)], registers // [(0, vmrto)], cp, iid2)

  insert global (Ext$ BC.pack "Test") vmrto
  p$ "########## " ++ show idx

  r_f (dops, NewClass idx:aops, bops, ss, reg, cp, iid3)
  where
    (global, globalid) = last ss

r_f {-0x5D-} (dops, aops, FindPropStrict idx maybeName:bops, ss, reg, cp, iid) = do
  po dops aops$ FindPropStrict idx maybeName:bops
  
  name <- case maybeName of
    Nothing -> do
      Abc.Multiname_QName nSInfoIdx stringIdx <- get_multiname cp idx
      liftM2 B.append (resolve_nsinfo cp nSInfoIdx) (get_string cp stringIdx)
    Just n -> return n
  
  vmrt <- find_property name idx cp ss -- TODO this need error checking
  r_f (vmrt:dops, FindPropStrict idx maybeName:aops, bops, ss, reg, cp, iid)

r_f {-0x5E-} (dops, aops, FindProperty idx maybeName:bops, ss, reg, cp, iid) = do
  po dops aops$ FindProperty idx maybeName:bops
  name <- case maybeName of
    Nothing -> do
      Abc.Multiname_QName nSInfoIdx stringIdx <- get_multiname cp idx
      liftM2 B.append (resolve_nsinfo cp nSInfoIdx) (get_string cp stringIdx)
    Just n -> return n
  vmrt <- find_property name idx cp ss -- TODO this need error checking
  r_f (vmrt:dops, FindProperty idx maybeName:aops, bops, ss, reg, cp, iid)

r_f {-0x61-} (value:dops, aops, SetProperty idx Nothing:bops, ss, reg, cp, iid) = do
  multiname <- get_multiname cp idx
  (VmRt_String prop, (VmRt_Object this iidThis):dopsNew) <- case multiname of
    Abc.Multiname_QName _ stringIdx -> do
      str <- get_string cp stringIdx
      return (VmRt_String str, dops)
    Abc.Multiname_Multiname stringIdx _ -> do
      str <- get_string cp stringIdx
      return (VmRt_String str, dops)
    otherwise -> return (head dops, tail dops)
  insert this (Ext prop) value
  r_f (dopsNew, SetProperty idx Nothing:aops, bops, ss, reg, cp, iid)

r_f {-0x61-} (value:VmRt_Object this _:dops, aops, SetProperty idx (Just prop):bops, ss, reg, cp, iid) = do
  {-# SCC "SetProperty_idx_Just" #-} insert this (Ext prop) value
  r_f (dops, SetProperty idx (Just prop):aops, bops, ss, reg, cp, iid)

r_f {-0x62-} (dops, aops, GetLocal u30:bops, ss, reg, cp, iid) = do
  r_f (reg ! fromIntegral u30:dops, GetLocal u30:aops, bops, ss, reg, cp, iid)

r_f {-0x63-} (new:dops, aops, SetLocal u30:bops, ss, reg, cp, iid) = do
  po (new:dops) aops$ SetLocal u30:bops
  r_f (dops, SetLocal u30:aops, bops, ss, reg // [(fromIntegral u30, new)], cp, iid)

r_f {-0x64-} (dops, aops, GetGlobalScope:bops, ss, reg, cp, iid) = do
  po dops aops$ GetGlobalScope:bops
  r_f (global:dops, GetGlobalScope:aops, bops, ss, reg, cp, iid)
  where
    (v, iidv) = last ss
    global = VmRt_Object v iidv

r_f {-0x65-} (dops, aops, GetScopeObject idx:bops, ss, reg, cp, iid) = do
  po dops aops$ GetScopeObject idx:bops
  r_f (scopedObject:dops, GetScopeObject idx:aops, bops, ss, reg, cp, iid)
  where
    (obj, iidObj) = ss !! fromIntegral idx
    scopedObject = VmRt_Object obj iidObj

r_f {-0x66-} (dops, aops, GetProperty idx Nothing:bops, ss, reg, cp, iid) = do
  po dops aops$ GetProperty idx Nothing:bops
  multiname <- get_multiname cp idx
  (prop, value:dopsNew) <- case multiname of
    Abc.Multiname_QName _ stringIdx -> do
      str <- get_string cp stringIdx
      return (VmRt_String str, dops)
    Abc.Multiname_Multiname stringIdx _ -> do
      str <- get_string cp stringIdx
      return (VmRt_String str, dops)
    otherwise -> return (head dops, tail dops)

  d <- case value of
    VmRt_Undefined -> return VmRt_Undefined
    VmRt_Null -> return VmRt_Undefined
    VmRt_Boolean bool -> return VmRt_Undefined
    VmRt_Int v -> return VmRt_Undefined
    VmRt_Uint v -> return VmRt_Undefined
    VmRt_Number v -> return VmRt_Undefined
    VmRt_String v -> return VmRt_Undefined
    VmRt_Array v _ -> case prop of
      VmRt_String name -> case name of
        ic_length -> return . VmRt_Int . fromIntegral$ length v
        otherwise -> fail$ "GetProperty - VmRt_Array - can't get property " ++ BC.unpack name
      VmRt_Int i -> return$ v !! fromIntegral i
    VmRt_Object this _ -> case prop of
      VmRt_String name -> do
        maybeProp <- H.lookup this$ Ext name
        case maybeProp of
          Just prop -> return prop
          Nothing -> return VmRt_Undefined
      otherwise -> fail "GetProperty - VmRt_Object - only strings"
  
  r_f (d:dopsNew, GetProperty idx Nothing:aops, bops, ss, reg, cp, iid)

r_f {-0x66-} (value:dops, aops, GetProperty idx (Just prop):bops, ss, reg, cp, iid) = do
  po dops aops$ GetProperty idx (Just prop):bops

  d <- case value of
    VmRt_Undefined -> return VmRt_Undefined
    VmRt_Null -> return VmRt_Undefined
    VmRt_Boolean bool -> return VmRt_Undefined
    VmRt_Int v -> return VmRt_Undefined
    VmRt_Uint v -> return VmRt_Undefined
    VmRt_Number v -> return VmRt_Undefined
    VmRt_String v -> return VmRt_Undefined
    VmRt_Array v _ -> case prop of
      ic_length -> return . VmRt_Int . fromIntegral$ length v
      otherwise -> fail$ "GetProperty - VmRt_Array - can't get property " ++ BC.unpack prop
    VmRt_Object this _ -> do
      maybeProp <- H.lookup this$ Ext prop
      case maybeProp of
        Just prop -> return prop
        Nothing -> return VmRt_Undefined

  r_f (d:dops, GetProperty idx (Just prop):aops, bops, ss, reg, cp, iid)

r_f {-0x68-} (value:VmRt_Object this iidThis:dops, aops, InitProperty idx maybeName:bops, ss, reg, cp, iid) = do
  po (value:VmRt_Object this iidThis:dops) aops (InitProperty idx maybeName:bops)
  name <- case maybeName of
    Nothing -> do
      Abc.Multiname_QName nSInfoIdx stringIdx <- get_multiname cp idx
      liftM2 B.append (resolve_nsinfo cp nSInfoIdx) (get_string cp stringIdx)
    Just n -> return n
  p$ "InitProperty [" ++ show idx ++ " " ++ BC.unpack name ++ "]"
  maybeProp <- H.lookup this (Ext name)
  case maybeProp of
    Nothing -> do
      insert this (Ext name) value
      return ()
    otherwise -> fail$ "init_property - property [" ++ BC.unpack name ++ "] exists"
  r_f (dops, InitProperty idx maybeName:aops, bops, ss, reg, cp, iid)

r_f {-0x6C-} (VmRt_Object obj _:dops, aops, GetSlot slotIdx ti:bops, ss, reg, cp, iid) = do
  po (VmRt_Object obj 0:dops) aops$ GetSlot slotIdx ti:bops
  let TraitsInfo (Just p) _ _ _ _ = ti
  Just slot <- lookup obj (Ext p)
  r_f (slot:dops, GetSlot slotIdx ti:aops, bops, ss, reg, cp, iid)

r_f {-0x6D-} (v:VmRt_Object obj _:dops, aops, SetSlot slotIdx ti:bops, ss, reg, cp, iid) = do
  po (v:VmRt_Object obj 0:dops) aops$ SetSlot slotIdx ti:bops
  let TraitsInfo (Just p) _ _ _ _ = ti
  insert obj (Ext p) v
  r_f (dops, SetSlot slotIdx ti:aops, bops, ss, reg, cp, iid)

--r_f {-0x6E-} (VmRt_Object obj _:dops, aops, GetGlobalSlot u30:bops, ss, reg, cp, iid) = do
-- po (VmRt_Object obj 0:dops) aops$ GetGlobalSlot u30:bops
-- Just slot <- lookup obj $ pfx_slot_idx u30
-- r_f (slot:dops, GetGlobalSlot u30:aops, bops, ss, reg, cp, iid)

--r_f {-0x6F-} (value:VmRt_Object obj _:dops, aops, SetGlobalSlot u30:bops, ss, reg, cp, iid) = do
-- po (value:VmRt_Object obj 0:dops) aops$ SetGlobalSlot u30:bops
-- insert obj (pfx_slot_idx u30) value
-- r_f (dops, SetGlobalSlot u30:aops, bops, ss, reg, cp, iid)

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

r_f {-0x80-} (obj:dops, aops, Coerce idx (Just t):bops, ss, reg, cp, iid) = do
  po (obj:dops) aops$ Coerce idx (Just t):bops
  case t of
    ic_function -> case obj of
      f@(VmRt_Function _ _ _) -> r_f (f:dops, Coerce idx (Just t):aops, bops, ss, reg, cp, iid)
    otherwise -> fail$ "unsupported Coerce " ++ BC.unpack t

r_f {-0x90-} (a:dops, aops, Negate:bops, ss, reg, cp, iid) = do
  vmrt <- case a of
    VmRt_Int i -> return$ VmRt_Int$ negate i
    VmRt_Uint i -> return$ VmRt_Uint$ negate i
    VmRt_Number i -> return$ VmRt_Number$ negate i
    otherwise -> fail$ "can't negate " ++ show a
  r_f (vmrt:dops, Negate:aops, bops, ss, reg, cp, iid)

r_f {-0x91-} (a:dops, aops, Increment:bops, ss, reg, cp, iid) = do
  r_f (a + 1:dops, Increment:aops, bops, ss, reg, cp, iid)

r_f {-0x93-} (a:dops, aops, Decrement:bops, ss, reg, cp, iid) = do
  r_f (a - 1:dops, Decrement:aops, bops, ss, reg, cp, iid)

r_f {-0xA0-} (a:b:dops, aops, Add:bops, ss, reg, cp, iid) = do
  r_f (b + a:dops, Add:aops, bops, ss, reg, cp, iid)

r_f {-0xA1-} (a:b:dops, aops, Subtract:bops, ss, reg, cp, iid) = do
  r_f (b - a:dops, Subtract:aops, bops, ss, reg, cp, iid)

r_f {-0xA2-} (a:b:dops, aops, Multiply:bops, ss, reg, cp, iid) = do
  r_f (b * a:dops, Multiply:aops, bops, ss, reg, cp, iid)

r_f {-0xA3-} (a:b:dops, aops, Divide:bops, ss, reg, cp, iid) = do
  r_f (b / a:dops, Divide:aops, bops, ss, reg, cp, iid)

r_f {-0xAB-} (a:b:dops, aops, Equals:bops, ss, reg, cp, iid) = do
  po (a:b:dops) aops$ Equals:bops
  r_f (VmRt_Boolean (b == a):dops, Equals:aops, bops, ss, reg, cp, iid)

r_f {-0xAC-} (a:b:dops, aops, StrictEquals:bops, ss, reg, cp, iid) = do
  po (a:b:dops) aops$ StrictEquals:bops
  r_f (VmRt_Boolean (b == a):dops, StrictEquals:aops, bops, ss, reg, cp, iid)

r_f {-0xC0-} (a:dops, aops, IncrementInt:bops, ss, reg, cp, iid) = do
  r_f (a + 1:dops, IncrementInt:aops, bops, ss, reg, cp, iid)

r_f {-0xC1-} (a:dops, aops, DecrementInt:bops, ss, reg, cp, iid) = do
  r_f (a - 1:dops, DecrementInt:aops, bops, ss, reg, cp, iid)

r_f {-0xC2-} (dops, aops, IncLocalInt regIdx:bops, ss, reg, cp, iid) = do
  r_f (dops, IncLocalInt regIdx:aops, bops, ss, reg2, cp, iid)
  where
    reg2 = reg // [(iidx, (reg ! iidx) + 1)]
    iidx = fromIntegral regIdx

r_f {-0xC3-} (dops, aops, DecLocalInt regIdx:bops, ss, reg, cp, iid) = do
  r_f (dops, DecLocalInt regIdx:aops, bops, ss, reg2, cp, iid)
  where
    reg2 = reg // [(iidx, (reg ! iidx) - 1)]
    iidx = fromIntegral regIdx

r_f {-0xD0-} (dops, aops, GetLocal0:bops, ss, reg, cp, iid) = do
  po dops aops$ GetLocal0:bops
  r_f (reg ! 0:dops, GetLocal0:aops, bops, ss, reg, cp, iid)

r_f {-0xD1-} (dops, aops, GetLocal1:bops, ss, reg, cp, iid) = do
  po dops aops$ GetLocal1:bops
  r_f (reg ! 1:dops, GetLocal1:aops, bops, ss, reg, cp, iid)

r_f {-0xD2-} (dops, aops, GetLocal2:bops, ss, reg, cp, iid) = do
  po dops aops$ GetLocal2:bops
  r_f (reg ! 2:dops, GetLocal2:aops, bops, ss, reg, cp, iid)

r_f {-0xD3-} (dops, aops, GetLocal3:bops, ss, reg, cp, iid) = do
  po dops aops$ GetLocal3:bops
  r_f (reg ! 3:dops, GetLocal3:aops, bops, ss, reg, cp, iid)

r_f {-0xD4-} (new:dops, aops, SetLocal0:bops, ss, reg, cp, iid) = do
  po (new:dops) aops$ SetLocal0:bops
  r_f (dops, SetLocal0:aops, bops, ss, reg // [(0, new)], cp, iid)

r_f {-0xD5-} (new:dops, aops, SetLocal1:bops, ss, reg, cp, iid) = do
  po (new:dops) aops$ SetLocal1:bops
  r_f (dops, SetLocal1:aops, bops, ss, reg // [(1, new)], cp, iid)

r_f {-0xD6-} (new:dops, aops, SetLocal2:bops, ss, reg, cp, iid) = do
  po (new:dops) aops$ SetLocal2:bops
  r_f (dops, SetLocal2:aops, bops, ss, reg // [(2, new)], cp, iid)

r_f {-0xD7-} (new:dops, aops, SetLocal3:bops, ss, reg, cp, iid) = do
  po (new:dops) aops$ SetLocal3:bops
  r_f (dops, SetLocal3:aops, bops, ss, reg // [(3, new)], cp, iid)

r_f {-0xD7-} (dops, aops, op:bops, ss, reg, cp, iid) = do
  fail$ "didn't match opcode " ++ show op

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

-- TODO dynamic multinames (pattern match Ops)

-- TODO resolve against
--   * declared traits (on the method body, instance info?, class info?)
--   * dynamic properties
--   * prototype chain
--   * script traits (global object (for classes))

find_property :: B.ByteString -> Abc.MultinameIdx -> ConstantPool -> ScopeStack -> AVM3 VmRt
find_property name idx cp ss = do
  attempts <- liftM2 (<|>) at1 at2
  case attempts of
    Just obj -> return obj
    Nothing -> fail "find_property - couldn't find_property"
  where
    at1 = scope_stack (Ext name) ss
    at2 = traits_ref cp idx ss

    scope_stack :: VmRtP -> ScopeStack -> AVM3 (Maybe VmRt)
    scope_stack key [] = return Nothing
    scope_stack key@(Ext _) ((top, iid):stack) = do
      maybeValue <- H.lookup top key
      case maybeValue of
        Nothing -> scope_stack key stack
        Just value -> returnJ$ VmRt_Object top iid

    traits_ref :: ConstantPool -> Abc.MultinameIdx -> ScopeStack -> AVM3 (Maybe VmRt)
    traits_ref cp idx [] = return Nothing
    traits_ref cp idx ((top, iid):stack) = do
      maybeClassInfoIdx <- H.lookup top pfx_class_info_idx
      case maybeClassInfoIdx of
        Just (VmRtInternalInt classIdx) -> do
          Abc.ClassInfo _ traits <- get_class cp classIdx
          let matchingTraits = filter (\(Abc.TraitsInfo idx2 _ _ _ _) -> idx2 == idx) traits
          case length matchingTraits of
            0 -> return Nothing
            1 -> returnJ$ VmRt_Object top iid
            otherwise -> fail "traits_ref - too many matching traits"
        otherwise -> traits_ref cp idx stack
