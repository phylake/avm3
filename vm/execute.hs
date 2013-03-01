{-# LANGUAGE ScopedTypeVariables #-}
module Vm.Execute where

import           Abc.DeepSeq
import           Abc.Def
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
import qualified Control.Monad.State as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import qualified Data.HashTable.IO as H

p :: String -> AVM3 ()
--p = liftIO.putStrLn
p _ = return ()
{-# INLINE p #-}

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
insert h k v = H.insert h k v
{-# INLINE insert #-}

lookup :: VmObject -> VmRtP -> AVM3 (Maybe VmRt)
lookup h k = H.lookup h k
{-# INLINE lookup #-}

pfx_class_info_idx :: VmRtP
pfx_class_info_idx = ClassIdx$ BC.pack$ avm_prefix ++ "class_info_idx"

{-new_object :: AVM3 (VmObject, InstanceId)
new_object = do
  iid <- next_iid
  obj <- H.new
  return (obj, iid)-}

test_file = do
  (abc :: Abc) <- E.run_ (EB.enumFile "abc/Test.abc" E.$$ parseAbc)
  --writeFile "abc/Test.abc.json"$ encode$ abcToJson abc
  (either::Either AVM3Exception VmRt) <- abc `deepseq` execute_abc abc
  case either of
    Left err -> Prelude.putStrLn$ "EXCEPTION\n" ++ (unlines$ map ("\t"++)$ lines err)
    otherwise -> return ()

execute_abc :: Abc -> AVM3 (Either AVM3Exception VmRt)
execute_abc abc = do
  t0 <- getCurrentTime
  cp <- build_cp abc
  ScriptInfo _ ((TraitsInfo _ _ _ (TT_Class (TraitClass _ idx)) _):[]) <- get_script cp 0
  p$ "-------------------------------------------"
  (global, globalid) <- build_global_scope 0
  let ops = [NewClass idx,ReturnVoid]
  (registers :: Registers) <- H.new
  H.insert registers 0 $ VmRt_Object global $ globalid+1
  vmrt <- r_f ([([], [], ops, [(global, globalid)], registers)], cp, globalid+2)
  p$ "-------------------------------------------"
  t1 <- getCurrentTime
  putStrLn$ show$ diffUTCTime t1 t0
  return vmrt

show_ops :: String -> Ops -> AVM3 ()
show_ops header ops = do
  case length ops of
    0 -> do
      p$ header ++ " []"
    otherwise -> do
      p$ header
      p$ unlines$ map (\s -> "\t" ++ show s) ops

build_global_scope :: InstanceId -> AVM3 (VmObject, InstanceId)
build_global_scope iid = do
  (int :: VmObject) <- H.new
  insert int (Ext$ BC.pack "MAX_VALUE") (VmRt_Int 2147483647)

  (global :: VmObject) <- H.new
  insert global (Ext$ BC.pack "int") (VmRt_Object int next_iid)

  return (global, next_iid)
  where
    next_iid = iid + 1

convert_offset :: [VmRtOp] -> S24 -> Int
--convert_offset s24 (op:[]) = toBytes op
convert_offset ((O op):ops) s24
  | s24 > 0 = 1 + (convert_offset ops$ s24 - (fromIntegral$ toBytes op))
  | otherwise = 1

{-push_undefined :: AVM3 ()
push_undefined = pushd VmRt_Undefined-}

{-
  prone to off by one errors.
  this must be called after the sp is modified in r_f
-}
jump :: S24 -> (A_Ops, B_Ops) -> (A_Ops, B_Ops)
jump = undefined
{-jump s24 (aboveSp, belowSp) = if s24 > 0
  then convert_offset (drop 1 belowSp) s24 - 1
  else negate$ convert_offset (drop 1$ reverse aboveSp) (abs s24) - 1-}

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

{-
 dops (top:bottoms)
 aops (bottom:tops)
 bops (top:bottoms)

 D1, D2 dops
 O2, O1 aops
 O3, O4 bops
-}

        {-p$ "run_function"
        p$ (unlines$ map (\s -> "\t" ++ show s) aboveSp)
          ++ "\t--------------------" ++ show sp ++ "\n"
          ++ (unlines$ map (\s -> "\t" ++ show s) belowSp)-}
r_f :: Execution -> AVM3 (Either AVM3Exception VmRt)
r_f {-0x08-} (((dops, aops, Kill regIdx:bops, ss, reg):fs), cp, iid) = do
  H.insert reg (fromIntegral regIdx) VmRt_Undefined
  r_f ((dops,Kill regIdx:aops,bops,ss,reg):fs, cp, iid)

r_f {-0x09-} (((dops, aops, Label:bops, ss, reg):fs), cp, iid) =
  r_f ((dops,Label:aops,bops,ss,reg):fs, cp, iid)

r_f {-0x10-} (((dops, aops, Jump s24:bops, ss, reg):fs), cp, iid) =
  r_f ((dops,newAops,newBops,ss,reg):fs, cp, iid)
  where
    (newAops, newBops) = jump s24 (Jump s24:aops, bops)

r_f {-0x11-} (((a:dops, aops, IfTrue s24:bops, ss, reg):fs), cp, iid) =
  if to_boolean a == True
    then r_f ((dops,newAops,newBops,ss,reg):fs, cp, iid)
    else r_f ((dops,IfTrue s24:aops,bops,ss,reg):fs, cp, iid)
  where
    (newAops, newBops) = jump s24 (IfTrue s24:aops, bops)

r_f {-0x12-} (((a:dops, aops, IfFalse s24:bops, ss, reg):fs), cp, iid) =
  if to_boolean a == False
    then r_f ((dops,newAops,newBops,ss,reg):fs, cp, iid)
    else r_f ((dops,IfFalse s24:aops,bops,ss,reg):fs, cp, iid)
  where
    (newAops, newBops) = jump s24 (IfFalse s24:aops, bops)

r_f {-0x13-} (((a:b:dops, aops, IfEqual s24:bops, ss, reg):fs), cp, iid) =
  if b == a
    then r_f ((dops,newAops,newBops,ss,reg):fs, cp, iid)
    else r_f ((dops,IfEqual s24:aops,bops,ss,reg):fs, cp, iid)
  where
    (newAops, newBops) = jump s24 (IfEqual s24:aops, bops)

r_f {-0x14-} (((a:b:dops, aops, IfNotEqual s24:bops, ss, reg):fs), cp, iid) =
  if b /= a
    then r_f ((dops,newAops,newBops,ss,reg):fs, cp, iid)
    else r_f ((dops,IfNotEqual s24:aops,bops,ss,reg):fs, cp, iid)
  where
    (newAops, newBops) = jump s24 (IfNotEqual s24:aops, bops)

r_f {-0x15-} (((a:b:dops, aops, IfLessThan s24:bops, ss, reg):fs), cp, iid) =
  if b < a
    then r_f ((dops,newAops,newBops,ss,reg):fs, cp, iid)
    else r_f ((dops,IfLessThan s24:aops,bops,ss,reg):fs, cp, iid)
  where
    (newAops, newBops) = jump s24 (IfLessThan s24:aops, bops)

r_f {-0x16-} (((a:b:dops, aops, IfLessEqual s24:bops, ss, reg):fs), cp, iid) =
  if b <= a
    then r_f ((dops,newAops,newBops,ss,reg):fs, cp, iid)
    else r_f ((dops,IfLessEqual s24:aops,bops,ss,reg):fs, cp, iid)
  where
    (newAops, newBops) = jump s24 (IfLessEqual s24:aops, bops)

r_f {-0x17-} (((a:b:dops, aops, IfGreaterThan s24:bops, ss, reg):fs), cp, iid) =
  if b > a
    then r_f ((dops,newAops,newBops,ss,reg):fs, cp, iid)
    else r_f ((dops,IfGreaterThan s24:aops,bops,ss,reg):fs, cp, iid)
  where
    (newAops, newBops) = jump s24 (IfGreaterThan s24:aops, bops)

r_f {-0x18-} (((a:b:dops, aops, IfGreaterEqual s24:bops, ss, reg):fs), cp, iid) =
  if b >= a
    then r_f ((dops,newAops,newBops,ss,reg):fs, cp, iid)
    else r_f ((dops,IfGreaterEqual s24:aops,bops,ss,reg):fs, cp, iid)
  where
    (newAops, newBops) = jump s24 (IfGreaterEqual s24:aops, bops)

r_f {-0x19-} (((a:b:dops, aops, IfStrictEqual s24:bops, ss, reg):fs), cp, iid) =
  if a == b -- TODO class StrictEq ===
    then r_f ((dops,newAops,newBops,ss,reg):fs, cp, iid)
    else r_f ((dops,IfStrictEqual s24:aops,bops,ss,reg):fs, cp, iid)
  where
    (newAops, newBops) = jump s24 (IfStrictEqual s24:aops, bops)

r_f {-0x1A-} (((a:b:dops, aops, IfStrictNotEqual s24:bops, ss, reg):fs), cp, iid) =
  if a /= b -- TODO class StrictEq /==
    then r_f ((dops,newAops,newBops,ss,reg):fs, cp, iid)
    else r_f ((dops,IfStrictNotEqual s24:aops,bops,ss,reg):fs, cp, iid)
  where
    (newAops, newBops) = jump s24 (IfStrictNotEqual s24:aops, bops)

r_f {-0x1D-} (((dops, aops, PopScope:bops, (_:ss), reg):fs), cp, iid) =
  r_f ((dops,PopScope:aops,bops,ss,reg):fs, cp, iid)

r_f {-0x24-} (((dops, aops, PushByte u8:bops, ss, reg):fs), cp, iid) =
  r_f ((VmRt_Int (fromIntegral u8):dops,PushByte u8:aops,bops,ss,reg):fs, cp, iid)

r_f {-0x25-} (((dops, aops, PushShort u30:bops, ss, reg):fs), cp, iid) =
  r_f ((VmRt_Uint u30:dops,PushShort u30:aops,bops,ss,reg):fs, cp, iid)

r_f {-0x26-} (((dops, aops, PushTrue:bops, ss, reg):fs), cp, iid) =
  r_f ((VmRt_Boolean True:dops,PushTrue:aops,bops,ss,reg):fs, cp, iid)

r_f {-0x27-} (((dops, aops, PushFalse:bops, ss, reg):fs), cp, iid) =
  r_f ((VmRt_Boolean False:dops,PushFalse:aops,bops,ss,reg):fs, cp, iid)

r_f {-0x28-} (((dops, aops, PushNaN:bops, ss, reg):fs), cp, iid) =
  r_f ((VmRt_Number nan:dops,PushNaN:aops,bops,ss,reg):fs, cp, iid)

r_f {-0x2A-} (((a:dops, aops, Dup:bops, ss, reg):fs), cp, iid) =
  r_f ((a:a:dops,Dup:aops,bops,ss,reg):fs, cp, iid)

r_f {-0x2B-} (((a:b:dops, aops, Swap:bops, ss, reg):fs), cp, iid) =
  r_f ((b:a:dops,Swap:aops,bops,ss,reg):fs, cp, iid)

r_f {-0x2C-} (((dops, aops, PushString idx:bops, ss, reg):fs), cp, iid) = do
  a <- liftM VmRt_String$ get_string cp idx
  r_f ((a:dops,PushString idx:aops,bops,ss,reg):fs, cp, iid)

r_f {-0x2D-} (((dops, aops, PushInt idx:bops, ss, reg):fs), cp, iid) = do
  a <- liftM VmRt_Int$ get_int cp idx
  r_f ((a:dops,PushInt idx:aops,bops,ss,reg):fs, cp, iid)

r_f {-0x2E-} (((dops, aops, PushUInt idx:bops, ss, reg):fs), cp, iid) = do
  a <- liftM VmRt_Uint$ get_uint cp idx
  r_f ((a:dops,PushUInt idx:aops,bops,ss,reg):fs, cp, iid)

r_f {-0x2F-} (((dops, aops, PushDouble idx:bops, ss, reg):fs), cp, iid) = do
  a <- liftM VmRt_Number$ get_double cp idx
  r_f ((a:dops,PushDouble idx:aops,bops,ss,reg):fs, cp, iid)

r_f {-0x30-} ((((VmRt_Object v i):dops, aops, PushScope:bops, ss, reg):fs), cp, iid) =
  r_f ((dops,PushScope:aops,bops,(v,i):ss,reg):fs, cp, iid)

r_f {-0x47-} (((dops, aops, ReturnVoid:bops, ss, reg):fs), cp, iid) = returnR VmRt_Undefined

r_f {-0x48-} (((a:dops, aops, ReturnValue:bops, ss, reg):fs), cp, iid) = returnR a

  {-CallPropVoid idx args -> do -- TODO check for [ns] [name] -- 0x4F
            nArgs <- replicateM (fromIntegral args) pop
            D (VmRt_Object this iid) <- pop

            p$ "CallPropVoid"
            list <- H.toList this
            p$ show list

            maybeClassInfoIdx <- H.lookup this pfx_class_info_idx
            TraitsInfo _ _ _ (TT_Method (TraitMethod _ methodId)) _ <- case maybeClassInfoIdx of
              Just (VmRtInternalInt classIdx) -> do
                ClassInfo _ traits <- get_class classIdx
                let matchingTraits = filter (\(TraitsInfo idx2 _ _ _ _) -> idx2 == idx) traits
                p$ "got class info of pfx_class_info_idx" ++ show matchingTraits
                case length matchingTraits of
                  0 -> fail "CallPropVoid - couldn't find matchingTraits"
                  1 -> return$ head matchingTraits
                  otherwise -> fail "CallPropVoid - too many matching traits"
              otherwise -> do
                Multiname_QName nSInfoIdx stringIdx <- get_multiname idx
                name <- liftM2 B.append (resolve_nsinfo cp nSInfoIdx) (get_string cp stringIdx)
                proplist <- H.toList this
                fail$ "couldn't find " ++ BC.unpack name ++ " on " ++ show proplist

            MethodBody _ _ _ _ _ code _ _ <- get_methodBody methodId
            p$ show code
            (registers :: Registers) <- H.new
            H.insert registers 0 $ VmRt_Object this iid
            forM_ (zip [1..length nArgs] nArgs) (\(i,D arg) -> H.insert registers i arg)
            push_activation (0, map O code, [last ss], registers)
            r_f
            pop_activation

            return ()
  NewObject args -> do -- 0x55
            (obj, iid) <- new_object
            nArgs <- replicateM (fromIntegral args) (liftM2 (,) pop pop)
            forM_ nArgs$ \(D v,D (VmRt_String k)) -> insert obj (Ext k) v
            pushd$ VmRt_Object obj iid
  NewArray args -> do -- 0x56
            nArgs <- replicateM (fromIntegral args) pop
            iid <- next_iid
            pushd$ VmRt_Array (map (\(D a) -> a)$ reverse nArgs) iid
  NewClass idx -> do -- 0x58
            p$ "########## NewClass " ++ show idx
            ClassInfo msi traits <- get_class idx

            MethodBody _ _ _ _ _ code _ _ <- get_methodBody msi
            --p$ "\tops: " ++ show code

            (klass, iid) <- new_object
            -- store this class's identity in it
            insert klass pfx_class_info_idx (VmRtInternalInt idx)

            let (global, globalid) = last ss
            (registers :: Registers) <- H.new
            H.insert registers 0 $ VmRt_Object klass iid
            push_activation (0, map O code, [(global, globalid)], registers)
            r_f
            pop_activation

            insert global (Ext$ BC.pack "Test")$ VmRt_Object klass iid
            p$ "########## " ++ show idx
  FindPropStrict idx -> find_property idx ss >>= pushd -- TODO this need error checking -- 0x5D
  FindProperty idx -> find_property idx ss >>= pushd -- TODO this need error checking -- 0x5E
  GetLex idx -> fail "GetLex should have been replaced" -- 0x60
  SetProperty idx -> do -- 0x61
            D value <- pop

            multiname <- get_multiname idx
            (VmRt_String prop) <- case multiname of
              Multiname_QName _ stringIdx -> liftM VmRt_String$ get_string stringIdx
              Multiname_Multiname stringIdx _ -> liftM VmRt_String$ get_string stringIdx
              otherwise -> do (D d) <- pop; return d

            D (VmRt_Object this _) <- pop
            insert this (Ext prop) value
  GetLocal u30 -> do -- 0x62
            maybeR <- H.lookup reg (fromIntegral u30)
            case maybeR of
              Nothing -> fail$ "GetLocal" ++ show u30 ++ " - register doesn't exist"
              Just r -> pushd r
  SetLocal u30 -> do -- 0x63
            D new <- pop
            H.insert reg (fromIntegral u30) new
  GetScopeObject idx -> do -- 0x65
            let (obj, iid) = ss !! fromIntegral idx 
            pushd$ VmRt_Object obj iid
  GetProperty idx -> do -- 0x66
            multiname <- get_multiname idx
            prop <- case multiname of
              Multiname_QName _ stringIdx -> liftM VmRt_String$ get_string stringIdx
              Multiname_Multiname stringIdx _ -> liftM VmRt_String$ get_string stringIdx
              otherwise -> do (D d) <- pop; return d

            D vmrt <- pop

            case vmrt of
              VmRt_Undefined -> push_undefined
              VmRt_Null -> push_undefined
              VmRt_Boolean bool -> push_undefined
              VmRt_Int v -> push_undefined
              VmRt_Uint v -> push_undefined
              VmRt_Number v -> push_undefined
              VmRt_String v -> push_undefined
              VmRt_Array v _ -> case prop of
                VmRt_String name -> case name of
                  p_length -> pushd . VmRt_Int . fromIntegral$ length v
                  otherwise -> fail$ "GetProperty - VmRt_Array - can't get property " ++ BC.unpack name
                VmRt_Int i -> pushd$ v !! fromIntegral i
              VmRt_Object this _ -> case prop of
                VmRt_String name -> do
                  maybeProp <- H.lookup this$ Ext name
                  case maybeProp of
                    Just prop -> pushd prop
                    Nothing -> push_undefined
                otherwise -> fail "GetProperty - VmRt_Object - only strings"
              --VmRt_Closure f
  InitProperty idx -> do -- 0x68
            D vmrt <- pop
            D (VmRt_Object this iid) <- pop
            init_property this idx vmrt
  ConvertString -> do -- 0x70
            D v <- pop
            pushd$ convert_string v
  ConvertInt -> do -- 0x73
            D v <- pop
            pushd$ convert_int v
  ConvertUInt -> do -- 0x74
            D v <- pop
            pushd$ convert_uint v
  ConvertDouble -> do -- 0x75
            D v <- pop
            pushd$ convert_double v
  ConvertBoolean -> do -- 0x76
            D v <- pop
            pushd$ convert_boolean v
  ConvertObject -> ML.fail "NI: ConvertObject" -- 0x77
  Negate -> do -- 0x90
            D a <- pop
            case a of
              VmRt_Int i -> pushd$ VmRt_Int$ negate i
              VmRt_Uint i -> pushd$ VmRt_Uint$ negate i
              VmRt_Number i -> pushd$ VmRt_Number$ negate i
              otherwise -> fail$ "can't negate " ++ show a
  Increment -> do -- 0x91
            D a <- pop
            pushd$ a + 1
  Decrement -> do -- 0x93
            D a <- pop
            pushd$ a - 1
  Add -> do -- 0xA0
            D a <- pop
            D b <- pop
            pushd$ b + a
  Subtract -> do -- 0xA1
            D a <- pop
            D b <- pop
            pushd$ b - a
  Multiply -> do -- 0xA2
            D a <- pop
            D b <- pop
            pushd$ b * a
  Divide -> do -- 0xA3
            D a <- pop
            D b <- pop
            pushd$ b / a
  IncrementInt -> do -- 0xC0
            D a <- pop
            pushd$ a+1
  DecrementInt -> do -- 0xC1
            D a <- pop
            pushd$ a-1
  IncLocalInt regIdx -> do -- 0xC2
            maybeReg <- H.lookup reg (fromIntegral regIdx)
            case maybeReg of
              Nothing -> fail$ "register " ++ show regIdx ++ " didn't exist"
              Just r -> H.insert reg (fromIntegral regIdx) (r + 1)
  DecLocalInt regIdx -> do -- 0xC3
            maybeReg <- H.lookup reg (fromIntegral regIdx)
            case maybeReg of
              Nothing -> fail$ "register " ++ show regIdx ++ " didn't exist"
              Just r -> H.insert reg (fromIntegral regIdx) (r - 1)
  GetLocal0 -> do -- 0xD0
            maybeR <- H.lookup reg 0
            case maybeR of
              Nothing -> fail "GetLocal0 - register doesn't exist"
              Just r -> pushd r
  GetLocal1 -> do -- 0xD1
            maybeR <- H.lookup reg 1
            case maybeR of
              Nothing -> fail "GetLocal1 - register doesn't exist"
              Just r -> pushd r
  GetLocal2 -> do -- 0xD2
            maybeR <- H.lookup reg 2
            case maybeR of
              Nothing -> fail "GetLocal2 - register doesn't exist"
              Just r -> pushd r
  GetLocal3 -> do -- 0xD3
            maybeR <- H.lookup reg 3
            case maybeR of
              Nothing -> fail "GetLocal3 - register doesn't exist"
              Just r -> pushd r
  SetLocal0 -> do -- 0xD4
            D new <- pop
            H.insert reg 0 new
  SetLocal1 -> do -- 0xD5
            D new <- pop
            H.insert reg 1 new
  SetLocal2 -> do -- 0xD6
            D new <- pop
            H.insert reg 2 new
  SetLocal3 -> do -- 0xD7
            D new <- pop
            H.insert reg 3 new
          _ -> fail$ "didn't match opcode " ++ show op-}

  {-(_, ((newsp,newops,_,_):_), _) <- get
  case newsp of
    -1 -> let (D ret:_) = newops in return ret
    otherwise -> r_f-}

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
find_property :: ConstantPool -> MultinameIdx -> ScopeStack -> AVM3 VmRt
find_property cp idx ss = do
  Multiname_QName nSInfoIdx stringIdx <- get_multiname cp idx
  name <- liftM2 B.append (resolve_nsinfo cp nSInfoIdx) (get_string cp stringIdx)
  --p$ "FindProperty [" ++ show idx ++ " " ++ name ++ "]"
  --p$ "\tscope_stack length " ++ (show$ length ss)
  attempt1 <- search_stack (Ext name) ss
  case attempt1 of
    Just obj -> do
      --p$ "found obj in attempt1"
      return obj
    Nothing -> do
      attempt2 <- traits_ref cp idx ss
      case attempt2 of
        Just obj -> do
          --p$ "found obj in attempt2"
          return obj
        Nothing -> fail "find_property - couldn't find_property"
  where
    search_stack :: VmRtP -> ScopeStack -> AVM3 (Maybe VmRt)
    search_stack key [] = return Nothing
    search_stack key@(Ext str) ((top, iid):stack) = do
      if str == BC.pack "Foo"
        then do
          list <- H.toList top
          p$ show list
          p$ "key " ++ show key
        else return ()
      maybeValue <- H.lookup top key
      case maybeValue of
        Nothing -> search_stack key stack
        Just value -> returnJ$ VmRt_Object top iid

    traits_ref :: ConstantPool -> MultinameIdx -> ScopeStack -> AVM3 (Maybe VmRt)
    traits_ref cp idx [] = return Nothing
    traits_ref cp idx ((top, iid):stack) = do
      maybeClassInfoIdx <- H.lookup top pfx_class_info_idx
      case maybeClassInfoIdx of
        Just (VmRtInternalInt classIdx) -> do
          ClassInfo _ traits <- get_class cp classIdx
          let matchingTraits = filter (\(TraitsInfo idx2 _ _ _ _) -> idx2 == idx) traits
          --p$ "got class info of pfx_class_info_idx" ++ show matchingTraits
          case length matchingTraits of
            0 -> return Nothing
            1 -> returnJ$ VmRt_Object top iid
            otherwise -> fail "traits_ref - too many matching traits"
        otherwise -> traits_ref cp idx stack

{-
  "The indexing of elements on the local scope stack is the reverse of the
  indexing of elements on the local operand stack."
-}

{-get_scoped_object :: ScopeStack -> U8 -> AVM3 Ops
get_scoped_object ss idx = undefined-}

init_property :: ConstantPool -> VmObject -> MultinameIdx -> VmRt -> AVM3 ()
init_property cp this idx value = do
  Multiname_QName nSInfoIdx stringIdx <- get_multiname cp idx
  name <- liftM2 B.append (resolve_nsinfo cp nSInfoIdx) (get_string cp stringIdx)
  p$ "InitProperty [" ++ show idx ++ " " ++ BC.unpack name ++ "]"
  maybeProp <- H.lookup this (Ext name)
  case maybeProp of
    Nothing -> do
      insert this (Ext name) value
      return ()
    otherwise -> fail$ "init_property - property [" ++ BC.unpack name ++ "] exists"
