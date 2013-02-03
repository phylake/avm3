{-# LANGUAGE ScopedTypeVariables #-}
module Vm.Execute where

import           Abc.DeepSeq
import           Abc.Def
import           Abc.Deserialize
import           Abc.Json
import           Abc.Json2
import           Control.Applicative
import           Control.DeepSeq
import           Data.Int
import           Data.Maybe (listToMaybe)
import           Data.Word
import           Ecma.Prims
import           MonadLib as ML hiding (get, set)
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
import qualified Data.HashTable.IO as H

p :: String -> AVM3 ()
p = liftIO.putStrLn

returnJ :: a -> AVM3 (Maybe a)
returnJ = return. Just

returnN :: AVM3 (Maybe a)
returnN = return Nothing

avm_prefix :: String
avm_prefix = "avm3internal_"

pfx_class_info_idx :: String
pfx_class_info_idx = avm_prefix ++ "class_info_idx"

test_file = do
  bs <- B.readFile "abc/Test.abc"
  (abc :: Abc) <- S.evalStateT parseAbc bs
  --writeFile "abc/Test.abc.json"$ encode$ abcToJson abc
  ht <- H.new
  (either::Either String (), _) <- abc `deepseq`
    (runStateT (ht, [])$ runExceptionT$ execute_abc abc)
  case either of
    Left err -> Prelude.putStrLn$ "EXCEPTION\n" ++ (unlines$ map ("\t"++)$ lines err)
    otherwise -> return ()

execute_abc :: Abc -> AVM3 ()
execute_abc abc = do
  build_cp abc
  cp <- get_cp

  p$ show $ abcStrings abc
  p$ show $ abcNsInfo abc
  --p$ "nssets " ++ (show $ abcNsSet abc)
  p$ show $ abcMultinames abc
  --p$ "\nmethod bodies\n" ++ unlines (map (show.mbCode) $ abcMethodBodies abc)
  --p$ "\nscripts\n" ++ unlines (map show$ abcScripts abc)
  p$ "\nclassinfos\n" ++ unlines (map show$ abcClasses abc)

  ScriptInfo _ ((TraitsInfo _ _ _ (TT_Class (TraitClass _ idx)) _):[]) <- get_script 0
  {-ClassInfo msidx _ <- get_class idx
  MethodSignature returnT _ name _ _ _ <- get_methodSig msidx
  MethodBody _ _ _ _ _ mb_Code _ _ <- get_methodBody msidx-}
  
  p$ "-------------------------------------------"
  global <- liftIO build_global_scope
  let ops = [O$ NewClass idx,O PushScope,O ReturnVoid]
  (c, _) <- run_function [global] [VmRt_Object global] ops
  case c of
    Yield v -> p$ show v
    otherwise -> raise "didn't yield anything"
  p$ "-------------------------------------------"

  return ()

run_function :: ScopeStack -> Registers -> Ops -> AVM3 (VmCont, Ops)
run_function ss reg ops = do
  p$ "run_function"
  --p$ unlines$ map ((++)"\t" . show) ops
  p$ "\tops\n\t" ++ show ops
  (c, ops2) <- next_cont ops
  (c2, ops3) <- case c of
    NoMatch -> do
      let (dataOps, rest) = splitAt 1 ops
      p$ "NoMatch"
      p$ "\t" ++ show dataOps
      mod_ops (++dataOps)
      run_function ss reg rest
    Yield v -> return (Yield v, [])
    OpsMod f -> run_function ss reg (f ops2)
    OpsModM f -> f ss >>= return . flip (,) ops2
    OpsMod2 f -> run_function ss reg (f reg ops2)
    RegMod f -> run_function ss (f reg) ops2
    StackMod f -> run_function (f ss) reg ops2
    FindProp idx -> do
      vmrt <- find_property idx ss
      return (Yield vmrt, ops2)
  case c2 of
    NoMatch -> return (c2, ops3)
    otherwise -> do
      dataOps <- get_ops
      p$ "Reassembling"
      p$ "\tdataOps     " ++ show dataOps
      p$ "\tops3        " ++ show ops3
      p$ "\treassembled " ++ show (dataOps ++ ops3)
      set_ops []
      return (c2, dataOps ++ ops3)

show_ops :: String -> Ops -> AVM3 ()
show_ops header ops = do
  p$ header
  liftIO. putStrLn. unlines$ map (\s -> "\t" ++ show s) ops

build_global_scope :: IO VmObject
build_global_scope = do
  int <- H.new
  H.insert int "MAX_VALUE" (VmRt_Int 2147483647)

  global <- H.new
  H.insert global "int" (VmRt_Object int)

  return global

next_cont = n_c

{-
ScopeStack is first because i'm replicating as3 closures with
real haskell closures for now
-}
n_c :: Ops -> AVM3 (VmCont, Ops)
-- n_c {- 0xA0   -} reg ((D a):(D b):(O Add):ops) = n_c ss reg ((D$ a+b):ops)
-- n_c {- 0x30 1 -} reg (_:(O PushScope):ops) = raise "n_c - PushScope"
n_c {-      0 -} [] = raise "empty ops"
n_c {-      1 -} (D ret:[]) = yield [] ret
n_c {- 0x1D   -} (O PopScope:ops) = mod_ss ops tail
n_c {- 0x24   -} (O (PushByte u8):ops) = cons_vmrt ops$ VmRt_Int$ fromIntegral u8
n_c {- 0x26   -} (O PushTrue:ops) = cons_vmrt ops$ VmRt_Boolean True
n_c {- 0x27   -} (O PushFalse:ops) = cons_vmrt ops$ VmRt_Boolean False
n_c {- 0x28   -} (O PushNaN:ops) = cons_vmrt ops$ VmRt_Number nan
n_c {- 0x29   -} (D _:O Pop:ops) = ops_mod ops id
n_c {- 0x2A   -} (D a:O Dup:ops) = ops_mod ops$ (:)(D a) . (:)(D a)
n_c {- 0x2B   -} (D a:D b:O Swap:ops) = ops_mod ops$ (:)(D b) . (:)(D a)
n_c {- 0x2C   -} (O (PushString idx):ops) = get_string idx >>= cons_vmrt ops . VmRt_String
n_c {- 0x2D   -} (O (PushInt idx):ops) = get_int idx >>= cons_vmrt ops . VmRt_Int
n_c {- 0x2E   -} (O (PushUInt idx):ops) = get_uint idx >>= cons_vmrt ops . VmRt_Uint
n_c {- 0x2F   -} (O (PushDouble idx):ops) = get_double idx >>= cons_vmrt ops . VmRt_Number
n_c {- 0x30 0 -} (D (VmRt_Object v):(O PushScope):ops) = mod_ss ops$ (:)v
n_c {- 0x47   -} (O ReturnVoid:ops) = yield ops VmRt_Undefined
n_c {- 0x48   -} (D v:(O ReturnValue):ops) = yield ops v
--n_c {- 0x58   -} ss reg (O (NewClass idx):ops) = new_class ss idx >>= \ss' -> n_c ss' reg ops
n_c {- 0x5E   -} (O (FindProperty idx):ops) = return (FindProp idx, ops)
--n_c {- 0x60   -} ss reg (O (GetLex idx):ops) = n_c ss reg$ T[O$ FindPropStrict idx,O$ GetProperty idx]:ops
--n_c {- 0x65   -} ss reg (O (GetScopeObject idx):ops) = n_c ss reg ((D$VmRt_Object$ ss !! fromIntegral idx):ops)
--n_c {- 0x68   -} ss reg (D vmrt:D (VmRt_Object this):O (InitProperty idx):ops) = init_property this idx vmrt >>= \v -> n_c ss reg$ T[D v]:ops
n_c {- 0x70   -} (D v:O ConvertString:ops) = ops_mod ops$ (:)(convert_string v)
n_c {- 0x73   -} (D v:O ConvertInt:ops) = ops_mod ops$ (:)(convert_int v)
n_c {- 0x74   -} (D v:O ConvertUInt:ops) = ops_mod ops$ (:)(convert_uint v)
n_c {- 0x75   -} (D v:O ConvertDouble:ops) = ops_mod ops$ (:)(convert_double v)
n_c {- 0x76   -} (D v:O ConvertBoolean:ops) = ops_mod ops$ (:)(convert_boolean v)
n_c {- 0x77   -} (D v:O ConvertObject:ops) = ML.raise "NI: ConvertObject"
n_c {- 0xD0   -} (O GetLocal0:ops) = ops_mod2 ops$ \reg -> (:)(D$ reg !! 0)
n_c {- 0xD1   -} (O GetLocal1:ops) = ops_mod2 ops$ \reg -> (:)(D$ reg !! 1)
n_c {- 0xD2   -} (O GetLocal2:ops) = ops_mod2 ops$ \reg -> (:)(D$ reg !! 2)
n_c {- 0xD3   -} (O GetLocal3:ops) = ops_mod2 ops$ \reg -> (:)(D$ reg !! 3)
n_c {- 0xD4   -} (D new:O SetLocal0:ops) = reg_mod ops$ \reg -> let (h,(_:t)) = splitAt 0 reg in h ++ (new:t)
n_c {- 0xD5   -} (D new:O SetLocal1:ops) = reg_mod ops$ \reg -> let (h,(_:t)) = splitAt 1 reg in h ++ (new:t)
n_c {- 0xD6   -} (D new:O SetLocal2:ops) = reg_mod ops$ \reg -> let (h,(_:t)) = splitAt 2 reg in h ++ (new:t)
n_c {- 0xD7   -} (D new:O SetLocal3:ops) = reg_mod ops$ \reg -> let (h,(_:t)) = splitAt 3 reg in h ++ (new:t)
n_c _ = return (NoMatch, [])
{-n_c ss reg (T topOps:rest) = do
  midOps <- get_ops
  set_ops []
  p$ "\ttopOps      " ++ show topOps
  p$ "\tmidOps      " ++ show midOps
  p$ "\treassembled " ++ show (topOps ++ midOps ++ rest)
  n_c ss reg (topOps ++ midOps ++ rest)
n_c ss reg ops = do
  let (dataOps, rest) = splitAt 1 ops
  mod_ops (++dataOps)
  n_c ss reg rest-}

convert_string :: VmRt -> VmRtOp
convert_string = D . VmRt_String . to_string

convert_double :: VmRt -> VmRtOp
convert_double = D . VmRt_Number . to_number

convert_boolean :: VmRt -> VmRtOp
convert_boolean = D . VmRt_Boolean . to_boolean

convert_int :: VmRt -> VmRtOp
convert_int = D . VmRt_Int . to_int32

convert_uint :: VmRt -> VmRtOp
convert_uint = D . VmRt_Uint . to_uint32

new_function :: AVM3 ()
new_function = undefined

{-new_class :: ScopeStack -> ClassInfoIdx -> AVM3 ScopeStack
new_class scope_stack idx = do
  p "NewClass"
  
  ClassInfo msi traits <- get_class idx
  
  MethodBody _ _ _ _ _ code _ _ <- get_methodBody msi
  let ops = map O code
  p$ "\tops: " ++ show ops
  
  (klass :: VmObject) <- liftIO$ H.new
  liftIO$ H.insert klass pfx_class_info_idx (VmRtInternal_Int idx)
  
  let global = last scope_stack
  p$ "\trunning function"
  next_cont [global] [VmRt_Object klass] ops
  p$ "\t##############    FUNCTION RAN SUCCESSFULLY"
  klass_name <- class_name traits
  liftIO$ H.insert global klass_name$ VmRt_Object klass
  return$ init scope_stack ++ [global]
  where
    class_name :: [TraitsInfo] -> AVM3 String
    class_name traits = return "Test"-}

{- TODO dynamic multinames (pattern match Ops) -}
{-
TODO resolve against
  1. method closures
  2. declared traits (on the method body, instance info?, class info?)
  3. dynamic properties
  4. prototype chain
-}
find_property :: MultinameIdx -> ScopeStack -> AVM3 VmRt
find_property idx ss = do
  name <- resolve_multiname idx
  p$ "FindProperty [" ++ show idx ++ " " ++ name ++ "]"
  --p$ "\tscope_stack length " ++ (show$ length ss)
  attempt1 <- search_stack name ss
  case attempt1 of
    Just obj -> return obj
    Nothing -> do
      attempt2 <- traits_ref idx name ss
      case attempt2 of
        Just obj -> return obj
        Nothing -> raise "find_property - couldn't find_property"
  --return ()
  where
    traits_ref :: MultinameIdx -> String -> ScopeStack -> AVM3 (Maybe VmRt)
    traits_ref idx name [] = return Nothing
    traits_ref idx name (top:stack) = do
      maybeClassInfoIdx <- liftIO$ H.lookup top pfx_class_info_idx
      case maybeClassInfoIdx of
        Just (VmRtInternal_Int class_idx) -> do
          ClassInfo msi traits <- get_class class_idx
          let matchingTraits = filter (\(TraitsInfo idx2 _ _ _ _) -> idx2 == idx) traits
          --p$ "got class info of pfx_class_info_idx"
          case length matchingTraits of
            0 -> return Nothing
            1 -> returnJ$ VmRt_Object top
            otherwise -> raise "traits_ref - too many matching traits"
        otherwise -> traits_ref idx name stack

    search_stack :: String -> ScopeStack -> AVM3 (Maybe VmRt)
    search_stack key [] = return Nothing
    search_stack key (top:stack) = do
      {-list <- liftIO$ H.toList top
      p$ show list
      p$ "key " ++ key-}
      maybeValue <- liftIO$ H.lookup top key
      case maybeValue of
        Nothing -> search_stack key stack
        Just value -> returnJ$ VmRt_Object top

{-
  "The indexing of elements on the local scope stack is the reverse of the
  indexing of elements on the local operand stack."
-}

{-get_scoped_object :: ScopeStack -> U8 -> AVM3 Ops
get_scoped_object ss idx = undefined-}

{-
TODO this doesn't go back onto the stack but needs to be updated in memory on
the object to which this object is attached
-}
{-init_property :: VmObject -> MultinameIdx -> VmRt -> AVM3 VmRt
init_property this idx value = do
  name <- resolve_multiname idx
  p$ "InitProperty [" ++ show idx ++ " " ++ name ++ "]"
  maybeProp <- liftIO$ H.lookup this name
  case maybeProp of
    Nothing -> do
      liftIO$ H.insert this name value
      return$ VmRt_Object this
    otherwise -> raise$ "init_property - property [" ++ name ++ "] exists"-}






















