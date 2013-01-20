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
import           MonadLib hiding (get, set)
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
returnJ = return.Just

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
  run_function [global] [VmRt_Object global] ops
  p$ "-------------------------------------------"

  return ()

{-show_ops :: String -> AVM3 ()
show_ops header = do
  p$ header
  get_ops >>= liftIO. putStrLn. unlines. map (\s -> "\t" ++ show s)-}

show_ops :: String -> Ops -> AVM3 ()
show_ops header ops = do
  p$ header
  liftIO. putStrLn. unlines$ map (\s -> "\t" ++ show s) ops

prep_method_entry :: VmObject -> [OpCode] -> AVM3 Ops
prep_method_entry this code = return$ (D$ VmRt_Object this):(O SetLocal0):(map O code)

build_global_scope :: IO VmObject
build_global_scope = do
  int <- H.new
  H.insert int "MAX_VALUE" (VmRt_Int 2147483647)

  global <- H.new
  H.insert global "int" (VmRt_Object int)

  return global

run_function = r_f

{-
ScopeStack is first because i'm replicating as3 closures with
real haskell closures for now
-}
r_f :: ScopeStack -> Registers -> Ops -> AVM3 (VmRt, Ops)
-- r_f {- 0xA0   -} reg ((D a):(D b):(O Add):ops) = r_f ss reg ((D$ a+b):ops)
-- r_f {- 0x30 1 -} reg (_:(O PushScope):ops) = raise "r_f - PushScope"
r_f {-      0 -} ss reg [] = raise "empty ops"
r_f {-      1 -} ss reg (D ret:[]) = return (ret, [])
r_f {- 0x1D   -} (_:ss) reg (O PopScope:ops) = r_f ss reg ops
r_f {- 0x24   -} ss reg (O (PushByte u8):ops) = do
  p$ "PushByte " ++ show u8
  r_f ss reg$ T[D. VmRt_Int$ fromIntegral u8]:ops
r_f {- 0x26   -} ss reg (O PushTrue:ops) = r_f ss reg$ (D$ VmRt_Boolean True):ops
r_f {- 0x27   -} ss reg (O PushFalse:ops) = r_f ss reg$ (D$ VmRt_Boolean False):ops
r_f {- 0x28   -} ss reg (O PushNaN:ops) = r_f ss reg$ (D$ VmRt_Number nan):ops
r_f {- 0x29   -} ss reg (D _:O Pop:ops) = r_f ss reg ops
r_f {- 0x2A   -} ss reg (D a:O Dup:ops) = r_f ss reg (D a:D a:ops)
r_f {- 0x2B   -} ss reg (D a:D b:O Swap:ops) = r_f ss reg (D b:D a:ops)
r_f {- 0x2C   -} ss reg (O (PushString idx):ops) = get_string idx >>= \v -> r_f ss reg$ T[D$ VmRt_String v]:ops
r_f {- 0x2D   -} ss reg (O (PushInt idx):ops) = get_int idx >>= \v -> r_f ss reg$ T[D$ VmRt_Int v]:ops
r_f {- 0x2E   -} ss reg (O (PushUInt idx):ops) = get_uint idx >>= \v -> r_f ss reg$ T[D$ VmRt_Uint v]:ops
r_f {- 0x2F   -} ss reg (O (PushDouble idx):ops) = get_double idx >>= \v -> r_f ss reg$ T[D$ VmRt_Number v]:ops
r_f {- 0x30 0 -} ss reg (D (VmRt_Object v):(O PushScope):ops) = r_f (v:ss) reg ops
r_f {- 0x47   -} ss reg (O ReturnVoid:ops) = return (VmRt_Undefined, ops)
r_f {- 0x48   -} ss reg (D v:(O ReturnValue):ops) = return (v, ops)
r_f {- 0x58   -} ss reg (O (NewClass idx):ops) = new_class ss idx >>= \ss' -> r_f ss' reg ops
r_f {- 0x5E   -} ss reg (O (FindProperty idx):ops) = find_property ss idx >>= \v -> r_f ss reg$ T[D v]:ops
r_f {- 0x60   -} ss reg (O (GetLex idx):ops) = r_f ss reg$ T[O$ FindPropStrict idx,O$ GetProperty idx]:ops
r_f {- 0x65   -} ss reg (O (GetScopeObject idx):ops) = r_f ss reg ((D$VmRt_Object$ ss !! fromIntegral idx):ops)
r_f {- 0x68   -} ss reg (D (VmRt_Object this):D vmrt:O (InitProperty idx):ops) = init_property this idx vmrt >>= \v -> r_f ss reg$ T[D v]:ops
r_f {- 0x70   -} ss reg (D v:O ConvertString:ops) = convert_string v >>= \v -> r_f ss reg$ T[D v]:ops
r_f {- 0x73   -} ss reg (D v:O ConvertInt:ops) = convert_int v >>= \v -> r_f ss reg$ T[D v]:ops
r_f {- 0x74   -} ss reg (D v:O ConvertUInt:ops) = convert_uint v >>= \v -> r_f ss reg$ T[D v]:ops
r_f {- 0x75   -} ss reg (D v:O ConvertDouble:ops) = convert_double v >>= \v -> r_f ss reg$ T[D v]:ops
r_f {- 0x76   -} ss reg (D v:O ConvertBoolean:ops) = convert_boolean v >>= \v -> r_f ss reg$ T[D v]:ops
r_f {- 0x77   -} ss reg (D v:O ConvertObject:ops) = undefined
r_f {- 0xD0   -} ss reg@(r:      []) (O GetLocal0:ops) = r_f ss reg (D r:ops)
r_f {- 0xD1   -} ss reg@(_:r:    []) (O GetLocal1:ops) = r_f ss reg (D r:ops)
r_f {- 0xD2   -} ss reg@(_:_:r:  []) (O GetLocal2:ops) = r_f ss reg (D r:ops)
r_f {- 0xD3   -} ss reg@(_:_:_:r:[]) (O GetLocal3:ops) = r_f ss reg (D r:ops)
r_f {- 0xD4   -} ss (this:      reg) (D new:O SetLocal0:ops) = r_f ss (         new:reg) ops
r_f {- 0xD5   -} ss (this:a:    reg) (D new:O SetLocal1:ops) = r_f ss (this:    new:reg) ops
r_f {- 0xD6   -} ss (this:a:b:  reg) (D new:O SetLocal2:ops) = r_f ss (this:a:  new:reg) ops
r_f {- 0xD7   -} ss (this:a:b:c:reg) (D new:O SetLocal3:ops) = r_f ss (this:a:b:new:reg) ops
r_f ss reg (T midOps:rest) = do
  topOps <- get_ops
  set_ops []
  p$ "\tmidOps      " ++ show midOps
  p$ "\ttopOps      " ++ show topOps
  p$ "\treassembled " ++ show (topOps ++ midOps ++ rest)
  r_f ss reg (topOps ++ midOps ++ rest)
r_f ss reg ops = do
  --p$ "\tinexhaustive pattern for ops  " ++ show ops
  let (dataOps, rest) = splitAt 1 ops
  mod_ops (++dataOps)
  --p$ "\tdataOps " ++ show dataOps
  r_f ss reg rest

convert_string :: VmRt -> AVM3 VmRt
convert_string = return. VmRt_String. to_string

convert_double :: VmRt -> AVM3 VmRt
convert_double = return. VmRt_Number. to_number

convert_boolean :: VmRt -> AVM3 VmRt
convert_boolean = return. VmRt_Boolean. to_boolean

convert_int :: VmRt -> AVM3 VmRt
convert_int = return. VmRt_Int. to_int32

convert_uint :: VmRt -> AVM3 VmRt
convert_uint = return. VmRt_Uint. to_uint32

new_function :: AVM3 ()
new_function = undefined

new_class :: ScopeStack -> ClassInfoIdx -> AVM3 ScopeStack
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
  run_function [global] [VmRt_Object klass] ops
  p$ "\t##############    FUNCTION RAN SUCCESSFULLY"
  klass_name <- class_name traits
  liftIO$ H.insert global klass_name$ VmRt_Object klass
  return$ init scope_stack ++ [global]
  where
    class_name :: [TraitsInfo] -> AVM3 String
    class_name traits = return "Test"

{- TODO dynamic multinames (pattern match Ops) -}
{-
TODO resolve against
  1. method closures
  2. declared traits (on the method body, instance info?, class info?)
  3. dynamic properties
  4. prototype chain
-}
find_property :: ScopeStack -> MultinameIdx -> AVM3 VmRt
find_property ss idx = do
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

init_property :: VmObject -> MultinameIdx -> VmRt -> AVM3 VmRt
init_property this idx value = do
  name <- resolve_multiname idx
  p$ "InitProperty [" ++ show idx ++ " " ++ name ++ "]"
  maybeProp <- liftIO$ H.lookup this name
  case maybeProp of
    Nothing -> do
      liftIO$ H.insert this name value
      return$ VmRt_Object this
    otherwise -> raise$ "init_property - property [" ++ name ++ "] exists"






















