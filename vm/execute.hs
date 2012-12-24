{-# LANGUAGE ScopedTypeVariables #-}
module Vm.Execute where

import           Abc.DeepSeq
import           Abc.Def
import           Abc.Deserialize
import           Control.Applicative
import           Control.DeepSeq
import           Data.Int
import           Data.Word
import           MonadLib hiding (get, set)
import           TFish
import           Util.Misc
import           Util.Words
import           Vm.Def
import           Vm.Lookups
import           Vm.Store
import qualified Control.Monad.State as S
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashTable.IO as H

test_file = do
  bs <- BS.readFile "abc/Test.abc"
  abc <- S.evalStateT parseAbc bs
  {-abc `deepseq` evalStateT (execute_abc abc) $ Execution ([], [], H.new)
  return ()-}
  ht <- H.new
  (either::Either String (), _) <- abc `deepseq`
    (runStateT ([],[],[],ht)$ runExceptionT$ execute_abc abc)
  case either of
    Left err -> Prelude.putStrLn$ "EXCEPTION\n\t" ++ err
    otherwise -> return ()

execute_abc :: Abc -> AVM3 ()
execute_abc abc = do
  build_cp abc
  cp <- get_cp

  liftIO.putStrLn$ show $ abcStrings abc
  liftIO.putStrLn$ show $ abcNsInfo abc
  --liftIO.putStrLn$ "nssets " ++ (show $ abcNsSet abc)
  liftIO.putStrLn$ show $ abcMultinames abc
  --liftIO.putStrLn$ "\nmethod bodies\n" ++ unlines (map (show.mbCode) $ abcMethodBodies abc)
  --liftIO.putStrLn$ "\nscripts\n" ++ unlines (map show$ abcScripts abc)

  ScriptInfo _ ((TraitsInfo _ _ _ (TT_Class (TraitClass _ idx)) _):[]) <- get_script 0
  {-ClassInfo msidx _ <- get_class idx
  MethodSignature returnT _ name _ _ _ <- get_methodSig msidx
  MethodBody _ _ _ _ _ mb_Code _ _ <- get_methodBody msidx-}
  
  liftIO.putStrLn$ "-------------------------------------------"
  global <- liftIO build_global_scope
  set_ss [global]
  let ops = [O$ NewClass idx,O PushScope,O ReturnVoid]
  run_function [VmRt_Object global] ops
  liftIO.putStrLn$ "-------------------------------------------"

  return ()

{-show_ops :: String -> AVM3 ()
show_ops header = do
  liftIO.putStrLn$ header
  get_ops >>= liftIO. putStrLn. unlines. map (\s -> "\t" ++ show s)-}

show_ops :: String -> Ops -> AVM3 ()
show_ops header ops = do
  liftIO.putStrLn$ header
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

run_function :: Registers -> Ops -> AVM3 VmRt
-- run_function {- 0xA0   -} reg ((D a):(D b):(O Add):ops) = run_function reg ((D$ a+b):ops)
-- run_function {- 0x30 1 -} reg (_:(O PushScope):ops) = raise "run_function - PushScope"
run_function {-      0 -} reg2 [] = raise "empty ops"
run_function {-      1 -} reg2 ((D ret):[]) = return ret
run_function {- 0x1D   -} reg ((O PopScope):ops) = pop_scope >> run_function reg ops
run_function {- 0x24   -} reg ((O (PushByte u8)):ops) = run_function reg ((D$ VmRt_Int$ fromIntegral u8):ops)
run_function {- 0x30 0 -} reg ((D (VmRt_Object v)):(O PushScope):ops) = push_scope v >> run_function reg ops
run_function {- 0x47   -} reg ((O ReturnVoid):_) = return VmRt_Undefined
run_function {- 0x48   -} reg ((D v):(O ReturnValue):_) = return v
run_function {- 0x58   -} reg ((O (NewClass idx)):ops) = new_class idx >> run_function reg ops
run_function {- 0x5E   -} reg ((O (FindProperty idx)):ops) = find_property idx >> run_function reg ops
run_function {- 0x60   -} reg ((O (GetLex idx)):ops) = get_lex idx ops >>= run_function reg
run_function {- 0x65   -} reg ((O (GetScopeObject idx)):ops) = get_scoped_object idx >> run_function reg ops
run_function {- 0x68   -} reg ((D (VmRt_Object this)):(O (InitProperty idx)):ops) = init_property this idx >> run_function reg ops
run_function {- 0xD0   -} reg@(r:      []) ((O GetLocal0):ops) = run_function reg (D r:ops)
run_function {- 0xD1   -} reg@(_:r:    []) ((O GetLocal1):ops) = run_function reg (D r:ops)
run_function {- 0xD2   -} reg@(_:_:r:  []) ((O GetLocal2):ops) = run_function reg (D r:ops)
run_function {- 0xD3   -} reg@(_:_:_:r:[]) ((O GetLocal3):ops) = run_function reg (D r:ops)
run_function {- 0xD4   -} (this:      reg) ((D new):(O SetLocal0):ops) = run_function (         new:reg) ops
run_function {- 0xD5   -} (this:a:    reg) ((D new):(O SetLocal1):ops) = run_function (this:    new:reg) ops
run_function {- 0xD6   -} (this:a:b:  reg) ((D new):(O SetLocal2):ops) = run_function (this:a:  new:reg) ops
run_function {- 0xD7   -} (this:a:b:c:reg) ((D new):(O SetLocal3):ops) = run_function (this:a:b:new:reg) ops




push_scope :: VmObject -> AVM3 ()
push_scope v = mod_ss ((:)v)

pop_scope :: AVM3 ()
pop_scope = mod_ss tail

new_class :: ClassInfoIdx -> AVM3 VmRt
new_class idx = do
  --show_ops "NewClass"
  ClassInfo msi traits <- get_class idx
  MethodBody _ _ _ _ _ code _ _ <- get_methodBody msi
  scope_stack <- get_ss
  let global = last scope_stack
  klass <- liftIO$ H.new >>= return.VmRt_Object
  let ops = map O code
  liftIO.putStrLn$ "ops: " ++ show ops
  liftIO.putStrLn$ "running function"
  run_function [klass] ops
  klass_name <- class_name traits
  liftIO$ H.insert global klass_name klass
  set_ss$ init scope_stack ++ [global]
  return klass
  where
    class_name :: [TraitsInfo] -> AVM3 String
    class_name traits = return "Test"

{- TODO dynamic multinames (pattern match Ops) -}
{-
TODO resolve against
  1. method closures
  2. declared traits
  3. dynamic properties
  4. prototype chain
-}
find_property :: MultinameIdx -> AVM3 ()
find_property idx = do
  name <- resolve_multiname idx
  liftIO.putStrLn$ "find_property " ++ show name
  attempt1 <- get_ss >>= search_stack name
  obj <- case attempt1 of
    Just obj -> return obj
    --Nothing -> traits_ref
    Nothing -> raise "couldn't find_property"
  return ()
  where
    traits_ref = undefined

    search_stack :: String -> ScopeStack -> AVM3 (Maybe VmRt)
    search_stack key [] = return Nothing
    search_stack key (top:stack) = do
      {-list <- liftIO$ H.toList top
      liftIO.putStrLn$ show list
      liftIO.putStrLn$ "key " ++ key-}
      maybeValue <- liftIO$ H.lookup top key
      case maybeValue of
        Nothing -> search_stack key stack
        Just value -> return$ Just value

get_lex :: MultinameIdx -> Ops -> AVM3 Ops
get_lex idx ops = do
  --show_ops "GetLex"
  return$ (O$ FindPropStrict idx):(O$ GetProperty idx):ops

{-
  "The indexing of elements on the local scope stack is the reverse of the
  indexing of elements on the local operand stack."
-}

get_scoped_object :: U8 -> AVM3 ()
get_scoped_object idx = do
  --show_ops "GetScopeObject"
  ss <- get_ss
  mod_ops$ cons$ ss !! (fromIntegral idx)
  where
    cons = (:). D. VmRt_Object

init_property :: VmObject -> MultinameIdx -> AVM3 ()
init_property this idx = do
  liftIO.putStrLn$ "InitProperty"
  name <- resolve_multiname idx
  return ()
  --return$ VmRt_Object this






















