module Vm.Execute where

import           Abc.DeepSeq
import           Abc.Def
import           Abc.Deserialize
import           Control.Applicative
import           Control.DeepSeq
import           Data.Int
import           Data.Word
import           MonadLib
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
  abc `deepseq` runStateT ([],[],[],ht) (execute_abc abc)

execute_abc :: Abc -> AVM3 ()
execute_abc abc = do
  build_cp abc
  cp <- get_cp

  lift.putStrLn$ show $ abcStrings abc
  lift.putStrLn$ show $ abcNsInfo abc
  --lift.putStrLn$ "nssets " ++ (show $ abcNsSet abc)
  lift.putStrLn$ show $ abcMultinames abc
  lift.putStrLn$ "\nmethod bodies\n" ++ unlines (map (show.mbCode) $ abcMethodBodies abc)
  --lift.putStrLn$ "scripts length " ++ (show $ length $ abcScripts abc)

  Just (ScriptInfo initMeth ((TraitsInfo _ _ _ (TT_Class (TraitClass _ idx)) _):[])) <- get_script $ fromIntegral ((length $ abcScripts abc) - 1)
  lift.putStrLn$ unlines$ map show$ abcScripts abc
  lift.putStrLn$ show idx
  Just (ClassInfo msidx _) <- get_class idx
  Just (MethodSignature returnT _ name _ _ _) <- get_methodSig msidx
  Just (MethodBody _ _ _ _ _ mb_Code _ mb_Traits) <- get_methodBody msidx
  --(Just (MethodBody _ _ _ _ _ mb_Code _ mb_Traits)) <- get_methodBody cp initMeth
  lift.putStrLn$ show mb_Code
  
  (lift build_global_scope) >>= set_ss
  set_ops ((O$ NewClass idx):O PushScope:O ReturnVoid:[])
  lift.putStrLn$ "-------------------------------------------"
  run_method
  lift.putStrLn$ "-------------------------------------------"

  return ()

show_ops :: String -> AVM3 ()
show_ops header = do
  lift.putStrLn$ header
  get_ops >>= lift. putStrLn. unlines. map (\s -> "\t" ++ show s)

build_global_scope :: IO ScopeStack
build_global_scope = do
  int <- H.new
  H.insert int "MAX_VALUE" (VmRt_Int 2147483647)

  global <- H.new
  H.insert global "int" (VmRt_Object int)

  return [global]

run_method :: AVM3 ()
run_method = do
  get_ops >>= switch
  ops <- get_ops
  case ops of
    (op:ops') -> run_method
    otherwise -> return ()
  where
    switch :: Ops -> AVM3 ()
    switch ((D (VmRt_Object v)):(O PushScope):ops) = set_ops ops >> push_scope v
    switch (_:(O PushScope):ops) = fail "fpm - PushScope"
    switch ((O (NewClass idx)):ops) = set_ops ops >> new_class idx
    switch ((O (FindProperty idx)):ops) = set_ops ops >> find_property idx
    switch ((O (GetLex idx)):ops) = set_ops ops >> get_lex idx
    switch ((O (GetScopeObject idx)):ops) = set_ops ops >> get_scoped_object idx
    switch ((D (VmRt_Object this)):(O (InitProperty idx)):ops) = set_ops ops >> init_property this idx
    switch ((O GetLocal0):ops) = do
      set_ops ops
      (this:reg) <- get_reg
      mod_ops$ \ops -> D this:ops
    switch ((O GetLocal1):ops) = do
      set_ops ops
      (this:a:reg) <- get_reg
      mod_ops$ \ops -> D a:ops
    switch ((O GetLocal2):ops) = do
      set_ops ops
      (this:a:b:reg) <- get_reg
      mod_ops$ \ops -> D b:ops
    switch ((O GetLocal3):ops) = do
      set_ops ops
      (this:a:b:c:reg) <- get_reg
      mod_ops$ \ops -> D c:ops
    switch ((D new):(O SetLocal0):ops) = set_ops ops >> mod_reg (\(this:      reg) -> (         new:reg))
    switch ((D new):(O SetLocal1):ops) = set_ops ops >> mod_reg (\(this:a:    reg) -> (this:    new:reg))
    switch ((D new):(O SetLocal2):ops) = set_ops ops >> mod_reg (\(this:a:b:  reg) -> (this:a:  new:reg))
    switch ((D new):(O SetLocal3):ops) = set_ops ops >> mod_reg (\(this:a:b:c:reg) -> (this:a:b:new:reg))

push_scope :: VmObject -> AVM3 ()
push_scope v = mod_ss ((:)v)

new_class :: ClassInfoIdx -> AVM3 ()
new_class idx = do
  lift.putStrLn$ "NewClass"
  maybeClassInfo <- get_class idx
  case maybeClassInfo of
    Nothing -> fail "NewClass - no ClassInfo"
    Just (ClassInfo msi traits) -> do
      maybeMethodBody <- get_methodBody msi
      case maybeMethodBody of
        Nothing -> fail "NewClass - no MethodBody"
        Just (MethodBody _ _ _ _ _ code _ _) -> do
          global <- get_ss >>= return.last
          klass <- lift$ H.new
          prep_method_entry klass code
          klass_name <- class_name traits
          lift$ H.insert global klass_name (VmRt_Object klass) -- TODO this won't make it
          return ()
  where
    class_name :: [TraitsInfo] -> AVM3 String
    class_name traits = return "Test"
    prep_method_entry :: VmObject -> [OpCode] -> AVM3 Ops
    prep_method_entry this code = return$ (D$ VmRt_Object this):(O SetLocal0):(map O code)


{- TODO dynamic multinames -}
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
  attempt1 <- get_ss >>= search_stack name
  obj <- case attempt1 of
    Just obj -> return obj
    Nothing -> traits_ref
  return ()
  where
    traits_ref = undefined

    search_stack :: String -> ScopeStack -> AVM3 (Maybe VmRt)
    search_stack key [] = return Nothing
    search_stack key (top:stack) = do
      {-list <- H.toList top
      putStrLn$ show list
      putStrLn$ "key " ++ key-}
      maybeValue <- lift$ H.lookup top key
      case maybeValue of
        Nothing -> search_stack key stack
        Just value -> return$ Just value

get_lex :: MultinameIdx -> AVM3 ()
get_lex idx = do
  show_ops "GetLex"
  mod_ops$ \ops -> ((O$ FindPropStrict idx):(O$ GetProperty idx):ops)

get_scoped_object :: U8 -> AVM3 ()
get_scoped_object idx = do
  show_ops "GetScopeObject"
  ss <- get_ss
  mod_ops$ (:)$ D$ VmRt_Object$ ss !! (fromIntegral idx)

init_property :: VmObject -> MultinameIdx -> AVM3 ()
init_property this idx = do
  lift.putStrLn$ "InitProperty"
  name <- resolve_multiname idx
  return ()
  --return$ VmRt_Object this






















