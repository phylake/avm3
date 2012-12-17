module Vm.Execute where

import           Abc.Def
import           Abc.Deserialize
import           Abc.DeepSeq
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad.State
import           Data.Int
import           Data.Word
import           TFish
import           Util.Words
import           Util.Misc
import           Vm.Def
import           Vm.Store
import           Vm.Lookups
import qualified Data.HashTable.IO as H
import qualified Data.ByteString.Lazy as BS

test_file = do
  bs <- BS.readFile "abc/Test.abc"
  abc <- evalStateT parseAbc bs
  {-abc `deepseq` evalStateT (execute_abc abc) $ Execution ([], [], H.new)
  return ()-}
  abc `deepseq` execute_abc abc

execute_abc :: Abc -> IO ()
execute_abc abc = do
  cp <- build_cp abc

  putStrLn$ show $ abcStrings abc
  putStrLn$ show $ abcNsInfo abc
  --putStrLn$ "nssets " ++ (show $ abcNsSet abc)
  putStrLn$ show $ abcMultinames abc
  --putStrLn$ "\nmethod bodies\n" ++ unlines (map (show.mbCode) $ abcMethodBodies abc)
  --putStrLn$ "scripts length " ++ (show $ length $ abcScripts abc)

  (Just (ScriptInfo initMeth ((TraitsInfo _ _ _ (TT_Class (TraitClass _ idx)) _):[]))) <- get_script cp $ fromIntegral ((length $ abcScripts abc) -1)
  putStrLn$ show $ abcScripts abc
  putStrLn$ show idx
  (Just (ClassInfo msidx _)) <- get_class cp idx
  (Just (MethodSignature returnT _ name _ _ _)) <- get_methodSig cp msidx
  --(Just (MethodBody _ _ _ _ _ mb_Code _ mb_Traits)) <- get_methodBody cp msidx
  (Just (MethodBody _ _ _ _ _ mb_Code _ mb_Traits)) <- get_methodBody cp initMeth
  putStrLn$ show mb_Code
  
  putStrLn$ "-------------------------------------------"
  global <- build_global_scope
  ret <- m_e [] (prep_method_entry global mb_Code) [] cp
  putStrLn$ "-------------------------------------------"

  return ()

show_ops :: (Show a) => [a] -> String
show_ops ops = unlines$ map (\s -> "\t" ++ show s) ops

prep_method_entry :: VmObject -> [OpCode] -> Ops
prep_method_entry this code = (D$ VmRt_Object this):(O SetLocal0):(map O code)

build_global_scope :: IO VmObject
build_global_scope = do
  int <- H.new
  H.insert int "MAX_VALUE" (VmRt_Int 2147483647)

  global <- H.new
  H.insert global "int" (VmRt_Object int)
  H.insert global "Object" (VmRt_Object global) -- this is weird
  
  return global

new_class :: ConstantPool -> ClassInfoIdx -> IO VmObject
new_class cp idx = do
  maybeClassInfo <- get_class cp idx
  case maybeClassInfo of
    Nothing -> fail "new_class 1 failed"
    Just (ClassInfo msi _) -> do
      maybeMethodBody <- get_methodBody cp msi
      case maybeMethodBody of
        Nothing -> fail "new_class 2 failed"
        Just (MethodBody _ _ _ _ _ code _ _) -> H.new

-- method entry
m_e :: Registers -> Ops -> ScopeStack -> ConstantPool -> IO VmRt
m_e reg ((D (VmRt_Object v)):(O PushScope):ops) ss cp = m_e reg ops (v:ss) cp

m_e reg ((O (NewClass idx)):ops) ss cp = do
  putStrLn "NewClass"
  m_e reg ops ss cp
  where
    inst = new_class cp idx

m_e reg ((D (VmRt_Object this)):(O (GetLex idx)):ops) ss cp = do
  putStrLn "GetLex"
  putStrLn$ show_ops ops
  name <- resolve_multiname cp idx ops
  obj <- findProperty name ss
  m_e reg (D obj:ops) ss cp
  where
    findProperty :: String -> ScopeStack -> IO VmRt
    findProperty key [] = fail "ReferenceError"
    findProperty key (top:stack) = do
      {-list <- H.toList top
      putStrLn$ show list
      putStrLn$ "key " ++ key-}
      maybeValue <- H.lookup top key
      case maybeValue of
        Nothing -> findProperty key stack
        Just value -> return value

m_e reg ((O (GetScopeObject idx)):ops) ss cp = do
  putStrLn "GetScopeObject"
  m_e reg (obj:ops) ss cp
  where
    obj = D$ VmRt_Object$ ss !! (fromIntegral idx)

m_e reg ((D (VmRt_Object this)):(O (InitProperty idx)):ops) ss cp = do
  putStrLn "InitProperty"
  name <- resolve_multiname cp idx ops
  return$ VmRt_Object this

m_e (this:      reg) ((O GetLocal0):ops) ss cp = do
  putStrLn "GetLocal0"
  putStrLn$ show_ops ops
  m_e reg (D this:ops) ss cp
m_e               [] ((O GetLocal0):ops) ss cp = fail "nothing in register 0"
m_e (this:a:    reg) ((O GetLocal1):ops) ss cp = m_e reg (D a:ops) ss cp
m_e (this:a:b:  reg) ((O GetLocal2):ops) ss cp = m_e reg (D b:ops) ss cp
m_e (this:a:b:c:reg) ((O GetLocal3):ops) ss cp = m_e reg (D c:ops) ss cp

m_e (this:      reg) ((D new):(O SetLocal0):ops) ss cp = m_e (         new:reg) ops ss cp
m_e               [] ((D new):(O SetLocal0):ops) ss cp = m_e (         new:[])  ops ss cp
m_e (this:a:    reg) ((D new):(O SetLocal1):ops) ss cp = m_e (this:    new:reg) ops ss cp
m_e (this:a:b:  reg) ((D new):(O SetLocal2):ops) ss cp = m_e (this:a:  new:reg) ops ss cp
m_e (this:a:b:c:reg) ((D new):(O SetLocal3):ops) ss cp = m_e (this:a:b:new:reg) ops ss cp




























