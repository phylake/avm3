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
import           Vm.Def
import           Vm.Store
import qualified Data.HashTable.IO as H
import qualified Data.ByteString.Lazy as BS

test_file = do
  bs <- BS.readFile "abc/Test.abc"
  abc <- evalStateT parseAbc bs
  abc `deepseq` evalStateT (execute_abc abc) $ Execution ([], [], H.new)
  return ()

execute_abc :: Abc -> AVM3 ()
execute_abc abc = do
  build_ht abc
  {-(Just m) <- get_int 1
  liftIO.putStrLn$ show m-}

  liftIO.putStrLn$ "strings " ++ (show $ abcStrings abc)
  liftIO.putStrLn$ "multinames " ++ (show $ abcMultinames abc)
  liftIO.putStrLn$ "\nmethod bodies\n" ++ unlines (map (show.mbCode) $ abcMethodBodies abc)
  liftIO.putStrLn$ "scripts length " ++ (show $ length $ abcScripts abc)
  --liftIO.putStrLn$ "method bodies " ++ (show $ abcMethodBodies abc)

  (Just (ScriptInfo initMeth ((TraitsInfo _ _ _ (TT_Class (TraitClass _ idx)) _):[]))) <- get_script $ fromIntegral ((length $ abcScripts abc) -1)
  liftIO.putStrLn$ show $ abcScripts abc
  liftIO.putStrLn$ show idx
  (Just (ClassInfo msidx _)) <- get_class idx
  (Just (MethodSignature returnT _ name _ _ _)) <- get_methodSig msidx
  --(Just (MethodBody _ _ _ _ _ mb_Code _ mb_Traits)) <- get_methodBody msidx
  (Just (MethodBody _ _ _ _ _ mb_Code _ mb_Traits)) <- get_methodBody initMeth
  liftIO.putStrLn$ show mb_Code

  return ()

--run_method :: Registers -> [Stack] -> [OpCode] -> (Registers, [Stack])
--run_method = undefined

build_global_scope :: IO ScopeStack
build_global_scope = do
  int <- H.new
  H.insert int "MAX_VALUE" (VmRt_Int 2147483647)

  {-global <- H.new
  H.insert global "int" int
  return [global]-}

  return [int]

resolve_multiname :: ConstantPool -> MultinameIdx -> IO String
resolve_multiname cp idx = return ""

-- method entry
m_e :: Registers -> Ops -> ScopeStack -> ConstantPool -> IO VmRt
m_e reg ((O PushScope):(D (VmRt_Object v)):ops) ss cp = m_e reg ops ss cp
-- shouldn't encounter this because the global scope should already exist on the scope stack
--m_e reg ((O PushScope):ops) [] = build_global_scope >>= m_e reg ops

m_e (this:reg)       ((O GetLocal0):ops) ss cp = m_e reg (D this:ops) ss cp
m_e (this:a:reg)     ((O GetLocal1):ops) ss cp = m_e reg (D a:ops) ss cp
m_e (this:a:b:reg)   ((O GetLocal2):ops) ss cp = m_e reg (D b:ops) ss cp
m_e (this:a:b:c:reg) ((O GetLocal3):ops) ss cp = m_e reg (D c:ops) ss cp

m_e (this:reg)       ((O SetLocal0):(D new):ops) ss cp = m_e (         new:reg) ops ss cp
m_e (this:a:reg)     ((O SetLocal1):(D new):ops) ss cp = m_e (this:    new:reg) ops ss cp
m_e (this:a:b:reg)   ((O SetLocal2):(D new):ops) ss cp = m_e (this:a:  new:reg) ops ss cp
m_e (this:a:b:c:reg) ((O SetLocal3):(D new):ops) ss cp = m_e (this:a:b:new:reg) ops ss cp

m_e reg ((O (GetScopeObject idx)):ops) ss cp = m_e reg (obj:ops) ss cp
  where
    obj = D (VmRt_Object (ss !! (fromIntegral idx)))

m_e reg ((O (GetLex idx)):(D (VmRt_Object this)):ops) ss cp = do
  mayb <- resolve_multiname cp idx >>= H.lookup this
  case mayb of
    Nothing -> fail "lookup failed"
    Just obj -> m_e reg (D obj:ops) ss cp





























