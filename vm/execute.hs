module Vm.Execute where

import           ABC.Def
import           ABC.Deserialize
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

run_method :: Registers -> [Stack] -> [OpCode] -> (Registers, [Stack])
run_method = undefined
