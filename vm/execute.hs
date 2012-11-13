module Vm.Execute where

import ABC.Def
import ABC.Deserialize
import Control.DeepSeq
import Control.Applicative
import Control.Monad.State
import Data.Int
import Data.Word
import Util.Words
import TFish
import Vm.Def
import Vm.Store
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
    (Just m) <- get_int 1
    liftIO.putStrLn$ show m

    (Just si) <- get_script ((length $ abcScripts abc) -1)
    liftIO.putStrLn$ show $ length $ abcScripts abc
    liftIO.putStrLn$ show $ abcScripts abc
    liftIO.putStrLn$ show si

    return ()
