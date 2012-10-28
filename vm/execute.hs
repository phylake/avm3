module Vm.Execute where

import qualified Data.HashTable.IO as H

type HashTable k v = H.BasicHashTable k v

data Foo = Foo {
    bar :: String,
    baz :: Int
}

foo :: IO (HashTable Int Foo)
foo = do
    ht <- H.new
    H.insert ht 1 $ Foo "hi!" 2
    (Just f) <- H.lookup ht 1
    putStrLn $ bar f
    return ht
