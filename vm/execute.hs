module Vm.Execute where

import qualified Data.HashTable.IO as H

type HashTable k v = H.BasicHashTable k v

foo :: IO (HashTable Int Int)
foo = do
    ht <- H.new
    H.insert ht 1 1
    return ht
