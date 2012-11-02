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

data Abcs = Abc_Int32 [Int32]
          | Abc_Word32 [Word32]
          | Abc_Double [Double]
          | Abc_String [String]
          | Abc_NSInfo [NSInfo]
          | Abc_NSSet [NSSet]
          | Abc_Multiname [Multiname]
          | Abc_MethodSignature [MethodSignature]
