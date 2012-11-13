module Vm.Def where

import ABC.Def
import Control.Monad.State (StateT)
import Data.Int
import qualified Data.HashTable.IO as H

type CPoolHashTable k v = H.BasicHashTable k v
type AVM3 a = StateT Execution IO a

type ScopeStack = [Int]
type Registers = [(Int, Int)]
newtype Execution = Execution { ex :: (ScopeStack, Registers, IO (CPoolHashTable String Abc2)) }

newtype Abcs = Abcs { inner :: SingleHash }
--newtype Abcs = Abcs { inner :: MultipleHash }

type SingleHash = IO (CPoolHashTable String Abc2)

data Abc2 = Abc2_Int Int32
          | Abc2_Uint U30
          | Abc2_Double Double
          | Abc2_String String
          | Abc2_NsInfo NSInfo
          | Abc2_NsSet NSSet
          | Abc2_Multiname Multiname
          | Abc2_MethodSig MethodSignature
          | Abc2_Metadata Metadata
          | Abc2_Instance InstanceInfo
          | Abc2_Class ClassInfo
          | Abc2_Script ScriptInfo
          | Abc2_MethodBody MethodBody
          deriving (Show)

{-data MultipleHash = Abc2_Ints IO (CPoolHashTable Int32 [Int32])
                  | Abc2_Uints IO (CPoolHashTable Int32 [U30])
                  | Abc2_Doubles IO (CPoolHashTable Int32 [Double])
                  | Abc2_Strings IO (CPoolHashTable Int32 [String])
                  | Abc2_NsInfo IO (CPoolHashTable Int32 [NSInfo])
                  | Abc2_NsSet IO (CPoolHashTable Int32 [NSSet])
                  | Abc2_Multinames IO (CPoolHashTable Int32 [Multiname])
                  | Abc2_MethodSigs IO (CPoolHashTable Int32 [MethodSignature])
                  | Abc2_Metadata IO (CPoolHashTable Int32 [Metadata])
                  | Abc2_Instances IO (CPoolHashTable Int32 [InstanceInfo])
                  | Abc2_Classes IO (CPoolHashTable Int32 [ClassInfo])
                  | Abc2_Scripts IO (CPoolHashTable Int32 [ScriptInfo])
                  | Abc2_MethodBodies IO (CPoolHashTable Int32 [MethodBody])-}

