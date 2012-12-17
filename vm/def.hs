module Vm.Def where

import Abc.Def
import Control.Monad.State (StateT)
import Data.Int
import Data.Word
import qualified Data.HashTable.IO as H

type AVM3 = StateT Execution IO

{-data Heapidx_or_VmAbc = Former Int | Latter VmAbc
type Scope = [Heapidx_or_VmAbc] -- indexes into the heap or values
type ScopeStack = [Scope]-}

type ConstantPool = H.BasicHashTable String VmAbc

--newtype Execution = Execution { env :: (ScopeStack, Registers, IO ConstantPool) }
newtype Execution = Execution { env :: ([Int], [Int], IO ConstantPool) }

{-
  "The indexing of elements on the local scope stack is the reverse of the
  indexing of elements on the local operand stack."
-}

type VmObject = H.BasicHashTable String VmRt
--type ScopeStack = [H.BasicHashTable Multiname VmObject]
--type ScopeStack = [H.BasicHashTable String VmObject]
type ScopeStack = [VmObject]

type Registers = [VmRt]
type Ops = [VmRtOp]
data VmRtOp = O OpCode
            | D VmRt
            deriving (Show)

data VmRt = VmRt_Undefined
          | VmRt_Null
          | VmRt_Int Int32
          | VmRt_Uint U30
          | VmRt_Double Double
          | VmRt_String String
          | VmRt_Object VmObject
          deriving (Show)

{- 1:1 transformation of Abc to an ADT -}
data VmAbc = VmAbc_Int Int32
           | VmAbc_Uint U30
           | VmAbc_Double Double
           | VmAbc_String String
           | VmAbc_NsInfo NSInfo
           | VmAbc_NsSet NSSet
           | VmAbc_Multiname Multiname
           | VmAbc_MethodSig MethodSignature
           | VmAbc_Metadata Metadata
           | VmAbc_Instance InstanceInfo
           | VmAbc_Class ClassInfo
           | VmAbc_Script ScriptInfo
           | VmAbc_MethodBody MethodBody
           deriving (Show)
