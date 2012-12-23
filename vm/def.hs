module Vm.Def where

import Abc.Def
import MonadLib
import Data.Int
import Data.Word
import qualified Data.HashTable.IO as H

type ConstantPool = H.BasicHashTable String VmAbc
type Execution = (Registers, Ops, ScopeStack, ConstantPool)
type AVM3 = StateT Execution IO

{-data Heapidx_or_VmAbc = Former Int | Latter VmAbc
type Scope = [Heapidx_or_VmAbc] -- indexes into the heap or values
type ScopeStack = [Scope]-}

{-
  "The indexing of elements on the local scope stack is the reverse of the
  indexing of elements on the local operand stack."
-}

data HeapObject = Hn VmObject | H_func VmObject ScopeStack

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
