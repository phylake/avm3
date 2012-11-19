module Vm.Def where

import ABC.Def
import Control.Monad.State (StateT)
import Data.Int
import Data.Word
import qualified Data.HashTable.IO as H

type CPoolHashTable k v = H.BasicHashTable k v
type AVM3 a = StateT Execution IO a

type Heap = Int


data Heapidx_or_VmAbc = Former Int | Latter VmAbc

type Scope = [Heapidx_or_VmAbc] -- indexes into the heap or values
type ScopeStack = [Scope]
type Registers = [VmAbc]
type ConstantPool = IO (CPoolHashTable String VmAbc)
newtype Execution = Execution { ex :: (ScopeStack, Registers, ConstantPool) }

newtype Abcs = Abcs { inner :: SingleHash }
--newtype Abcs = Abcs { inner :: MultipleHash }

type SingleHash = IO (CPoolHashTable String VmAbc)

{-
  "The indexing of elements on the local scope stack is the reverse of the
  indexing of elements on the local operand stack."
-}

data Stack = StackCode OpCode
           | StackData VmAbc

data VmMethod = VmMethod {
                         -- maybe body
                           vmm_mbody :: Maybe MethodBody
                         -- method body
                         , vmm_method :: MethodSignatureIdx
                         , vmm_maxStack :: U30
                         , vmm_localCount :: U30
                         , vmm_initScopeDepth :: U30
                         , vmm_maxScopeDepth :: U30
                         , vmm_code :: [OpCode]
                         , vmm_exceptions :: [Exception]
                         , vmm_traits :: [TraitsInfo]
                         -- method signature
                         , vmm_returnType :: MultinameIdx
                         , vmm_paramTypes :: [U30]
                         , vmm_methodName :: StringIdx
                         , vmm_flags :: Word8
                         , vmm_optionInfo :: Maybe [CPC]
                         , vmm_paramNames :: Maybe [U30]
                         }
                         deriving (Show)

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

{-data MultipleHash = VmAbc_Ints IO (CPoolHashTable Int32 [Int32])
                  | VmAbc_Uints IO (CPoolHashTable Int32 [U30])
                  | VmAbc_Doubles IO (CPoolHashTable Int32 [Double])
                  | VmAbc_Strings IO (CPoolHashTable Int32 [String])
                  | VmAbc_NsInfo IO (CPoolHashTable Int32 [NSInfo])
                  | VmAbc_NsSet IO (CPoolHashTable Int32 [NSSet])
                  | VmAbc_Multinames IO (CPoolHashTable Int32 [Multiname])
                  | VmAbc_MethodSigs IO (CPoolHashTable Int32 [MethodSignature])
                  | VmAbc_Metadata IO (CPoolHashTable Int32 [Metadata])
                  | VmAbc_Instances IO (CPoolHashTable Int32 [InstanceInfo])
                  | VmAbc_Classes IO (CPoolHashTable Int32 [ClassInfo])
                  | VmAbc_Scripts IO (CPoolHashTable Int32 [ScriptInfo])
                  | VmAbc_MethodBodies IO (CPoolHashTable Int32 [MethodBody])-}

