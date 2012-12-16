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

--newtype Execution = Execution { ex :: (ScopeStack, Registers, ConstantPool) }
newtype Execution = Execution { env :: ([Int], Registers, IO ConstantPool) }--TEMP

{-
  "The indexing of elements on the local scope stack is the reverse of the
  indexing of elements on the local operand stack."
-}

data VmMethod = VmMethod {
                         -- maybe body
                           vmm_mbody :: Maybe MethodBody
                         -- method body
                         {-, vmm_method :: MethodSignatureIdx
                         , vmm_maxStack :: U30
                         , vmm_localCount :: U30
                         , vmm_initScopeDepth :: U30
                         , vmm_maxScopeDepth :: U30
                         , vmm_code :: [OpCode]
                         , vmm_exceptions :: [Exception]
                         , vmm_traits :: [TraitsInfo]-}
                         -- method signature
                         , vmm_returnType :: MultinameIdx
                         , vmm_paramTypes :: [U30]
                         , vmm_methodName :: StringIdx
                         , vmm_flags :: Word8
                         , vmm_optionInfo :: Maybe [CPC]
                         , vmm_paramNames :: Maybe [U30]
                         }
                         deriving (Show)

type VmObject = H.BasicHashTable String VmRt
--type ScopeStack = [H.BasicHashTable Multiname VmObject]
--type ScopeStack = [H.BasicHashTable String VmObject]
type ScopeStack = [VmObject]

type Registers = [VmRt]
type Ops = [VmRtOps]
data VmRtOps = O OpCode
             | D VmRt

data VmRt = VmRt_Undefined
          | VmRt_Null
          | VmRt_Int Int32
          | VmRt_Uint U30
          | VmRt_Double Double
          | VmRt_String String
          | VmRt_Object VmObject

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
