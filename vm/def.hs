module Vm.Def where

import           Abc.Def
import           Data.Hashable
import           Data.Int
import           Data.Word
import           Util.Misc
import qualified Data.HashTable.IO as H
import qualified MonadLib as ML

type ConstantPool = H.BasicHashTable String VmAbc

type StackIndex = Int
type Execution2 = (ConstantPool, [[(StackIndex, Ops)]])

type Execution = (ConstantPool, Ops)
type AVM3_State = ML.StateT Execution IO
type AVM3 = ML.ExceptionT String AVM3_State

type VmObject = H.BasicHashTable VmRtP VmRt
type ScopeStack = [VmObject]
type Registers = [VmRt]
type Ops = [VmRtOp]

data VmRtP = Ext String -- all run time, external properties
           | ClassIdx String -- the identity of the class
           deriving (Eq, Show)

instance Hashable VmRtP where
  hashWithSalt salt (Ext a) = hashWithSalt salt$ "Ext" ++ a
  hashWithSalt salt (ClassIdx a) = hashWithSalt salt$ "ClassIdx" ++ a

data VmCont = NoMatch
            | Yield VmRt
            | OpsMod (U30, (Ops -> Ops))
            | OpsModR (Registers -> Ops -> Ops)
            | OpsModS (ScopeStack -> Ops -> Ops)
            -- | OpsModM (ScopeStack -> AVM3 VmCont) -- maybe works for find_property, new_class
            | RegMod (Registers -> Registers)
            | StackMod (ScopeStack -> ScopeStack)
            | FindProp MultinameIdx
            | InitProp MultinameIdx VmObject VmRt
            | NewClassC MultinameIdx

data VmRtOp = O OpCode
            | D VmRt
            deriving (Show)

{-
  Seriously considering a different representation of VmRt_Object to enable
  pure functions outside of monads in order to keep Emca.Prims classes simple
  (and implementable). Part of the entire reason to use haskell is to find the
  path of least resistance toward correctness. If i'm resorting to a series of
  string lookups I'm hardly leveraging the type system anymore

  What keeps bringing me back to the hashtable is all the name resolution I have
  to do

  The lookups are primarily on the ScopeStack (i.e. findprop). Could I do a
  transformation of ScopeStack VmObjects to VmRt_Objects of a pure type on
  which to operate?
-}

{-
TODO
 1) the primitives are objects but since they have a fixed set of immutable
    methods I can pattern match for things like callproperty along the lines of
    (D vmrt:O CallProperty):ops -> custom_method
 2) method closures
      Maybe ScopeStack ?
      (Registers -> Ops -> AVM3 VmRt) ?
 3) a heap: type RefCount = Int ?
 4) space/time: eagerly resolve all strings, merge method info, etc
 5) move Traits off of scripts and onto the global object for find_property
-}
data VmRt = VmRt_Undefined
          | VmRt_Null
          | VmRt_Boolean Bool
          | VmRt_Int Int32
          | VmRt_Uint Word32
          | VmRt_Number Double
          | VmRt_String String
          | VmRt_Array [VmRt]
          | VmRt_Object VmObject {-RefCount-} {-(Maybe ScopeStack)-}
          | VmRt_Closure (Registers -> Ops -> AVM3 VmRt) -- curried r_f
          | VmRtInternalInt U30

instance Show VmRt where
  show VmRt_Undefined   = "VmRt_Undefined"
  show VmRt_Null        = "VmRt_Null"
  show (VmRt_Boolean a) = "VmRt_Boolean " ++ show a
  show (VmRt_Int a)     = "VmRt_Int " ++ show a
  show (VmRt_Uint a)    = "VmRt_Uint " ++ show a
  show (VmRt_Number a)  = "VmRt_Number " ++ show a
  show (VmRt_String a)  = "VmRt_String " ++ show a
  show (VmRt_Array a)   = "VmRt_Array " ++ show a
  show (VmRt_Object a)  = "VmRt_Object [Object]"
  show (VmRt_Closure _) = "VmRt_Closure"
  show (VmRtInternalInt a) = "VmRtInternalInt " ++ show a

{- 1:1 transformation of Abc to an ADT -}
data VmAbc = VmAbc_Int Int32
           | VmAbc_Uint U30
           | VmAbc_Double Double
           | VmAbc_String String
           | VmAbc_NsInfo NSInfo
           | VmAbc_NsSet NSSet
           | VmAbc_Multiname Multiname
           | VmAbc_MethodSig MethodSignature {- Maybe MethodBody -}
           | VmAbc_Metadata Metadata
           | VmAbc_Instance InstanceInfo
           | VmAbc_Class ClassInfo
           | VmAbc_Script ScriptInfo
           | VmAbc_MethodBody MethodBody
           deriving (Show)

liftIO :: IO a -> AVM3 a
liftIO = ML.lift . ML.lift

{-
  Monad helpers
-}

get :: AVM3 Execution
get = ML.lift$ ML.get

set :: Execution -> AVM3 ()
set = ML.lift . ML.set

get_cp :: AVM3 ConstantPool
get_cp = get >>= return. t21

mod_cp :: (ConstantPool -> ConstantPool) -> AVM3 ()
mod_cp f = get_cp >>= return.f >>= set_cp

set_cp :: ConstantPool -> AVM3 ()
set_cp cp = do
  (_,ops) <- get
  set (cp,ops)

get_ops :: AVM3 Ops
get_ops = get >>= return. t22

mod_ops :: (Ops -> Ops) -> AVM3 ()
mod_ops f = get_ops >>= return.f >>= set_ops

set_ops :: Ops -> AVM3 ()
set_ops ops = do
  (cp,_) <- get
  set (cp,ops)

{-
  VmCont helpers
-}

yield :: Ops -> VmRt -> AVM3 (VmCont, Ops)
yield ops c = return$ (Yield c, ops)

ops_mod :: Ops -> (Ops -> Ops) -> AVM3 (VmCont, Ops)
ops_mod ops c = return$ (OpsMod (0, c), ops)

cons_vmrt :: Ops -> VmRt -> AVM3 (VmCont, Ops)
cons_vmrt ops = return . flip (,) ops . OpsMod . (,) 0 . (:) . D

ops_modR :: Ops -> (Registers -> Ops -> Ops) -> AVM3 (VmCont, Ops)
ops_modR ops c = return$ (OpsModR c, ops)

ops_modS :: Ops -> (ScopeStack -> Ops -> Ops) -> AVM3 (VmCont, Ops)
ops_modS ops c = return$ (OpsModS c, ops)

reg_mod :: Ops -> (Registers -> Registers) -> AVM3 (VmCont, Ops)
reg_mod ops c = return$ (RegMod c, ops)

mod_ss :: Ops -> (ScopeStack -> ScopeStack) -> AVM3 (VmCont, Ops)
mod_ss ops c = return$ (StackMod c, ops)













