{-# LANGUAGE ScopedTypeVariables #-}
module Vm.Execute where

import           Abc.DeepSeq
import           Abc.Def
import           Abc.Deserialize
import           Abc.Json
import           Abc.Json2
import           Control.Applicative
import           Control.DeepSeq
import           Data.Int
import           Data.Maybe (listToMaybe)
import           Data.Word
import           Ecma.Prims
import           MonadLib as ML hiding (get, set)
import           Text.JSON
import           TFish
import           Util.Misc
import           Util.Words
import           Vm.Def
import           Vm.Ecma
import           Vm.Lookups
import           Vm.Store
import qualified Control.Monad.State as S
import qualified Data.ByteString as B
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import qualified Data.HashTable.IO as H

p :: String -> AVM3 ()
p = liftIO.putStrLn

returnJ :: a -> AVM3 (Maybe a)
returnJ = return. Just

returnN :: AVM3 (Maybe a)
returnN = return Nothing

avm_prefix :: String
avm_prefix = "avm3internal_"

pfx_class_info_idx :: VmRtP
pfx_class_info_idx = ClassIdx$ avm_prefix ++ "class_info_idx"

test_file = do
  (abc :: Abc) <- E.run_ (EB.enumFile "abc/Test.abc" E.$$ parseAbc)
  --writeFile "abc/Test.abc.json"$ encode$ abcToJson abc
  ht <- H.new
  (either::Either String (), _) <- abc `deepseq`
    (runStateT (ht, [])$ runExceptionT$ execute_abc abc)
  case either of
    Left err -> Prelude.putStrLn$ "EXCEPTION\n" ++ (unlines$ map ("\t"++)$ lines err)
    otherwise -> return ()

execute_abc :: Abc -> AVM3 ()
execute_abc abc = do
  build_cp abc
  cp <- get_cp

  p$ show $ abcStrings abc
  p$ show $ abcNsInfo abc
  --p$ "nssets " ++ (show $ abcNsSet abc)
  p$ show $ abcMultinames abc
  --p$ "\nmethod bodies\n" ++ unlines (map (show.mbCode) $ abcMethodBodies abc)
  --p$ "\nscripts\n" ++ unlines (map show$ abcScripts abc)
  p$ "\nclassinfos\n" ++ unlines (map show$ abcClasses abc)

  ScriptInfo _ ((TraitsInfo _ _ _ (TT_Class (TraitClass _ idx)) _):[]) <- get_script 0
  {-ClassInfo msidx _ <- get_class idx
  MethodSignature returnT _ name _ _ _ <- get_methodSig msidx
  MethodBody _ _ _ _ _ mb_Code _ _ <- get_methodBody msidx-}
  
  p$ "-------------------------------------------"
  global <- liftIO build_global_scope
  let ops = [O$ NewClass idx,O ReturnVoid]
  push_activation (0, ops, [global], [VmRt_Object global])
  vmrt <- r_f
  p$ "-------------------------------------------"

  return ()

show_ops :: String -> Ops -> AVM3 ()
show_ops header ops = do
  case length ops of
    0 -> do
      p$ header ++ " []"
    otherwise -> do
      p$ header
      p$ unlines$ map (\s -> "\t" ++ show s) ops

build_global_scope :: IO VmObject
build_global_scope = do
  (int :: VmObject) <- H.new
  H.insert int (Ext "MAX_VALUE") (VmRt_Int 2147483647)

  (global :: VmObject) <- H.new
  H.insert global (Ext "int") (VmRt_Object int)

  return global

{-
  Since interrupts wait for the stack to wind down I don't have to handle them
  explicitly in run_function
-}

{-
REMEMBER the stack order here is the REVERSE of the docs
  docs
  bottom, mid, top => newvalue

  pattern
  top, mid, bottom => newvalue
-}
r_f :: AVM3 VmRt
r_f = do
  (_, ((sp,ops,ss,reg):_)) <- get

  let (aboveSp, belowSp) = splitAt sp ops
  case take 1 belowSp of
    [] -> set_sp (-1)
    (vmrtOp:[]) -> case vmrtOp of
      (D _) -> mod_sp (+1)
      (O op) -> do
        p "run_function"
        p$ (unlines$ map (\s -> "\t" ++ show s) aboveSp)
          ++ "\t--------------------" ++ show sp ++ "\n"
          ++ (unlines$ map (\s -> "\t" ++ show s) belowSp)
        let ops2 = aboveSp ++ (drop 1 belowSp)
        set_ops ops2
        case op of
    {-1D-}PopScope -> pop_ss
    {-24-}PushByte u8 -> push$ D$ VmRt_Int$ fromIntegral u8
    {-26-}PushTrue -> push$ D$ VmRt_Boolean True
    {-27-}PushFalse -> push$ D$ VmRt_Boolean False
    {-28-}PushNaN -> push$ D$ VmRt_Number nan
    {-29-}Pop -> pop >> return ()
    {-2A-}Dup -> get_ops >>= push . head
    {-2B-}Swap -> do
            a <- pop
            b <- pop
            push a
            push b
    {-2C-}PushString idx -> get_string idx >>= push . D . VmRt_String
    {-2D-}PushInt idx -> get_int idx >>= push . D . VmRt_Int
    {-2E-}PushUInt idx -> get_uint idx >>= push . D . VmRt_Uint
    {-2F-}PushDouble idx -> get_double idx >>= push . D . VmRt_Number
    {-30-}PushScope -> do
            D (VmRt_Object v) <- pop
            push_ss v
    {-47-}ReturnVoid -> set_sp (-1) >> push (D VmRt_Undefined)
    {-48-}ReturnValue -> set_sp (-1)
    {-4F-}CallPropVoid idx args -> do -- TODO check for [ns] [name]
            nArgs <- replicateM (fromIntegral args) pop
            D (VmRt_Object this) <- pop
            
            p$ "CallPropVoid"
            list <- liftIO$ H.toList this
            p$ show list
            
            maybeClassInfoIdx <- liftIO$ H.lookup this pfx_class_info_idx
            TraitsInfo _ _ _ (TT_Method (TraitMethod _ methodId)) _ <- case maybeClassInfoIdx of
              Just (VmRtInternalInt classIdx) -> do
                ClassInfo _ traits <- get_class classIdx
                let matchingTraits = filter (\(TraitsInfo idx2 _ _ _ _) -> idx2 == idx) traits
                p$ "got class info of pfx_class_info_idx" ++ show matchingTraits
                case length matchingTraits of
                  0 -> raise "CallPropVoid - couldn't find matchingTraits"
                  1 -> return$ head matchingTraits
                  otherwise -> raise "CallPropVoid - too many matching traits"
              otherwise -> do
                name <- resolve_multiname idx
                proplist <- liftIO$ H.toList this
                raise$ "couldn't find " ++ name ++ " on " ++ show proplist
            
            MethodBody _ _ _ _ _ code _ _ <- get_methodBody methodId
            p$ show code
            push_activation (0, map O code, [last ss], [VmRt_Object this])
            r_f
            pop_activation

            return ()
    {-56-}NewArray args -> do
            nArgs <- replicateM (fromIntegral args) pop
            push$ D$ VmRt_Array$ map (\(D a) -> a)$ reverse nArgs
    {-58-}NewClass idx -> do
            p$ "########## NewClass " ++ show idx
            ClassInfo msi traits <- get_class idx
            
            MethodBody _ _ _ _ _ code _ _ <- get_methodBody msi
            --p$ "\tops: " ++ show code
            
            (klass :: VmObject) <- liftIO$ H.new
            -- store this class's identity in it
            liftIO$ H.insert klass pfx_class_info_idx (VmRtInternalInt idx)
            
            let global = last ss
            push_activation (0, map O code, [global], [VmRt_Object klass])
            r_f
            pop_activation

            liftIO$ H.insert global (Ext "Test")$ VmRt_Object klass
            p$ "########## " ++ show idx
    {-5D-}FindPropStrict idx -> find_property idx ss >>= push . D -- TODO this need error checking
    {-5E-}FindProperty idx -> find_property idx ss >>= push . D -- TODO this need error checking
    {-60-}GetLex idx -> do
            push$ O$ GetProperty idx
            push$ O$ FindPropStrict idx
    {-65-}GetScopeObject idx -> push$ D$ VmRt_Object$ ss !! fromIntegral idx 
    {-68-}InitProperty idx -> do
            (D vmrt) <- pop
            (D (VmRt_Object this)) <- pop
            init_property this idx vmrt
    {-70-}ConvertString -> do
            (D v) <- pop
            push$ convert_string v
    {-73-}ConvertInt -> do
            (D v) <- pop
            push$ convert_int v
    {-74-}ConvertUInt -> do
            (D v) <- pop
            push$ convert_uint v
    {-75-}ConvertDouble -> do
            (D v) <- pop
            push$ convert_double v
    {-76-}ConvertBoolean -> do
            (D v) <- pop
            push$ convert_boolean v
    {-77-}ConvertObject -> ML.raise "NI: ConvertObject"
    {-D0-}GetLocal0 -> push$ D$ reg !! 0
    {-D1-}GetLocal1 -> push$ D$ reg !! 1
    {-D2-}GetLocal2 -> push$ D$ reg !! 2
    {-D3-}GetLocal3 -> push$ D$ reg !! 3
    {-D4-}SetLocal0 -> do
            D new <- pop
            mod_reg$ \reg -> let (h,(_:t)) = splitAt 0 reg in h ++ (new:t)
    {-D5-}SetLocal1 -> do
            D new <- pop
            mod_reg$ \reg -> let (h,(_:t)) = splitAt 1 reg in h ++ (new:t)
    {-D6-}SetLocal2 -> do
            D new <- pop
            mod_reg$ \reg -> let (h,(_:t)) = splitAt 2 reg in h ++ (new:t)
    {-D7-}SetLocal3 -> do
            D new <- pop
            mod_reg$ \reg -> let (h,(_:t)) = splitAt 3 reg in h ++ (new:t)
          _ -> mod_sp (+1)
  
  (_, ((newsp,newops,_,_):_)) <- get
  case newsp of
    -1 -> let (D ret:_) = newops in return ret
    otherwise -> r_f

convert_string :: VmRt -> VmRtOp
convert_string = D . VmRt_String . to_string

convert_double :: VmRt -> VmRtOp
convert_double = D . VmRt_Number . to_number

convert_boolean :: VmRt -> VmRtOp
convert_boolean = D . VmRt_Boolean . to_boolean

convert_int :: VmRt -> VmRtOp
convert_int = D . VmRt_Int . to_int32

convert_uint :: VmRt -> VmRtOp
convert_uint = D . VmRt_Uint . to_uint32

new_function :: AVM3 ()
new_function = undefined

{- TODO dynamic multinames (pattern match Ops) -}
{-
TODO resolve against
  1. method closures
  2. declared traits (on the method body, instance info?, class info?)
  3. dynamic properties
  4. prototype chain
  5. script traits (global object (for classes))
-}
find_property :: MultinameIdx -> ScopeStack -> AVM3 VmRt
find_property idx ss = do
  name <- resolve_multiname idx
  p$ "FindProperty [" ++ show idx ++ " " ++ name ++ "]"
  --p$ "\tscope_stack length " ++ (show$ length ss)
  attempt1 <- search_stack (Ext name) ss
  case attempt1 of
    Just obj -> do
      --p$ "found obj in attempt1"
      return obj
    Nothing -> do
      attempt2 <- traits_ref idx ss
      case attempt2 of
        Just obj -> do
          --p$ "found obj in attempt2"
          return obj
        Nothing -> raise "find_property - couldn't find_property"
  where
    search_stack :: VmRtP -> ScopeStack -> AVM3 (Maybe VmRt)
    search_stack key [] = return Nothing
    search_stack key@(Ext str) (top:stack) = do
      if str == "Foo"
        then do
          list <- liftIO$ H.toList top
          p$ show list
          p$ "key " ++ show key
        else return ()
      maybeValue <- liftIO$ H.lookup top key
      case maybeValue of
        Nothing -> search_stack key stack
        Just value -> returnJ$ VmRt_Object top

    traits_ref :: MultinameIdx -> ScopeStack -> AVM3 (Maybe VmRt)
    traits_ref idx [] = return Nothing
    traits_ref idx (top:stack) = do
      maybeClassInfoIdx <- liftIO$ H.lookup top pfx_class_info_idx
      case maybeClassInfoIdx of
        Just (VmRtInternalInt classIdx) -> do
          ClassInfo _ traits <- get_class classIdx
          let matchingTraits = filter (\(TraitsInfo idx2 _ _ _ _) -> idx2 == idx) traits
          --p$ "got class info of pfx_class_info_idx" ++ show matchingTraits
          case length matchingTraits of
            0 -> return Nothing
            1 -> returnJ$ VmRt_Object top
            otherwise -> raise "traits_ref - too many matching traits"
        otherwise -> traits_ref idx stack

{-
  "The indexing of elements on the local scope stack is the reverse of the
  indexing of elements on the local operand stack."
-}

{-get_scoped_object :: ScopeStack -> U8 -> AVM3 Ops
get_scoped_object ss idx = undefined-}

init_property :: VmObject -> MultinameIdx -> VmRt -> AVM3 ()
init_property this idx value = do
  name <- resolve_multiname idx
  p$ "InitProperty [" ++ show idx ++ " " ++ name ++ "]"
  maybeProp <- liftIO$ H.lookup this (Ext name)
  case maybeProp of
    Nothing -> do
      liftIO$ H.insert this (Ext name) value
      return ()
    otherwise -> raise$ "init_property - property [" ++ name ++ "] exists"






















