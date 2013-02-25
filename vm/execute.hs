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
import           MonadLib as ML hiding (get, set, jump)
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

insert :: VmObject -> VmRtP -> VmRt -> AVM3 ()
insert h k v = liftIO$ H.insert h k v

pfx_class_info_idx :: VmRtP
pfx_class_info_idx = ClassIdx$ avm_prefix ++ "class_info_idx"

new_object :: AVM3 (VmObject, InstanceId)
new_object = do
  iid <- next_iid
  obj <- liftIO$ H.new
  return (obj, iid)

test_file = do
  (abc :: Abc) <- E.run_ (EB.enumFile "abc/Test.abc" E.$$ parseAbc)
  --writeFile "abc/Test.abc.json"$ encode$ abcToJson abc
  ht <- H.new
  (either::Either String (), _) <- abc `deepseq`
    (runStateT (ht, [], 0)$ runExceptionT$ execute_abc abc)
  case either of
    Left err -> Prelude.putStrLn$ "EXCEPTION\n" ++ (unlines$ map ("\t"++)$ lines err)
    otherwise -> return ()

execute_abc :: Abc -> AVM3 ()
execute_abc abc = do
  build_cp abc
  cp <- get_cp
  ScriptInfo _ ((TraitsInfo _ _ _ (TT_Class (TraitClass _ idx)) _):[]) <- get_script 0
  p$ "-------------------------------------------"
  (global, globalid) <- build_global_scope
  let ops = [O$ NewClass idx,O ReturnVoid]
  iid <- next_iid
  push_activation (0, ops, [(global, globalid)], [VmRt_Object global iid])
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

build_global_scope :: AVM3 (VmObject, InstanceId)
build_global_scope = do
  (int :: VmObject, iid) <- new_object
  insert int (Ext "MAX_VALUE") (VmRt_Int 2147483647)

  (global :: VmObject, globalid) <- new_object
  insert global (Ext "int") (VmRt_Object int iid)

  return (global, globalid)

convert_offset :: [VmRtOp] -> S24 -> Int
--convert_offset s24 (op:[]) = toBytes op
convert_offset ((O op):ops) s24
  | s24 > 0 = 1 + (convert_offset ops$ s24 - (fromIntegral$ toBytes op))
  | otherwise = 1

push_undefined :: AVM3 ()
push_undefined = pushd VmRt_Undefined

{-
  prone to off by one errors.
  this must be called after the sp is modified in r_f
-}
jump :: S24 -> AVM3 ()
jump s24 = do
  (_, ((sp,ops,_,_):_), _) <- get
  let (aboveSp, belowSp) = splitAt sp ops
  if s24 > 0
    then do
      let offset = convert_offset (drop 1 belowSp) s24 - 1
      mod_sp ((+)offset)
    else do
      let offset = convert_offset (drop 1$ reverse aboveSp) (abs s24) - 1
      mod_sp ((+) (negate offset))

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
  (_, ((sp,ops,ss,reg):_), _) <- get

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
        mod_sp (+1)
        case op of
    {-09-}Label -> return ()
    {-10-}Jump s24 -> jump s24
    {-11-}IfTrue s24 -> do
            D a <- pop
            if to_boolean a == True
              then jump s24
              else return ()
    {-12-}IfFalse s24 -> do
            D a <- pop
            if to_boolean a == False
              then jump s24
              else return ()
    {-13-}IfEqual s24 -> do
            D a <- pop
            D b <- pop
            if b == a
              then jump s24
              else return ()
    {-14-}IfNotEqual s24 -> do
            D a <- pop
            D b <- pop
            if b /= a
              then jump s24
              else return ()
    {-15-}IfLessThan s24 -> do
            D a <- pop
            D b <- pop
            if b < a
              then jump s24
              else return ()
    {-16-}IfLessEqual s24 -> do
            D a <- pop
            D b <- pop
            if b <= a
              then jump s24
              else return ()
    {-17-}IfGreaterThan s24 -> do
            D a <- pop
            D b <- pop
            if b > a
              then jump s24
              else return ()
    {-18-}IfGreaterEqual s24 -> do
            D a <- pop
            D b <- pop
            if b >= a
              then jump s24
              else return ()
    {-19-}IfStrictEqual s24 -> do
            D a <- pop
            D b <- pop
            if a == b -- TODO class StrictEq ===
              then jump s24
              else return ()
    {-1A-}IfStrictNotEqual s24 -> do
            D a <- pop
            D b <- pop
            if a /= b -- TODO class StrictEq /==
              then jump s24
              else return ()
    {-1D-}PopScope -> pop_ss
    {-24-}PushByte u8 -> pushd$ VmRt_Int$ fromIntegral u8
    {-26-}PushTrue -> pushd$ VmRt_Boolean True
    {-27-}PushFalse -> pushd$ VmRt_Boolean False
    {-28-}PushNaN -> pushd$ VmRt_Number nan
    {-29-}Pop -> pop >> return ()
    {-2A-}Dup -> do a <- pop; push a; push a
    {-2B-}Swap -> do
            a <- pop
            b <- pop
            push a
            push b
    {-2C-}PushString idx -> get_string idx >>= pushd . VmRt_String
    {-2D-}PushInt idx -> get_int idx >>= pushd . VmRt_Int
    {-2E-}PushUInt idx -> get_uint idx >>= pushd . VmRt_Uint
    {-2F-}PushDouble idx -> get_double idx >>= pushd . VmRt_Number
    {-30-}PushScope -> do
            D (VmRt_Object v iid) <- pop
            push_ss (v,iid)
    {-47-}ReturnVoid -> push_undefined >> set_sp (-1)
    {-48-}ReturnValue -> set_sp (-1)
    {-4F-}CallPropVoid idx args -> do -- TODO check for [ns] [name]
            nArgs <- replicateM (fromIntegral args) pop
            D (VmRt_Object this iid) <- pop
            
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
                Multiname_QName nSInfoIdx stringIdx <- get_multiname idx
                name <- liftM2 (++) (resolve_nsinfo nSInfoIdx) (get_string stringIdx)
                proplist <- liftIO$ H.toList this
                raise$ "couldn't find " ++ name ++ " on " ++ show proplist
            
            MethodBody _ _ _ _ _ code _ _ <- get_methodBody methodId
            p$ show code
            push_activation (0, map O code, [last ss], [VmRt_Object this iid] ++ (map (\(D a) -> a) nArgs))
            r_f
            pop_activation

            return ()
    {-55-}NewObject args -> do
            (obj, iid) <- new_object
            nArgs <- replicateM (fromIntegral args) (liftM2 (,) pop pop)
            forM_ nArgs$ \(D v,D (VmRt_String k)) -> insert obj (Ext k) v
            pushd$ VmRt_Object obj iid
    {-56-}NewArray args -> do
            nArgs <- replicateM (fromIntegral args) pop
            iid <- next_iid
            pushd$ VmRt_Array (map (\(D a) -> a)$ reverse nArgs) iid
    {-58-}NewClass idx -> do
            p$ "########## NewClass " ++ show idx
            ClassInfo msi traits <- get_class idx
            
            MethodBody _ _ _ _ _ code _ _ <- get_methodBody msi
            --p$ "\tops: " ++ show code
            
            (klass, iid) <- new_object
            -- store this class's identity in it
            insert klass pfx_class_info_idx (VmRtInternalInt idx)
            
            let (global, globalid) = last ss
            push_activation (0, map O code, [(global, globalid)], [VmRt_Object klass iid])
            r_f
            pop_activation

            insert global (Ext "Test")$ VmRt_Object klass iid
            p$ "########## " ++ show idx
    {-5D-}FindPropStrict idx -> find_property idx ss >>= pushd -- TODO this need error checking
    {-5E-}FindProperty idx -> find_property idx ss >>= pushd -- TODO this need error checking
    {-60-}GetLex idx -> raise "GetLex should have been replaced"
    {-65-}GetScopeObject idx -> do
            let (obj, iid) = ss !! fromIntegral idx 
            pushd$ VmRt_Object obj iid
    {-66-}GetProperty idx -> do
            multiname <- get_multiname idx
            prop <- case multiname of
              Multiname_QName nSInfoIdx stringIdx -> do
                name <- liftM2 (++) (resolve_nsinfo nSInfoIdx) (get_string stringIdx)
                return$ VmRt_String name
              otherwise -> do (D d) <- pop; return d

            D vmrt <- pop

            case vmrt of
              VmRt_Undefined -> push_undefined
              VmRt_Null -> push_undefined
              VmRt_Boolean bool -> push_undefined
              VmRt_Int v -> push_undefined
              VmRt_Uint v -> push_undefined
              VmRt_Number v -> push_undefined
              VmRt_String v -> push_undefined
              VmRt_Array v _ -> case prop of
                VmRt_String name -> case name of
                  "length" -> pushd . VmRt_Int . fromIntegral$ length v
                  otherwise -> raise$ "GetProperty - VmRt_Array - can't get property " ++ name
                VmRt_Int i -> pushd$ v !! fromIntegral i
              VmRt_Object this _ -> case prop of
                VmRt_String name -> do
                  maybeProp <- liftIO$ H.lookup this$ Ext name
                  case maybeProp of
                    Just p -> pushd p
                    Nothing -> push_undefined
                otherwise -> raise "GetProperty - VmRt_Object - only strings"
              --VmRt_Closure f
    {-68-}InitProperty idx -> do
            D vmrt <- pop
            D (VmRt_Object this iid) <- pop
            init_property this idx vmrt
    {-70-}ConvertString -> do
            D v <- pop
            pushd$ convert_string v
    {-73-}ConvertInt -> do
            D v <- pop
            pushd$ convert_int v
    {-74-}ConvertUInt -> do
            D v <- pop
            pushd$ convert_uint v
    {-75-}ConvertDouble -> do
            D v <- pop
            pushd$ convert_double v
    {-76-}ConvertBoolean -> do
            D v <- pop
            pushd$ convert_boolean v
    {-77-}ConvertObject -> ML.raise "NI: ConvertObject"
    {-A0-}Add -> do
            D a <- pop
            D b <- pop
            pushd$ b + a
    {-A1-}Subtract -> do
            D a <- pop
            D b <- pop
            pushd$ b - a
    {-A2-}Multiply -> do
            D a <- pop
            D b <- pop
            pushd$ b * a
    {-A3-}Divide -> do
            D a <- pop
            D b <- pop
            pushd$ b / a
    {-C0-}IncrementInt -> do
            D a <- pop
            pushd$ a+1
    {-C1-}DecrementInt -> do
            D a <- pop
            pushd$ a-1
    {-C2-}IncLocalInt regIdx -> do
            let new = (reg !! fromIntegral regIdx) + 1
            mod_reg$ \reg -> let (h,(_:t)) = splitAt (fromIntegral regIdx) reg in h ++ (new:t)
    {-C3-}DecLocalInt regIdx -> do
            let new = (reg !! fromIntegral regIdx) - 1
            mod_reg$ \reg -> let (h,(_:t)) = splitAt (fromIntegral regIdx) reg in h ++ (new:t)
    {-D0-}GetLocal0 -> pushd$ reg !! 0
    {-D1-}GetLocal1 -> pushd$ reg !! 1
    {-D2-}GetLocal2 -> pushd$ reg !! 2
    {-D3-}GetLocal3 -> pushd$ reg !! 3
    {-D4-}SetLocal0 -> do
            D new <- pop
            mod_reg$ \reg -> let (h,(_:t)) = splitAt 0 reg in h ++ (new:t)
    {-D5-}SetLocal1 -> do
            D new <- pop
            case reg of
              [] -> mod_reg$ \_ -> [VmRt_Undefined, new]
              (_:[]) -> mod_reg (++[new])
              otherwise -> mod_reg$ \reg -> let (h,(_:t)) = splitAt 1 reg in h ++ (new:t)
    {-D6-}SetLocal2 -> do
            D new <- pop
            case reg of
              [] -> mod_reg$ \_ -> [VmRt_Undefined, VmRt_Undefined, new]
              (_:[]) -> mod_reg$ \(a:[]) -> [a, VmRt_Undefined, new]
              (_:_:[]) -> mod_reg (++[new])
              otherwise -> mod_reg$ \reg -> let (h,(_:t)) = splitAt 2 reg in h ++ (new:t)
            --mod_reg$ \reg -> let (h,(_:t)) = splitAt 2 reg in h ++ (new:t)
    {-D7-}SetLocal3 -> do
            D new <- pop
            case reg of
              [] -> mod_reg$ \_ -> [VmRt_Undefined, VmRt_Undefined, VmRt_Undefined, new]
              (_:[]) -> mod_reg$ \(a:[]) -> [a, VmRt_Undefined, VmRt_Undefined, new]
              (_:_:[]) -> mod_reg$ \(a:b:[]) -> [a, b, VmRt_Undefined, new]
              (_:_:_:[]) -> mod_reg (++[new])
              otherwise -> mod_reg$ \reg -> let (h,(_:t)) = splitAt 3 reg in h ++ (new:t)
            --mod_reg$ \reg -> let (h,(_:t)) = splitAt 3 reg in h ++ (new:t)
          _ -> raise$ "didn't match opcode " ++ show op
  
  (_, ((newsp,newops,_,_):_), _) <- get
  case newsp of
    -1 -> let (D ret:_) = newops in return ret
    otherwise -> r_f

convert_string :: VmRt -> VmRt
convert_string = VmRt_String . to_string

convert_double :: VmRt -> VmRt
convert_double = VmRt_Number . to_number

convert_boolean :: VmRt -> VmRt
convert_boolean = VmRt_Boolean . to_boolean

convert_int :: VmRt -> VmRt
convert_int = VmRt_Int . to_int32

convert_uint :: VmRt -> VmRt
convert_uint = VmRt_Uint . to_uint32

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
  Multiname_QName nSInfoIdx stringIdx <- get_multiname idx
  name <- liftM2 (++) (resolve_nsinfo nSInfoIdx) (get_string stringIdx)
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
    search_stack key@(Ext str) ((top, iid):stack) = do
      if str == "Foo"
        then do
          list <- liftIO$ H.toList top
          p$ show list
          p$ "key " ++ show key
        else return ()
      maybeValue <- liftIO$ H.lookup top key
      case maybeValue of
        Nothing -> search_stack key stack
        Just value -> returnJ$ VmRt_Object top iid

    traits_ref :: MultinameIdx -> ScopeStack -> AVM3 (Maybe VmRt)
    traits_ref idx [] = return Nothing
    traits_ref idx ((top, iid):stack) = do
      maybeClassInfoIdx <- liftIO$ H.lookup top pfx_class_info_idx
      case maybeClassInfoIdx of
        Just (VmRtInternalInt classIdx) -> do
          ClassInfo _ traits <- get_class classIdx
          let matchingTraits = filter (\(TraitsInfo idx2 _ _ _ _) -> idx2 == idx) traits
          --p$ "got class info of pfx_class_info_idx" ++ show matchingTraits
          case length matchingTraits of
            0 -> return Nothing
            1 -> returnJ$ VmRt_Object top iid
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
  Multiname_QName nSInfoIdx stringIdx <- get_multiname idx
  name <- liftM2 (++) (resolve_nsinfo nSInfoIdx) (get_string stringIdx)
  p$ "InitProperty [" ++ show idx ++ " " ++ name ++ "]"
  maybeProp <- liftIO$ H.lookup this (Ext name)
  case maybeProp of
    Nothing -> do
      insert this (Ext name) value
      return ()
    otherwise -> raise$ "init_property - property [" ++ name ++ "] exists"






















