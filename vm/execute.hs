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
import           Data.Time.Clock
import           Data.Word
import           Ecma.Prims
import           MonadLib as ML hiding (get, set, jump)
import           Prelude hiding (lookup)
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
--p = liftIO.putStrLn
p s = return ()

returnJ :: a -> AVM3 (Maybe a)
returnJ = return. Just

returnN :: AVM3 (Maybe a)
returnN = return Nothing

avm_prefix :: String
avm_prefix = "avm3internal_"

insert :: VmObject -> VmRtP -> VmRt -> AVM3 ()
insert h k v = liftIO$ H.insert h k v

lookup :: VmObject -> VmRtP -> AVM3 (Maybe VmRt)
lookup h k = liftIO$ H.lookup h k

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
  t0 <- liftIO$ getCurrentTime
  build_cp abc
  cp <- get_cp
  ScriptInfo _ ((TraitsInfo _ _ _ (TT_Class (TraitClass _ idx)) _):[]) <- get_script 0
  p$ "-------------------------------------------"
  (global, globalid) <- build_global_scope
  let ops = [O$ NewClass idx,O ReturnVoid]
  iid <- next_iid
  (registers :: Registers) <- liftIO$ H.new
  liftIO$ H.insert registers 0 $ VmRt_Object global iid
  push_activation (0, ops, [(global, globalid)], registers)
  vmrt <- r_f
  p$ "-------------------------------------------"
  t1 <- liftIO$ getCurrentTime
  liftIO.putStrLn$ show$ diffUTCTime t1 t0
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
  {-0x08-}Kill regIdx -> liftIO$ H.insert reg (fromIntegral regIdx) VmRt_Undefined
  {-0x09-}Label -> return ()
  {-0x10-}Jump s24 -> jump s24
  {-0x11-}IfTrue s24 -> do
            D a <- pop
            if to_boolean a == True
              then jump s24
              else return ()
  {-0x12-}IfFalse s24 -> do
            D a <- pop
            if to_boolean a == False
              then jump s24
              else return ()
  {-0x13-}IfEqual s24 -> do
            D a <- pop
            D b <- pop
            if b == a
              then jump s24
              else return ()
  {-0x14-}IfNotEqual s24 -> do
            D a <- pop
            D b <- pop
            if b /= a
              then jump s24
              else return ()
  {-0x15-}IfLessThan s24 -> do
            D a <- pop
            D b <- pop
            if b < a
              then jump s24
              else return ()
  {-0x16-}IfLessEqual s24 -> do
            D a <- pop
            D b <- pop
            if b <= a
              then jump s24
              else return ()
  {-0x17-}IfGreaterThan s24 -> do
            D a <- pop
            D b <- pop
            if b > a
              then jump s24
              else return ()
  {-0x18-}IfGreaterEqual s24 -> do
            D a <- pop
            D b <- pop
            if b >= a
              then jump s24
              else return ()
  {-0x19-}IfStrictEqual s24 -> do
            D a <- pop
            D b <- pop
            if a == b -- TODO class StrictEq ===
              then jump s24
              else return ()
  {-0x1A-}IfStrictNotEqual s24 -> do
            D a <- pop
            D b <- pop
            if a /= b -- TODO class StrictEq /==
              then jump s24
              else return ()
  {-0x1D-}PopScope -> pop_ss
  {-0x24-}PushByte u8 -> pushd$ VmRt_Int$ fromIntegral u8
  {-0x25-}PushShort u30 -> pushd$ VmRt_Uint u30
  {-0x26-}PushTrue -> pushd$ VmRt_Boolean True
  {-0x27-}PushFalse -> pushd$ VmRt_Boolean False
  {-0x28-}PushNaN -> pushd$ VmRt_Number nan
  {-0x29-}Pop -> pop >> return ()
  {-0x2A-}Dup -> do a <- pop; push a; push a
  {-0x2B-}Swap -> do
            a <- pop
            b <- pop
            push a
            push b
  {-0x2C-}PushString idx -> get_string idx >>= pushd . VmRt_String
  {-0x2D-}PushInt idx -> get_int idx >>= pushd . VmRt_Int
  {-0x2E-}PushUInt idx -> get_uint idx >>= pushd . VmRt_Uint
  {-0x2F-}PushDouble idx -> get_double idx >>= pushd . VmRt_Number
  {-0x30-}PushScope -> do
            D (VmRt_Object v iid) <- pop
            push_ss (v,iid)
  {-0x47-}ReturnVoid -> push_undefined >> set_sp (-1)
  {-0x48-}ReturnValue -> set_sp (-1)
  {-0x4F-}CallPropVoid idx args -> do -- TODO check for [ns] [name]
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
            (registers :: Registers) <- liftIO$ H.new
            liftIO$ H.insert registers 0 $ VmRt_Object this iid
            forM_ (zip [1..length nArgs] nArgs) (\(i,D arg) -> liftIO$ H.insert registers i arg)
            push_activation (0, map O code, [last ss], registers)
            r_f
            pop_activation

            return ()
  {-0x55-}NewObject args -> do
            (obj, iid) <- new_object
            nArgs <- replicateM (fromIntegral args) (liftM2 (,) pop pop)
            forM_ nArgs$ \(D v,D (VmRt_String k)) -> insert obj (Ext k) v
            pushd$ VmRt_Object obj iid
  {-0x56-}NewArray args -> do
            nArgs <- replicateM (fromIntegral args) pop
            iid <- next_iid
            pushd$ VmRt_Array (map (\(D a) -> a)$ reverse nArgs) iid
  {-0x58-}NewClass idx -> do
            p$ "########## NewClass " ++ show idx
            ClassInfo msi traits <- get_class idx
            
            MethodBody _ _ _ _ _ code _ _ <- get_methodBody msi
            --p$ "\tops: " ++ show code
            
            (klass, iid) <- new_object
            -- store this class's identity in it
            insert klass pfx_class_info_idx (VmRtInternalInt idx)
            
            let (global, globalid) = last ss
            (registers :: Registers) <- liftIO$ H.new
            liftIO$ H.insert registers 0 $ VmRt_Object klass iid
            push_activation (0, map O code, [(global, globalid)], registers)
            r_f
            pop_activation

            insert global (Ext "Test")$ VmRt_Object klass iid
            p$ "########## " ++ show idx
  {-0x5D-}FindPropStrict idx -> find_property idx ss >>= pushd -- TODO this need error checking
  {-0x5E-}FindProperty idx -> find_property idx ss >>= pushd -- TODO this need error checking
  {-0x60-}GetLex idx -> raise "GetLex should have been replaced"
  {-0x61-}SetProperty idx -> do
            D value <- pop
            
            multiname <- get_multiname idx
            (VmRt_String prop) <- case multiname of
              Multiname_QName _ stringIdx -> liftM VmRt_String$ get_string stringIdx
              Multiname_Multiname stringIdx _ -> liftM VmRt_String$ get_string stringIdx
              otherwise -> do (D d) <- pop; return d
            
            D (VmRt_Object this _) <- pop
            insert this (Ext prop) value
  {-0x62-}GetLocal u30 -> do
            maybeR <- liftIO$ H.lookup reg (fromIntegral u30)
            case maybeR of
              Nothing -> raise$ "GetLocal" ++ show u30 ++ " - register doesn't exist"
              Just r -> pushd r
  {-0x63-}SetLocal u30 -> do
            D new <- pop
            liftIO$ H.insert reg (fromIntegral u30) new
  {-0x65-}GetScopeObject idx -> do
            let (obj, iid) = ss !! fromIntegral idx 
            pushd$ VmRt_Object obj iid
  {-0x66-}GetProperty idx -> do
            multiname <- get_multiname idx
            prop <- case multiname of
              Multiname_QName _ stringIdx -> liftM VmRt_String$ get_string stringIdx
              Multiname_Multiname stringIdx _ -> liftM VmRt_String$ get_string stringIdx
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
  {-0x68-}InitProperty idx -> do
            D vmrt <- pop
            D (VmRt_Object this iid) <- pop
            init_property this idx vmrt
  {-0x70-}ConvertString -> do
            D v <- pop
            pushd$ convert_string v
  {-0x73-}ConvertInt -> do
            D v <- pop
            pushd$ convert_int v
  {-0x74-}ConvertUInt -> do
            D v <- pop
            pushd$ convert_uint v
  {-0x75-}ConvertDouble -> do
            D v <- pop
            pushd$ convert_double v
  {-0x76-}ConvertBoolean -> do
            D v <- pop
            pushd$ convert_boolean v
  {-0x77-}ConvertObject -> ML.raise "NI: ConvertObject"
  {-0x90-}Negate -> do
            D a <- pop
            case a of
              VmRt_Int i -> pushd$ VmRt_Int$ negate i
              VmRt_Uint i -> pushd$ VmRt_Uint$ negate i
              VmRt_Number i -> pushd$ VmRt_Number$ negate i
              otherwise -> raise$ "can't negate " ++ show a
  {-0x91-}Increment -> do
            D a <- pop
            pushd$ a + 1
  {-0x93-}Decrement -> do
            D a <- pop
            pushd$ a - 1
  {-0xA0-}Add -> do
            D a <- pop
            D b <- pop
            pushd$ b + a
  {-0xA1-}Subtract -> do
            D a <- pop
            D b <- pop
            pushd$ b - a
  {-0xA2-}Multiply -> do
            D a <- pop
            D b <- pop
            pushd$ b * a
  {-0xA3-}Divide -> do
            D a <- pop
            D b <- pop
            pushd$ b / a
  {-0xC0-}IncrementInt -> do
            D a <- pop
            pushd$ a+1
  {-0xC1-}DecrementInt -> do
            D a <- pop
            pushd$ a-1
  {-0xC2-}IncLocalInt regIdx -> do
            maybeReg <- liftIO$ H.lookup reg (fromIntegral regIdx)
            case maybeReg of
              Nothing -> raise$ "register " ++ show regIdx ++ " didn't exist"
              Just r -> liftIO$ H.insert reg (fromIntegral regIdx) (r + 1)
  {-0xC3-}DecLocalInt regIdx -> do
            maybeReg <- liftIO$ H.lookup reg (fromIntegral regIdx)
            case maybeReg of
              Nothing -> raise$ "register " ++ show regIdx ++ " didn't exist"
              Just r -> liftIO$ H.insert reg (fromIntegral regIdx) (r - 1)
  {-0xD0-}GetLocal0 -> do
            maybeR <- liftIO$ H.lookup reg 0
            case maybeR of
              Nothing -> raise "GetLocal0 - register doesn't exist"
              Just r -> pushd r
  {-0xD1-}GetLocal1 -> do
            maybeR <- liftIO$ H.lookup reg 1
            case maybeR of
              Nothing -> raise "GetLocal1 - register doesn't exist"
              Just r -> pushd r
  {-0xD2-}GetLocal2 -> do
            maybeR <- liftIO$ H.lookup reg 2
            case maybeR of
              Nothing -> raise "GetLocal2 - register doesn't exist"
              Just r -> pushd r
  {-0xD3-}GetLocal3 -> do
            maybeR <- liftIO$ H.lookup reg 3
            case maybeR of
              Nothing -> raise "GetLocal3 - register doesn't exist"
              Just r -> pushd r
  {-0xD4-}SetLocal0 -> do
            D new <- pop
            liftIO$ H.insert reg 0 new
  {-0xD5-}SetLocal1 -> do
            D new <- pop
            liftIO$ H.insert reg 1 new
  {-0xD6-}SetLocal2 -> do
            D new <- pop
            liftIO$ H.insert reg 2 new
  {-0xD7-}SetLocal3 -> do
            D new <- pop
            liftIO$ H.insert reg 3 new
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






















