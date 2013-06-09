{-# LANGUAGE ScopedTypeVariables #-}
module LLVM.Compile where

import           Abc.Def as Abc
import           Abc.Deserialize
import           Abc.Json
import           Abc.Json2
import           Abc.Util (toBytes)
import           Control.Applicative ((<|>))
import           Control.DeepSeq
import           Control.Monad
import           Data.Int
import           Data.Maybe (listToMaybe)
import           Data.Time.Clock
import           Data.Vector ((//), (!))
import           Data.Word
import           Ecma.Prims
import           LLVM.AbcOps as AbcT
import           LLVM.Lang
import           LLVM.Passes.Branch
import           LLVM.Util
import           Prelude hiding (lookup)
import           Text.JSON
import           Util.Misc
import           Util.Words
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified MonadLib as ML

scopeStack :: D
scopeStack = P $ Struct [scopeStack, P I32]

{-
  define i32 @global.Test.main() {
  entry:
    %as_2 = alloca i32
    store i32 1000000, i32* %as_2
    %as_3 = alloca i32
    store i32 0, i32* %as_3
    br label %L1
  L2:
    %T21 = load i32* %as_2
    %T22 = sub i32 %T21, 1
    store i32 %T22, i32* %as_2

    %T31 = load i32* %as_3
    %T32 = add i32 %T31, 1
    store i32 %T32, i32* %as_3
    br label %L1
  L1:
    %b1 = load i32* %as_2
    %b2 = load i32* %as_3
    %cond = icmp ugt i32 %b1, %b2
    br i1 %cond, label %L2, label %L3
  L3:
    %fin = load i32* %as_2
    %d = getelementptr [4 x i8]* @.percentD, i64 0, i64 0
    call i32 @printf(i8* %d, i32 %fin)

    ret i32 0
  }
-}

op2String :: [Abc.OpCode] -> [[String]]

-- assuming %local_* are already allocaed
op2String (Abc.PushInt i:Abc.SetLocal2:ops) = ["store i32 " ++ show i ++ ", i32 * %local_2"] : op2String ops

-- assuming %local_* are already allocaed
op2String (Abc.PushByte i:Abc.SetLocal3:ops) = ["store i32 " ++ show i ++ ", i32 * %local_3"] : op2String ops
op2String (Abc.Jump i:ops) = ["br label %L1"] : op2String ops
op2String (Abc.Label:ops) = ["L1:"] : op2String ops

op2String (Abc.GetLocal2:ops) = ["%T21 = load i32* %local_2"] : op2String ops
op2String (Abc.DecrementInt:ops) = ["%T22 = add i32 -1, %T21"] : op2String ops
op2String (Abc.SetLocal2:ops) = ["store i32 %T22, i32 * %local_2"] : op2String ops

-- translate IncLocalInt 3 to GetLocal3 IncrementInt SetLocal3 POST Label insertion
op2String (Abc.GetLocal3:ops) = ["%T31 = load i32* %local_3"] : op2String ops
op2String (Abc.IncrementInt:ops) = ["%T32 = add i32 1, %T31"] : op2String ops
op2String (Abc.SetLocal3:ops) = ["store i32 %T32, i32 * %local_3"] : op2String ops

op2String (Abc.GetLocal2:ops) = ["%T23 = load i32* %local_2"] : op2String ops
op2String (Abc.GetLocal3:ops) = ["%T33 = load i32* %local_3"] : op2String ops

-- need to insert Label after IfGreaterThan
op2String (Abc.IfGreaterThan i:ops) = ["%gt0 = icmp ugt i32 %T23, %T33", "br i1 %gt0, label %L1, label %L2", "L2:"] : op2String ops

-- for every register i need (1) the temporary register count and (2) the data type
op2String (Abc.GetLocal2:ops) = ["%T24 = load i32* %local_2"] : op2String ops
op2String (Abc.GetLocal3:ops) = ["%T34 = load i32* %local_3"] : op2String ops
-- will need to know the data types of the two things i'm adding as this will either be
-- a simple add or a call to an object's add function
op2String (Abc.Add:ops) = ["%add0 = add i32 %T24, %T34"] : op2String ops
op2String (Abc.ReturnValue:ops) = ["ret i32 %add0"] : op2String ops
op2String _ = []

theops :: [Abc.OpCode]
theops =
  [
    Abc.PushInt 1000000     -- 
  , Abc.SetLocal2           -- store i32 1000000, i32* %reg_2
  , Abc.PushByte 0          -- 
  , Abc.SetLocal3           -- store i32 0, i32* %reg_3
  , Abc.Jump 6              -- br label %L1; L2:
  , Abc.GetLocal2           -- %T21 = load i32* %reg_2
  , Abc.DecrementInt        -- %T22 = sub i32 %T21, 1
  , Abc.SetLocal2           -- store i32 %T22, i32* %reg_2
  , Abc.IncLocalInt 3       -- %T31 = load i32* %reg_3; %T32 = add i32 %T31, 1; store i32 %T32, i32* %reg_3
  , Abc.Label               -- L1:
  , Abc.GetLocal2           -- %T41 = load i32* %reg_2
  , Abc.GetLocal3           -- %T42 = load i32* %reg_3
  , Abc.IfGreaterThan (-12) -- %cond = icmp ugt i32 %T41, %T42; br i1 %cond, label %L2, label %L3; L3:
  , Abc.GetLocal2           -- %T51 = load i32* %reg_2
  , Abc.GetLocal3           -- %T52 = load i32* %reg_3
  , Abc.Add                 -- %T53 = add i32 %T51, %T52
  , Abc.ReturnValue         -- ret i32 %T53
  ]

{-
TODO passes in order
  - insert entry: Label
  - insert Label after branching ops
  - tag Labels
  - detect local register types and alloca them
  - look ahead for unary ops like Push* since they resolve to a single llvm op
  - i think binary ops only need the last 2 temp registers which are already
    captured in the StateT
  - detect types being added to know whether to call a function or just add
    and possibly insert arbitary new codes to keep transform to LLVMOp simple
-}

locals :: Abc.OpCode -> Abc.OpCode
locals Abc.GetLocal0 = Abc.GetLocal 0
locals Abc.GetLocal1 = Abc.GetLocal 1
locals Abc.GetLocal2 = Abc.GetLocal 2
locals Abc.GetLocal3 = Abc.GetLocal 3
locals Abc.SetLocal0 = Abc.SetLocal 0
locals Abc.SetLocal1 = Abc.SetLocal 1
locals Abc.SetLocal2 = Abc.SetLocal 2
locals Abc.SetLocal3 = Abc.SetLocal 3
locals op = op

{-
can only alter code in place. modifying length will result in incorrect branches
once branches are resolved consider grouping code by branch before doing expansions like
IncLocalInt 3 -> [GetLocal3, IncrementInt, SetLocal3]
-}
simplify :: [Abc.OpCode] -> [Abc.OpCode]
simplify = map locals

{-
many to many
push + any instruction == 1 AbcT.OpCode
inclocalint == 3 AbcT.OpCodes
-}
type OpT1 = [(Abc.OpCode, [AbcT.OpCode])]
type OpT2 = [([AbcT.OpCode], [LLVMOp])]

type StateOpT1 = ML.StateT [R] IO

unaryOp :: OpT1 -> StateOpT1 OpT1
unaryOp ops = undefined

binaryOp :: OpT1 -> StateOpT1 OpT1
binaryOp ops = undefined

runner :: OpT1 -> StateOpT1 OpT1
runner ops = binaryOp ops >>= unaryOp

main :: IO ()
main = do
  abc@(Abc ints uints doubles strings nsInfo nsSet multinames methodSigs metadata instances classes scripts methodBodies) <- E.run_ (EB.enumFile "abc/Test.abc" E.$$ parseAbc)
  let (_, _, _, _, mres) = getResolutionMethods abc
  llvm_ints <- mapM (\(i, a) -> return $ "@.int_" ++ show i ++ " = constant i32 " ++ show a) $ zip (map fromIntegral [0..length ints]) ints
  llvm_uints <- mapM (\(i, a) -> return $ "@.uint_" ++ show i ++ " = constant i32 " ++ show a) $ zip (map fromIntegral [0..length uints]) uints
  llvm_doubles <- mapM (\(i, a) -> return $ "@.double_" ++ show i ++ " = constant double " ++ show a) $ zip (map fromIntegral [0..length doubles]) doubles
  llvm_strings <- mapM (\(i, a) -> return $ "@.string_" ++ show i ++ " = constant [" ++ show (length a + 1) ++ " x i8] c\"" ++ a ++ "\\00\"") $ zip (map fromIntegral [0..length strings]) strings
  --mapM (\(i, a) -> put_nsInfo cp idx a) $ zip (map fromIntegral [0..length nsInfo]) nsInfo
  --mapM (\(i, a) -> put_nsSet cp idx a) $ zip (map fromIntegral [0..length nsSet]) nsSet
  llvm_multinames <- mapM (\(i, a) -> return $ "@.mn_" ++ show i ++ " = constant [" ++ show (length (mres i) + 1) ++ " x i8] c\"" ++ mres i ++ "\\00\"") $ zip (map fromIntegral [0..length multinames]) multinames
  
  (llvm_function_definitions :: OpT1, _) <- ML.runStateT [RN I32 0] $ runner [(a,[]) | a <- simplify theops]
  --let llvm_function_definitions :: [AbcT.OpCode] = []
  mapM_ (putStrLn . show) llvm_function_definitions

  writeFile "llvm/compiled.ll" $
       unlines llvm_ints
    ++ unlines llvm_uints
    ++ unlines llvm_doubles
    ++ unlines llvm_strings
    ++ unlines llvm_multinames
    -- ++ unlines (map show $ xform_methodBodies abc)
    ++ "\n"
    ++ unlines (map show llvm_function_definitions)

  --mapM (\(i, a) -> put_methodSig cp idx a) $ zip (map fromIntegral [0..length methodSigs]) methodSigs
  --mapM (\(i, a) -> put_metadata cp idx a) $ zip (map fromIntegral [0..length metadata]) metadata
  --mapM (\(i, a) -> put_instance cp idx a) $ zip (map fromIntegral [0..length instances]) instances
  --mapM (\(i, a) -> put_class cp idx a) $ zip (map fromIntegral [0..length classes]) classes
  --mapM (\(i, a) -> put_script cp idx a) $ zip (map fromIntegral [0..length scripts]) scripts
  --mapM (\(i, a) -> put_methodBody cp idx a) $ zip (map mbMethod methodBodies) methodBodiesNew
  return ()

getResolutionMethods :: Abc -> (
                                 U30 -> S32    -- int
                               , U30 -> U30    -- uint
                               , U30 -> Double -- double
                               , U30 -> String -- string
                               , U30 -> String -- multiname
                               )
getResolutionMethods (Abc ints uints doubles strings nsInfo nsSet multinames methodSigs metadata instances classes scripts methodBodies) =
    (
      int_res
    , uint_res
    , double_res
    , string_res
    , multiname_res
    )
  where
    int_res :: U30 -> S32
    int_res i = ints !! fromIntegral i

    uint_res :: U30 -> U30
    uint_res i = uints !! fromIntegral i

    double_res :: U30 -> Double
    double_res i = doubles !! fromIntegral i

    string_res :: U30 -> String
    string_res i = strings !! fromIntegral i

    multiname_res :: U30 -> String
    multiname_res i = multiname_impl string_res nsinfo_res $ multinames !! fromIntegral i

    nsinfo_res :: U30 -> String
    nsinfo_res i = nsinfo_impl string_res$ nsInfo !! fromIntegral i

xform_methodBodies :: Abc -> M.Map Int [LLVMOp]
xform_methodBodies (Abc ints uints doubles strings nsInfo nsSet multinames methodSigs metadata instances classes scripts methodBodies) =
  --map extractOpCode methodBodies
  undefined
  where
    {-data MethodSignature = MethodSignature {
                                         msReturnType :: MultinameIdx
                                       , msParamTypes :: [MultinameIdx]
                                       , msMethodName :: StringIdx -- debug
                                       , msFlags :: Word8
                                       , msOptionInfo :: Maybe [CPC]
                                       , msParamNames :: Maybe [StringIdx] -- debug
                                       }
                                       deriving (Show)-}
    {-data MethodBody = MethodBody {
                               mbMethod :: MethodSignatureIdx
                             , mbMaxStack :: U30
                             , mbLocalCount :: U30
                             , mbInitScopeDepth :: U30
                             , mbMaxScopeDepth :: U30
                             , mbCode :: [Abc.OpCode]
                             , mbExceptions :: [Exception]
                             , mbTraits :: [TraitsInfo]
                             }
                             deriving (Show)-}
    extractOpCode :: MethodBody -> [Abc.OpCode]
    extractOpCode (MethodBody _ _ _ _ _ code _ _) = code


multiname_impl :: (U30 -> String) -- string resolution
               -> (U30 -> String) -- nsinfo resolution
               -> Multiname
               -> String
multiname_impl string_res nsinfo_res (Multiname_QName ns str)
  | nsinfo == "" = string
  | otherwise = nsinfo ++ "::" ++ string
  where
    nsinfo = nsinfo_res ns
    string = string_res str
multiname_impl string_res nsinfo_res (Multiname_QNameA ns str)
  | nsinfo == "" = string
  | otherwise = nsinfo ++ "::" ++ string
  where
    nsinfo = nsinfo_res ns
    string = string_res str
multiname_impl string_res nsinfo_res (Multiname_Multiname str ns)
  | nsinfo == "" = string
  | otherwise = nsinfo ++ "::" ++ string
  where
    nsinfo = nsinfo_res ns
    string = string_res str
multiname_impl string_res nsinfo_res (Multiname_MultinameA str ns)
  | nsinfo == "" = string
  | otherwise = nsinfo ++ "::" ++ string
  where
    nsinfo = nsinfo_res ns
    string = string_res str
multiname_impl _ _ (Multiname_RTQName a) = "Multiname_RTQName"
multiname_impl _ _ (Multiname_RTQNameA a) = "Multiname_RTQNameA"
multiname_impl _ _ (Multiname_MultinameL a) = "Multiname_MultinameL"
multiname_impl _ _ (Multiname_MultinameLA a) = "Multiname_MultinameLA"
multiname_impl _ _ Multiname_Any = "*"

nsinfo_impl :: (U30 -> String) -- string resolution
            -> NSInfo
            -> String
nsinfo_impl string_res (NSInfo_Namespace a)          = string_res a
nsinfo_impl string_res (NSInfo_PackageNamespace a)   = string_res a
nsinfo_impl string_res (NSInfo_PackageInternalNs a)  = string_res a
nsinfo_impl string_res (NSInfo_ProtectedNamespace a) = string_res a
nsinfo_impl string_res (NSInfo_ExplicitNamespace a)  = string_res a
nsinfo_impl string_res (NSInfo_StaticProtectedNs a)  = string_res a
nsinfo_impl string_res (NSInfo_PrivateNs a)          = string_res a
nsinfo_impl string_res NSInfo_Any                    = "*"

{-xform_methodBodies2 :: (Abc.IntIdx -> Abc.S32)                  -- int resolution
                    -> (Abc.UintIdx -> Abc.U32)                 -- uint resolution
                    -> (Abc.DoubleIdx -> Double)                -- double resolution
                    -> (Abc.StringIdx -> B.ByteString)          -- string resolution
                    -> (Abc.MultinameIdx -> Maybe B.ByteString) -- multiname resolution
                    -> [Abc.MethodBody]
                    -> [MethodBody]
xform_methodBodies2 fi fu fd op2String fm = map toVmMethodBody where
  toVmMethodBody (Abc.MethodBody _ b c d e code g h) =
    newCode `deepseq` MethodBody b c d e newCode g registers
    where
      newCode = concatMap (xform_opCode fi fu fd op2String fm $ xform_traits fm h) code

      xform_traits :: (Abc.MultinameIdx -> Maybe B.ByteString) -- multiname resolution
                   -> [Abc.TraitsInfo]
                   -> Abc.U30
                   -> TraitsInfo
      xform_traits m_res ((Abc.TraitsInfo a b c tt@(Abc.TraitVar tid _ _ _) d):ts) u30
        | tid == u30 = (TraitsInfo (m_res a) b c tt d)
        | otherwise = xform_traits m_res ts u30
      xform_traits m_res ((Abc.TraitsInfo a b c tt@(Abc.TraitConst tid _ _ _) d):ts) u30
        | tid == u30 = (TraitsInfo (m_res a) b c tt d)
        | otherwise = xform_traits m_res ts u30
      xform_traits m_res ((Abc.TraitsInfo a b c tt@(Abc.TraitMethod tid _) d):ts) u30
        | tid == u30 = (TraitsInfo (m_res a) b c tt d)
        | otherwise = xform_traits m_res ts u30
      xform_traits m_res ((Abc.TraitsInfo a b c tt@(Abc.TraitGetter tid _) d):ts) u30
        | tid == u30 = (TraitsInfo (m_res a) b c tt d)
        | otherwise = xform_traits m_res ts u30
      xform_traits m_res ((Abc.TraitsInfo a b c tt@(Abc.TraitSetter tid _) d):ts) u30
        | tid == u30 = (TraitsInfo (m_res a) b c tt d)
        | otherwise = xform_traits m_res ts u30
      xform_traits m_res ((Abc.TraitsInfo a b c tt@(Abc.TraitClass tid _) d):ts) u30
        | tid == u30 = (TraitsInfo (m_res a) b c tt d)
        | otherwise = xform_traits m_res ts u30
      xform_traits m_res ((Abc.TraitsInfo a b c tt@(Abc.TraitFunction tid _) d):ts) u30
        | tid == u30 = (TraitsInfo (m_res a) b c tt d)
        | otherwise = xform_traits m_res ts u30

      registers :: Registers
      registers = V.replicate (maxReg newCode) VmRt_Undefined

      maxReg :: [Abc.OpCode] -> Int
      maxReg [] = 0
      maxReg (GetLocal0:ops) = max 1 $ maxReg ops
      maxReg (GetLocal1:ops) = max 2 $ maxReg ops
      maxReg (GetLocal2:ops) = max 3 $ maxReg ops
      maxReg (GetLocal3:ops) = max 4 $ maxReg ops
      maxReg (GetLocal u30:ops) = max (fromIntegral u30 + 1) $ maxReg ops
      maxReg (SetLocal0:ops) = max 1 $ maxReg ops
      maxReg (SetLocal1:ops) = max 2 $ maxReg ops
      maxReg (SetLocal2:ops) = max 3 $ maxReg ops
      maxReg (SetLocal3:ops) = max 4 $ maxReg ops
      maxReg (SetLocal u30:ops) = max (fromIntegral u30 + 1) $ maxReg ops
      maxReg (op:ops) = maxReg ops-}
