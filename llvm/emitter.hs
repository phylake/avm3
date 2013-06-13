{-# LANGUAGE ScopedTypeVariables #-}
module LLVM.Emitter (emitLLVM) where

import           Data.Word
import           LLVM.AbcOps
import           LLVM.Lang
import           LLVM.NameRes
import           LLVM.Passes.AbcT
import           LLVM.Passes.Branch
import           LLVM.Passes.LLVMT
import qualified Abc.Def as Abc
import qualified MonadLib as ML

{-scopeStack :: D
scopeStack = P $ Struct [scopeStack, P I32]-}

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

theops :: [Abc.OpCode]
theops =
  [
    Abc.PushInt 2     -- 
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

type State = ML.StateT (AbcTMethod, [R]) IO

data AbcTMethod = AbcTMethod {
                               abctCode :: [(Label, [OpCode])]
                             , abctExceptions :: [Abc.Exception]
                             , abctTraits :: [Abc.TraitsInfo]
                             , abctReturnType :: D
                             , abctParamTypes :: [D]
                             , abctMethodName :: String
                             , abctFlags :: Word8
                             }

{-data MethodSignature = MethodSignature {
                                         msReturnType :: MultinameIdx
                                       , msParamTypes :: [MultinameIdx]
                                       , msMethodName :: StringIdx -- debug
                                       , msFlags :: Word8
                                       , msOptionInfo :: Maybe [CPC]
                                       , msParamNames :: Maybe [StringIdx] -- debug
                                       }
                                       deriving (Show)

data MethodBody = MethodBody {
                               mbMethod :: MethodSignatureIdx
                             , mbMaxStack :: U30
                             , mbLocalCount :: U30
                             , mbInitScopeDepth :: U30
                             , mbMaxScopeDepth :: U30
                             , mbCode :: [OpCode]
                             , mbExceptions :: [Exception]
                             , mbTraits :: [TraitsInfo]
                             }
                             deriving (Show)-}

abcTypeToLLVMType :: String -> D
abcTypeToLLVMType "Boolean" = Bool
abcTypeToLLVMType "int" = I32
abcTypeToLLVMType "uint" = U32
abcTypeToLLVMType "String" = P I8
abcTypeToLLVMType "void" = Void

toAbcTMethod :: ( (Abc.IntIdx -> Abc.S32)
                , (Abc.UintIdx -> Abc.U32)
                , (Abc.DoubleIdx -> Double)
                , (Abc.StringIdx -> String)
                , (Abc.MultinameIdx -> Maybe String) )
             -> ([Abc.OpCode] -> [(Label, [OpCode])])
             -> (Abc.MethodSignature, Abc.MethodBody)
             -> AbcTMethod
toAbcTMethod (i, u, d, s, m) c (sig, body) =
  AbcTMethod {
    abctCode = c code
  , abctExceptions = ex
  , abctTraits = tr
  , abctReturnType = maybe (P I8) abcTypeToLLVMType $ m rt
  , abctParamTypes = map (maybe (P I8) abcTypeToLLVMType . m) pts
  , abctMethodName = s name
  , abctFlags = flags
  }
  where
    (Abc.MethodSignature rt pts name flags opt pns) = sig
    (Abc.MethodBody _ _ _ _ _ code ex tr) = body

nextR :: D -> State R
nextR d = do
  (m, rs) <- ML.get
  let next = maxT rs
  ML.set $ (m, next:rs)
  return next
  where
    maxT :: [R] -> R
    maxT [] = RT d 1
    maxT rs = case maximum rs of
      R _ _ -> RT d 1
      RN _ i -> RT d (i+1)
      RAS3 _ i -> RT d (i+1)
      RT _ i -> RT d (i+1)

lastR :: State R
lastR = do
  (_, r:_) <- ML.get
  return r

functionEmitter :: [(Label, [OpCode])] -> State FunctionDef
functionEmitter = undefined

--topStatement :: [(Label, [OpCode])] -> State [TopStmt]
--topStatement = undefined
--topStatement ((l, ops):ts) = return [Block l [Load (R Bool "A") (R Bool "B")]]

emitLLVM :: Abc.Abc -> [TopStmt]
--emitLLVM = undefined
emitLLVM abc@(Abc.Abc ints uints doubles strings nsInfo nsSet multinames methodSigs metadata instances classes scripts methodBodies) = do
  let rms@(ires, ures, dres, sres, mres) = getResolutionMethods abc
  
  ML.lift $ testInsertLabels theops
  ML.lift . putStrLn $ "--------------"
  let llvms :: [AbcTMethod] = map (toAbcTMethod rms (abcT rms . insertLabels)) $ zip methodSigs methodBodies
  (blocks :: [Block], _) <- ML.runStateT ([RN I32 0]) $ emitLLVM $ abcT ires ures dres sres mres $ insertLabels theops
  return []

