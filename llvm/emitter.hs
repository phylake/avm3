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

type State = ML.StateT [R] IO

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
  , abctMethodName = map underscore $ s name
  , abctFlags = flags
  }
  where
    (Abc.MethodSignature rt pts name flags opt pns) = sig
    (Abc.MethodBody _ _ _ _ _ code ex tr) = body
    underscore :: Char -> Char
    underscore '/' = '_'
    underscore a = a

nextT :: D -> State R
nextT d = do
  rs <- ML.get
  let next = maxT rs
  ML.set $ next:rs
  return next
  where
    maxT :: [R] -> R
    maxT [] = RT d 1
    maxT rs = case maximum rs of
      RN _ i -> RT d (i+1)
      RAS3 _ i -> RT d (i+1)
      RT _ i -> RT d (i+1)

lastT :: State R
lastT = do
  (r:_) <- ML.get
  return r

functionEmitter :: AbcTMethod -> State FunctionDef
functionEmitter (AbcTMethod code _ _ ret params name _) = toBlocks code >>=
  return . FunctionDef Nothing Nothing Nothing ret name params

-- TODO pre-alloca
toBlocks :: [(Label, [OpCode])] -> State [Block]
toBlocks = mapM toBlock

toBlock :: (Label, [OpCode]) -> State Block
toBlock (l, ops) = toLLVMOps ops >>= return . Block l

toLLVMOps :: [OpCode] -> State [LLVMOp]
toLLVMOps (PushInt a:SetLocal n:ops) = do
  rest <- toLLVMOps ops
  return $ StoreC I32 (fromIntegral a) (RAS3 (P I32) n):rest
toLLVMOps (PushByte a:SetLocal n:ops) = do
  rest <- toLLVMOps ops
  return $ StoreC I32 (fromIntegral a) (RAS3 (P I32) n):rest
toLLVMOps (Jump l:ops) = do
  rest <- toLLVMOps ops
  return $ Br (UnConditional l) : rest
-- TODO need to know what data type this is
toLLVMOps (GetLocal i:ops) = do
  t <- nextT $ P I32
  rest <- toLLVMOps ops
  return $ Load t (RAS3 (P I32) i) : rest
toLLVMOps (DecrementInt:ops) = do
  t <- lastT
  n <- nextT I32
  rest <- toLLVMOps ops
  return $ Sub n t 1 : rest
toLLVMOps _ = return []

--topStatement :: [(Label, [OpCode])] -> State [TopStmt]
--topStatement = undefined
--topStatement ((l, ops):ts) = return [Block l [Load (R Bool "A") (R Bool "B")]]

emitLLVM :: Abc.Abc -> IO [TopStmt]
emitLLVM abc@(Abc.Abc ints uints doubles strings nsInfo nsSet multinames methodSigs metadata instances classes scripts methodBodies) = do
  (fs :: [(FunctionDef, [R])]) <- mapM (\abctM -> ML.runStateT [RN I32 0] $ functionEmitter abctM) llvms
  return $ map (\(f, _) -> FunctionDef_ f) fs
  where
    rms@(ires, ures, dres, sres, mres) = getResolutionMethods abc
    (llvms :: [AbcTMethod]) = map (toAbcTMethod rms (abcT rms . insertLabels)) $ zip methodSigs methodBodies
