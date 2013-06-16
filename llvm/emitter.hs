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

nextR :: D -> State R
nextR d = do
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

lastR :: State R
lastR = lastRs 1 >>= return . head

lastRs :: Int -> State [R]
lastRs a = ML.get >>= return . take a

functionEmitter :: AbcTMethod -> State FunctionDef
functionEmitter (AbcTMethod code _ _ ret params name _) = toBlocks code >>=
  return . FunctionDef Nothing Nothing Nothing ret name params

toBlocks :: [(Label, [OpCode])] -> State [Block]
--toBlocks = mapM toBlock
toBlocks ls = mapM toBlock ls >>= return . ensureTrailingBranch
  where
    ensureTrailingBranch :: [Block] -> [Block]
    ensureTrailingBranch ret@(_:[]) = ret
    ensureTrailingBranch (b1@(Block l ops):b2@(Block l2 _):bs) = case last ops of
      Br _ -> b1 : ensureTrailingBranch (b2:bs)
      otherwise -> Block l (ops ++ [Br $ UnConditional l2]) : ensureTrailingBranch (b2:bs)

    preAlloca :: [Block] -> [Block]
    preAlloca all@(Block l entryOps:bs) = Block l (preAllocaOps ++ entryOps):bs
      where
        ops :: [LLVMOp]
        ops = concatMap (\(Block _ ops) -> ops) all

        preAllocaOps :: [LLVMOp]
        preAllocaOps = undefined

toBlock :: (Label, [OpCode]) -> State Block
toBlock (l, ops) = toLLVMOps ops >>= return . Block l

returnR :: [OpCode] -> [LLVMOp] -> State [LLVMOp]
returnR abc llvm = do
  rest <- toLLVMOps abc
  return $ llvm ++ rest

-- the current goal is to preserve simple rules at the expense of efficiency
-- knowing that many of the inefficiencies will be handled by opt. The
-- "Combine redundant instructions" pass, for example, is taking care of
-- useless load/store operations for the Push* line of abc ops
toLLVMOps :: [OpCode] -> State [LLVMOp]
toLLVMOps (SetLocal a:ops) = do
  lt@(RT d _) <- lastR
  returnR ops
    [
      Comment $ "SetLocal" ++ show a
    , StoreR lt (RAS3 (P d) a)
    ]

-- TODO need to know what data type this is
toLLVMOps (GetLocal a:ops) = do
  t <- nextR I32
  returnR ops
    [
      Comment $ "GetLocal" ++ show a
    , Load t (RAS3 (P I32) a)
    ]

toLLVMOps (IfGreaterThan t f:ops) = do
  (t1:t2:[]) <- lastRs 2
  i1 <- nextR Bool
  returnR ops
    [
      Comment $ "IfGreaterThan " ++ show t ++ " " ++ show f
      -- TODO sign
    , Icmp i1 UGT t1 t2
    , Br $ Conditional i1 t f
    ]

toLLVMOps (PushInt a:ops) = do
  t1 <- nextR $ P I32
  t2 <- nextR I32
  returnR ops
    [
      Comment "PushInt"
    , Alloca t1
    , StoreC I32 (fromIntegral a) t1
    , Load t2 t1 -- the next instruction is expecting a value
    ]

toLLVMOps (PushByte a:ops) = do
  t1 <- nextR $ P I32
  t2 <- nextR I32
  returnR ops
    [
      Comment "PushByte"
    , Alloca t1
    , StoreC I32 (fromIntegral a) t1
    , Load t2 t1 -- the next instruction is expecting a value
    ]

toLLVMOps (Jump l:ops) = do
  returnR ops
    [
      Comment $ "Jump " ++ show l
    , Br (UnConditional l)
    ]

toLLVMOps (IncrementInt:ops) = do
  lt <- lastR
  t <- nextR I32
  returnR ops
    [
      Comment "IncrementInt"
    , LLVM.Lang.AddC t 1 lt
    ]

toLLVMOps (DecrementInt:ops) = do
  lt <- lastR
  t <- nextR I32
  returnR ops
    [
      Comment "DecrementInt"
    , Sub t lt 1
    ]

toLLVMOps (LLVM.AbcOps.Add:ops) = do
  (t1:t2:[]) <- lastRs 2
  t <- nextR I32
  returnR ops
    [
      Comment "Add"
    , LLVM.Lang.Add t t1 t2
    ]

toLLVMOps (ReturnValue:ops) = do
  lt <- lastR
  returnR ops
    [
      Comment "ReturnValue"
    , Ret lt
    ]

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





























