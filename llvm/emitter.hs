{-# LANGUAGE ScopedTypeVariables #-}
module LLVM.Emitter (emitLLVM) where

import           Data.Word
import           LLVM.AbcOps
import           LLVM.Lang
import           LLVM.Util
import           LLVM.Passes.AbcT
import           LLVM.Passes.Branch
import           LLVM.Passes.LLVMT
import qualified Abc.Def as Abc
import qualified MonadLib as ML

{-scopeStack :: D
scopeStack = P $ Struct [scopeStack, P I32]-}

{-
a stack of registers (?) and the register count to make new RTs
pushR/popR
CONSIDER
  var f1:Foo = new Foo;
  var f2:Foo = new Foo;
  f2.bar();
  f1.bar();
VS
  var f1:Foo = new Foo;
  var f2:Foo = new Foo;
  f1.bar();
  f2.bar();
-}

--type State = ML.StateT ([R], Int) IO
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

{-
TODO
  - detect local register types and alloca them
  - detect types being added to know whether to call a function or just add
    and possibly insert arbitary new codes to keep transform to LLVMOp simple
-}

toBlocks :: [(Label, [OpCode])] -> State [Block]
--toBlocks = mapM toBlock
toBlocks ls = mapM toBlock ls
  where
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

{-toLLVMOps (NewObject args:ops) = do
  lt <- lastR
  t <- nextR I32
  returnR ops
    [
      Comment "NewObject"
    , Sub t lt 1
    ]-}

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




























