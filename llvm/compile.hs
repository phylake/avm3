{-# LANGUAGE ScopedTypeVariables #-}
module LLVM.Compile where

import           Abc.Def as Abc
import           Abc.Deserialize
import           Abc.Json
import           Abc.Json2
import           Control.Applicative ((<|>))
import           Control.DeepSeq
import           Control.Monad
import           Data.Int
import           Data.Maybe (listToMaybe)
import           Data.Time.Clock
import           Data.Vector ((//), (!))
import           Data.Word
import           Ecma.Prims
import           LLVM.Def
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

scopeStack :: D
scopeStack = P $ Struct [scopeStack, P I32]

-- 1:1 AVM opcode to LLMV opcode
writeMethodBody :: [OpCode -> LLVMOp] -> [OpCode] -> [LLVMOp]
writeMethodBody = undefined

-- passes to alter entire methods
alterMethodBody :: [(OpCode, LLVMOp) -> (OpCode, LLVMOp)] -> [OpCode] -> [LLVMOp]
alterMethodBody = undefined

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
  writeFile "llvm/test.ll" $
       unlines llvm_ints
    ++ unlines llvm_uints
    ++ unlines llvm_doubles
    ++ unlines llvm_strings
    ++ unlines llvm_multinames
    -- ++ unlines (map show $ xform_methodBodies abc)
    ++ "\n"

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
                             , mbCode :: [OpCode]
                             , mbExceptions :: [Exception]
                             , mbTraits :: [TraitsInfo]
                             }
                             deriving (Show)-}
    extractOpCode :: MethodBody -> [OpCode]
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
xform_methodBodies2 fi fu fd fs fm = map toVmMethodBody where
  toVmMethodBody (Abc.MethodBody _ b c d e code g h) =
    newCode `deepseq` MethodBody b c d e newCode g registers
    where
      newCode = concatMap (xform_opCode fi fu fd fs fm $ xform_traits fm h) code

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

      maxReg :: [OpCode] -> Int
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
