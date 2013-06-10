module LLVM.Lang where

import           Abc.DeepSeq
import           Control.DeepSeq
import           Data.Int
import           Data.List
import           Data.Word
import           Util.Words (u30Bytes)
import qualified Abc.Def as Abc
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

-- TODO constants on R or D

data BrType = UnConditional Label | Conditional R Label Label

data Block = Block Label [LLVMOp]
instance Show Block where
  show (Block l ops) = unlines $ (show l):map ((++)"\t" . show) ops

data Label = L String Int deriving (Eq)
instance Show Label where
  show (L "" i) = "L" ++ show i ++ ":"
  show (L s i) = s ++ show i ++ ":"

data R = R D String -- semantically meaningful register. don't name mangle
       | RN D Int   -- numbered
       | RAS3 D Int -- maps to GetLocal and SetLocal
       | RT D Int   -- temporary (not sure if String will be needed)
instance Show R where
  show (R d n) = show d ++ " %" ++ n
  show (RT d i) = show d ++ " %" ++ show i
  show (RN d i) = show d ++ " %" ++ show i
  show (RAS3 d i) = show d ++ " %" ++ show i
instance Eq R where
  (R _ _) == (R _ _) = True
  (R _ _) == (RN _ b) = False
  (R _ _) == (RAS3 _ b) = False
  (R _ _) == (RT _ b) = False
  
  (RN _ a) == (R _ _) = False
  (RN _ a) == (RN _ b) = a == b
  (RN _ a) == (RAS3 _ b) = a == b
  (RN _ a) == (RT _ b) = a == b

  (RAS3 _ a) == (R _ _) = False
  (RAS3 _ a) == (RN _ b) = a == b
  (RAS3 _ a) == (RAS3 _ b) = a == b
  (RAS3 _ a) == (RT _ b) = a == b

  (RT _ a) == (R _ _) = False
  (RT _ a) == (RN _ b) = a == b
  (RT _ a) == (RAS3 _ b) = a == b
  (RT _ a) == (RT _ b) = a == b
instance Ord R where
  compare (R _ _) (R _ _) = EQ
  compare (R _ _) (RN _ b) = LT
  compare (R _ _) (RAS3 _ b) = LT
  compare (R _ _) (RT _ b) = LT
  
  compare (RN _ a) (R _ _) = GT
  compare (RN _ a) (RN _ b) = compare a b
  compare (RN _ a) (RAS3 _ b) = compare a b
  compare (RN _ a) (RT _ b) = compare a b

  compare (RAS3 _ a) (R _ _) = GT
  compare (RAS3 _ a) (RN _ b) = compare a b
  compare (RAS3 _ a) (RAS3 _ b) = compare a b
  compare (RAS3 _ a) (RT _ b) = compare a b

  compare (RT _ a) (R _ _) = GT
  compare (RT _ a) (RN _ b) = compare a b
  compare (RT _ a) (RAS3 _ b) = compare a b
  compare (RT _ a) (RT _ b) = compare a b

data LLVMOp = Load R R
            | Store R R
            | Alloca R R
            | GetElementPtr R R [Int]
            | Br BrType
instance Show LLVMOp where
  show (Load (R _ a) (R bd b)) = "%" ++ a ++ " = load " ++ show bd ++ " %" ++ b
  show (Store a b) = "store " ++ show a ++ ", " ++ show b
  -- TODO constant: store i32 0, i32* %3

data D = Bool
       | Void
       | I8
       | I32
       | IN Int
       | VarArg
       | Struct [D]
       | Func D [D]
       | P D
       deriving (Eq)
instance Show D where
  show (Bool) = "i1"
  show (I8) = "i8"
  show (I32) = "i32"
  show (IN i) = "i" ++ show i
  show (VarArg) = "..."
  show (Struct ds) = "{" ++ csv ds ++ "}"
  show (Func d ds) = show d ++ " (" ++ csv ds ++ ")"
  show (P d) = show d ++ " *"

csv :: [D] -> String
csv = concat . intersperse ", " . map show

data Global = Global String Linkage
data TopStmt = Global_ Global
             | FunctionDec_ FunctionDec
             | FunctionDef_ FunctionDef
             | Constant String D

data Linkage = Private
             | LinkerPrivate
             | LinkerPrivateWeak
             | Internal
             | AvailableExternally
             | Linkonce
             | Weak
             | Common
             | Appending
             | ExternWeak
             | LinkonceOdr
             | WeakOdr
             | LinkonceOdrAutoHide
             | External
instance Show Linkage where
  show Private = "private"
  show LinkerPrivate = "linker_private"
  show LinkerPrivateWeak = "linker_private_weak"
  show Internal = "internal"
  show AvailableExternally = "available_externally"
  show Linkonce = "linkonce"
  show Weak = "weak"
  show Common = "common"
  show Appending = "appending"
  show ExternWeak = "extern_weak"
  show LinkonceOdr = "linkonce_odr"
  show WeakOdr = "weak_odr"
  show LinkonceOdrAutoHide = "linkonce_odr_auto_hide"
  show External = "external"

data CallingConvention = C | Fast | Cold | GHC | HiPE | CustomCC Int
instance Show CallingConvention where
  show C = "ccc"
  show Fast = "fastcc"
  show Cold = "coldcc"
  show GHC = "cc 10"
  show HiPE = "cc 11"
  show (CustomCC i) = "cc " ++ show i

data Visibility = Default | Hidden | Protected
instance Show Visibility where
  show Default = "default"
  show Hidden = "hidden"
  show Protected = "protected"

data FunctionDef = FunctionDef
                     Linkage
                     Visibility
                     CallingConvention
                     D        -- return value
                     String   -- name
                     [D]      -- parameters
                     [Block]  -- function body
data FunctionDec = FunctionDec
                     Linkage
                     Visibility
                     CallingConvention
                     D        -- return value
                     String   -- name
                     [D]      -- parameters

data ParameterAttr = Zeroext
                   | Signext
                   | Inreg
                   | Byval
                   | Sret
                   | Noalias
                   | Nocapture
                   | Nest
                   | Nobuiltin
instance Show ParameterAttr where
  show Zeroext = "zeroext"
  show Signext = "signext"
  show Inreg = "inreg"
  show Byval = "byval"
  show Sret = "sret"
  show Noalias = "noalias"
  show Nocapture = "nocapture"
  show Nest = "nest"
  show Nobuiltin = "nobuiltin"

data FunctionAttr = Alignstack Int
                  | Alwaysinline
                  | Nonlazybind
                  | Inlinehint
                  | Naked
                  | Noduplicate
                  | Noimplicitfloat
                  | Noinline
                  | Noredzone
                  | Noreturn
                  | Nounwind
                  | Optsize
                  | Readnone
                  | Readonly
                  | Returns_twice
                  | Sanitize_address
                  | Sanitize_memory
                  | Sanitize_thread
                  | Ssp
                  | Sspreq
                  | Sspstrong
                  | Uwtable

data LLVMInt = LLVMInt Int
instance Show LLVMInt where
  show (LLVMInt i) = "i" ++ show i

data LLVMFloat = Half
               | Float
               | Double
               | X86_fp80
               | Fp128
               | Ppc_fp128
instance Show LLVMFloat where
  show Half = "half"
  show Float = "float"
  show Double = "double"
  show X86_fp80 = "x86_fp80"
  show Fp128 = "fp128"
  show Ppc_fp128 = "ppc_fp128"
