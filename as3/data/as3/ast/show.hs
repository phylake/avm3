module Data.AS3.AST.Show (PrettyAs(..)) where

import           Control.Monad (liftM, liftM2)
import           Data.AS3.AST.Def hiding (M,p)
import           Data.Foldable (foldl)
import           Data.List (intersperse, intercalate)
import qualified MonadLib as ML

type M = ML.StateT Int ML.Id

class PrettyAs a where
  toAs3 :: a -> M String

instance Show Expression where
  show = fst . ML.runId . ML.runStateT 0 . toAs3

instance Show Statement where
  show = fst . ML.runId . ML.runStateT 0 . toAs3

inBlock :: (PrettyAs a) => [a] -> M String
inBlock action = do
  open <- p "{"
  body <- withIndent $ mapM toAs3 action
  close <- p "}\n"
  return $ unlines $ [open] ++ body ++ [close]

withIndent :: M a -> M a
withIndent action = do
  i <- ML.get
  ML.set (i+1)
  ret <- action
  ML.set i
  return ret

withNoIndent :: M a -> M a
withNoIndent action = do
  orig <- ML.get
  ML.set 0
  ret <- action
  ML.set orig
  return ret

spaces :: M String
spaces = do
  i <- ML.get
  return $ replicate i '\t'

p :: String -> M String
p = liftM2 (++) spaces . return

instance PrettyAs Statement where
  toAs3 EmptyS = return ""
  toAs3 (Block sts) = inBlock sts
  toAs3 (Variable e) = do
    exps <- mapM toAs3 e
    p $ "var " ++ intercalate ", " exps ++ ";"
  toAs3 (Constant e) = do
    exps <- mapM toAs3 e
    p $ "const " ++ intercalate ", " exps ++ ";"
  toAs3 (ExpressionStmt e) = toAs3 e
  --toAs3 (ExpressionStmt e) = liftM2 (++) (return "ExpressionStmt: ") (toAs3 e)
  toAs3 (If e s@(Block _)) = do
    exp <- withNoIndent $ toAs3 e
    body <- toAs3 s
    p $ "if (" ++ exp ++ ")\n" ++ body
  toAs3 (If e s) = do
    e' <- toAs3 e
    e' <- toAs3 s
    p $ "if (" ++ e' ++ ") " ++ e' ++ ";"
  toAs3 (IfElse e s1 s2) = return "IfElse"
  toAs3 (DoWhile s e) = return "DoWhile"
  toAs3 (While e s) = do
    e' <- toAs3 e
    s' <- toAs3 s
    p $ "while (" ++ e' ++ ")\n" ++ s'
  toAs3 (For me1 me2 me3 s) = do
    me1' <- maybe (return ";") (withNoIndent . toAs3) me1
    me2' <- maybe (return "") toAs3 me2
    me3' <- maybe (return "") toAs3 me3
    s' <- toAs3 s
    for <- p $ "for (" ++ me1' ++ me2' ++ "; " ++ me3' ++ ")"
    return $ unlines [for, s']
  toAs3 (ForIn e1 e2 s) = return "ForIn"
  toAs3 (ForEach e1 e2 s) = return "ForEach"
  toAs3 (Continue me) = return "Continue"
  toAs3 (Break me) = return "Break"
  toAs3 (Return me) = return "Return"
  toAs3 (With e s) = return "With"
  toAs3 (Switch e s) = return "Switch"
  toAs3 (Case e ms) = return "Case"
  toAs3 (Default ms) = return "Default"
  toAs3 (Package a body) = do
    body <- inBlock body
    return $ unlines ["package" ++ maybe "" ((++)" ") a, body]
  toAs3 (Import a) = p $ "import " ++ a ++ ";"
  toAs3 (Class scopes name extends implements body) = do
    dec <- p $ intercalate " " (map show scopes) ++ " class " ++ name
               ++ maybe "" (" extends "++) extends
               ++ maybe "" (\i -> " implements " ++ intercalate ", " i) implements
               ++ "\n"
    body' <- inBlock body
    return $ dec ++ body'
  toAs3 (FnDec scopes name params t body) = do
    body <- withIndent $ mapM toAs3 body
    params <- withNoIndent $ mapM toAs3 params
    p $
      intercalate " " (map show scopes) ++ " function " ++ name
      ++ "(" ++ intercalate ", " params ++ "):" ++ show t ++ "\n"
      ++ "\t\t{\n" ++ unlines body ++ "\n\t\t}"

instance PrettyAs Expression where
  toAs3 (TODO_E a) = return $ "TODO[" ++ a ++ "]"
  toAs3 (Comma a) = liftM (intercalate ", ") $ mapM toAs3 a
  toAs3 (ParenGroup a) = do
    a' <- toAs3 a
    return $ "(" ++ a' ++ ")"
  toAs3 (ObjectLiteral kvps) = do
    kvps <- mapM toAs3 kvps
    close <- p "}"
    return $ "{" ++ intercalate ", " kvps ++ close
  toAs3 (KeyValue k v) = do
    k <- toAs3 k
    v <- toAs3 v
    return $ k ++ ":" ++ v
  toAs3 (TernOp cond t f) = do
    cond <- toAs3 cond
    t <- toAs3 t
    f <- toAs3 f
    return $ cond ++ " ? " ++ t ++ " : " ++ f
  toAs3 (RBinOp l op r) = do
    l' <- toAs3 l
    r' <- toAs3 r
    return $ intercalate " " [l', show op, r']
  toAs3 (LBinOp op l r) = do
    l' <- toAs3 l
    r' <- toAs3 r
    return $ intercalate " " [l', show op, r']
  toAs3 (Unary op e) = liftM2 (++) (return $ show op) (toAs3 e)
  toAs3 (ClassId ms cv n t) = p $
    intercalate " " (map show ms ++ [show cv, n]) ++ ":" ++ show t ++ ";"
  toAs3 (FnId cv n t) = return $ intercalate " " [show cv, n] ++ ":" ++ show t
  toAs3 (TypedId n t) = return $ n ++ ":" ++ show t
  toAs3 (ExpressionId a) = return a
  toAs3 (Lit a) = return $ show a
  toAs3 (Postfix e op) = liftM2 (++) (toAs3 e) (return $ show op)
  toAs3 (ArrayAccess a b) = do
    a' <- toAs3 a
    b' <- toAs3 b
    return $ a' ++ "[" ++ b' ++ "]"
  toAs3 (Call a b) = do
    a' <- toAs3 a
    b' <- toAs3 b
    return $ a' ++ "." ++ b'


instance Show BinaryOp where
  show Addition = "+"
  show Subtraction = "-"
  show Multiplication = "*"
  show Division = "/"
  show Modulo = "%"

  show LShift = "<<"
  show RShift = ">>"
  show URShift = ">>>"

  show LogicalAND = "&&"
  show LogicalOR = "||"

  show BitwiseAND = "&"
  show BitwiseOR = "|"
  show BitwiseXOR = "^"

  show LessThan = "<"
  show GreaterThan = ">"
  show LessThanEq = "<="
  show GreaterThanEq = ">="
  show InstanceOf = "instanceof"
  show In = "in"

  show Assignment = "="
  show PlusAssignment = "+="
  show MinusAssignment = "-="
  show MultiplicationAssignment = "*="
  show DivisionAssignment = "/="
  show ModuloAssignment = "%="
  show LShiftAssignment = "<<="
  show RShiftAssignment = ">>="
  show URShiftAssignment = ">>>="
  show BitwiseANDAssignment = "&="
  show BitwiseORAssignment = "|="
  show BitwiseXORAssignment = "^="

  show Equality = "=="
  show StrictEquality = "==="
  show InEquality = "!="
  show StrictInEquality = "!=="

instance Show UnaryOp where
  show Delete = "delete"
  show Void = "void"
  show TypeOf = "typeof"
  show Increment = "++"
  show Decrement = "--"
  show Positive = "+"
  show Negative = "-"
  show BitwiseNOT = "~"
  show LogicalNOT = "!"

instance Show Literal where
  show L_void = "void"
  show L_undefined = "undefined"
  show (L_int a) = show a
  show (L_uint a) = show a
  show (L_Number a) = show a
  show (L_Boolean True) = "true"
  show (L_Boolean False) = "false"
  show (L_String a) = a
  show (L_RegExp a) = a

instance Show Type where
  show T_int = "int"
  show T_uint = "uint"
  show T_void = "void"
  show T_undefined = "*"
  show T_Number = "Number"
  show T_Boolean = "Boolean"
  show T_String = "String"
  show T_Array = "Array"
  show T_Object = "Object"
  show (T_Vector t) = "Vector.<" ++ show t ++ ">"
  show (T_UserDefined t) = t

instance Show ScopeMod where
  show Public = "public"
  show Protected = "protected"
  show Private = "private"
  show Final = "final"
  show Override = "override"
  show Static = "static"

instance Show CV where
  show Const = "const"
  show Var = "var"
