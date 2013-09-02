module Data.AS3.AST.Show () where

import           Data.Foldable (foldl)
import           Data.List (intersperse, intercalate)
import           Data.AS3.AST.Def

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
  show T_Number = "Number"
  show T_Boolean = "Boolean"
  show T_String = "String"
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

instance Show Statement where
  show EmptyS = ""
  show (Block sts) = "{\n" ++ unlines (map show sts) ++ "\n}"
  show (Variable e me) = show e ++ maybe "" (\a -> " " ++ show a) me
  show (Constant e me) = show e ++ maybe "" (\a -> " " ++ show a) me
  show (ExpressionStmt e) = show e
  show (If e (Block s)) = "if (" ++ show e ++ ")\n{\n" ++ show s ++ "\n}\n"
  show (If e s) = "if (" ++ show e ++ ") " ++ show s ++ ";"
  {-show (IfElse e s1 s2)-}
  {-show (DoWhile s e)-}
  {-show (While e s)-}
  {-show (For me1 me2 me3 s)-}
  {-show (ForIn e1 e2 s)-}
  {-show (ForEach e1 e2 s)-}
  {-show (Continue me)-}
  {-show (Break me)-}
  {-show (Return me)-}
  {-show (With e s)-}
  {-show (Switch e s)-}
  {-show (Case e ms)-}
  {-show (Default ms)-}
  show (Package a body) = "package" ++ maybe "" ((++)" ") a
    ++ "\n{\n"
    ++ unlines (map ((++)"\t" . show) body)
    ++ "\n}\n"
  show (Import a) = "import " ++ a ++ ";"
  show (Class scopes name extends implements body) =
    intercalate " " (map show scopes) ++ " class " ++ name
    ++ maybe "" (" extends "++) extends
    ++ maybe "" (\i -> " implements " ++ intercalate ", " i) implements
    ++ "\n\t{\n" ++ unlines (map ((++)"\t\t" . show) body) ++ "\n\t}"

instance Show Expression where
  show (TODO_E a) = a
  show (Identifier a) = a
  show (ClassId ms cv n t) = intercalate " " (map show ms ++ [show cv, n]) ++ ":" ++ show t
  show (FnId cv n t) = intercalate " " [show cv, n] ++ ":" ++ show t
  show (FnParamId n t) = show n ++ ":" ++ show t
  show (TernOp cond t f) = show cond ++ " ? " ++ show t ++ " : " ++ show f
  show (RBinOp l op r) = intercalate " " [show l, show op, show r]
  show (LBinOp op l r) = intercalate " " [show l, show op, show r]
  show (Lit a) = show a
  show (Comma a) = intercalate ", " $ map show a
  show (ParenGroup a) = "(" ++ show a ++ ")"
