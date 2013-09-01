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

instance Show AST where
  show (TODO a) = a
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
  show (Ident ms cv n t) = intercalate " " (map show ms ++ [maybe "" show cv, n]) ++ ":" ++ show t
  show (TernOp cond t f) = show cond ++ " ? " ++ show t ++ " : " ++ show f
  show (BinOp l op r) = intercalate " " [show l, show op, show r]
  show (RBinOp op l r) = intercalate " " [show l, show op, show r]
  show (LBinOp op l r) = intercalate " " [show l, show op, show r]
  show (Lit a) = show a
  show (CommaExpression a) = intercalate ", " $ map show a
  show (ParenGroup a) = "(" ++ show a ++ ")"
  --show (If a) = a
  --show (For a) = a
  --show (UnOp a) = a
  --show (ForIn a) = a
  --show (ForEach a) = a
  --show (While a) = a
  --show (Switch a) = a
