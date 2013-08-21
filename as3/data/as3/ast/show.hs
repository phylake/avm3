module Data.AS3.AST.Show () where

import           Data.Foldable (foldl)
import           Data.List (intersperse, intercalate)
import           Data.AS3.AST.Def

instance Show BinaryOp where
  show Plus = "+"
  show Minus = "-"
  show Multiplication = "*"
  show Division = "/"
  show Modulo = "%"
  show LShift = "<<"
  show RShift = ">>"
  show BitwiseAND = "&"
  show BitwiseOR = "|"
  show Assigment = "="
  show PlusAssignment = "+="
  show MinusAssignment = "-="
  show MultiplicationAssignment = "*="
  show DivisionAssignment = "/="

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

instance (Show a) => Show (Tree a) where
  show ast = let (Leaf b) = Data.Foldable.foldl f End (fmap show ast) in b
    where
      f :: Tree String -> String -> Tree String
      f End acc = Leaf acc
      f (Leaf a) acc = Leaf $ a ++ acc
      f (Node l m r) acc = Node
                             (Data.Foldable.foldl f (Leaf acc) l)
                             m
                             (Data.Foldable.foldl f End r)

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

instance Show Ident where
  show (Ident a b) = a ++ ":" ++ show b

instance Show NodeData where
  show (Package a) = "package" ++ maybe "" ((++)" ") a ++ " {\n"
  show (Import a) = "\nimport " ++ a ++ ";"
  show (Class scopes name extends implements) =
    intercalate " " (map show scopes) ++ " class"
    ++ maybe "" ((++) " extends ") extends
    ++ maybe "" (\i -> " implements " ++ intercalate ", " i) implements
  show ArgList = ", "
  show Stmt = ";\n"
  show Null = "\n"
  show TernOp = "?"
  show (BinOp a) = show a
  show (Lit a) = show a
  show (If a) = a
  show (For a) = a
  show (UnOp a) = a
  show (ForIn a) = a
  show (ForEach a) = a
  show (While a) = a
  show (Switch a) = a
  show (Id a b c) = intercalate " " $ map show a ++ [show b, show c]
