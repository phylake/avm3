module Data.AS3.AST.Def (
  p
, xy
, pxy
, with_scope
, get_scope
, ScopeId(..)
, Identifiers
, ScopeTree
, M
, As3Parser
, ParseScope(..)
, BinaryOp(..)
, UnaryOp(..)
, Type(..)
, Literal(..)
, CV(..)
, ScopeMod(..)
, Statement(..)
, Expression(..)
) where

import           Control.Monad.State
import           Data.Foldable
import           Prelude hiding (foldr)
import           Text.Parsec (ParsecT, getPosition)
import           Text.Parsec.Pos (sourceLine, sourceColumn)
import qualified Data.HashTable.IO as H

p :: String -> As3Parser ()
p = liftIO . putStrLn

xy :: As3Parser String
xy = do
  pos <- getPosition
  return $ "(" ++ show (sourceLine pos) ++ ", " ++ show (sourceColumn pos) ++ ")"

pxy :: As3Parser ()
pxy = xy >>= p

{- ([class-level identifiers], [function-level identifiers]) -}
--type ScopeChain = ([String], [String])
--type M = StateT ([String], ScopeChain) IO
--type M = StateT ([ClassInterface], ScopeChain) IO

{- (params, type) -}
data ScopeId = ClassId2 [ScopeMod] Type
             | ClassFn [ScopeMod] [Type] Type -- ^ scopes, params, return
             | InstanceId [ScopeMod] Type
             | InstanceFn [ScopeMod] [Type] Type -- ^ scopes, params, return
type Identifiers = H.BasicHashTable String ScopeId
type ScopeTree = H.BasicHashTable String Identifiers

type M = StateT (IO ScopeTree, ParseScope, [String]) IO

--type ScopeIdParser = ParsecT String () IO [ScopeId]
type As3Parser = ParsecT String () M

-- ^ Swapped during parsing to alter the underlying grammar
data ParseScope = PS_Class
                | PS_Function
                | PS_TypedIds -- ^ function params, var/const ...
                deriving (Eq, Show)

with_scope :: ParseScope -> As3Parser a -> As3Parser a
with_scope new_scope action = do
  (a, old_scope, c) <- lift get
  lift $ put (a, new_scope, c)
  result <- action
  lift $ put (a, old_scope, c)
  return result

set_scope :: ParseScope -> As3Parser ()
set_scope new = do
  (a, _, c) <- lift get
  lift $ put (a, new, c)

get_scope :: As3Parser ParseScope
get_scope = do
  (a, b, c) <- lift get
  return b

is_scope :: ParseScope -> As3Parser Bool
is_scope ps2 = do
  ps1 <- get_scope
  return $ ps1 == ps2

data BinaryOp = Addition
              | Subtraction
              | Multiplication
              | Division
              | Modulo
              -- shift
              | LShift
              | RShift
              | URShift
              -- logical
              | LogicalAND
              | LogicalOR
              -- bitwise
              | BitwiseAND
              | BitwiseOR
              | BitwiseXOR
              -- relational
              | LessThan
              | GreaterThan
              | LessThanEq
              | GreaterThanEq
              | InstanceOf
              | In
              -- assignment
              | Assignment
              | PlusAssignment
              | MinusAssignment
              | MultiplicationAssignment
              | DivisionAssignment
              | ModuloAssignment
              | LShiftAssignment
              | RShiftAssignment
              | URShiftAssignment
              | BitwiseANDAssignment
              | BitwiseORAssignment
              | BitwiseXORAssignment
              -- equality
              | Equality
              | StrictEquality
              | InEquality
              | StrictInEquality

data UnaryOp = Delete
             | Void
             | TypeOf
             | Increment
             | Decrement
             | Positive
             | Negative
             | BitwiseNOT
             | LogicalNOT -- end of ECMA-262

data Type = T_int
          | T_uint
          | T_void
          | T_undefined
          | T_Number
          | T_Boolean
          | T_String
          | T_Array
          | T_Object
          | T_Vector Type
          | T_UserDefined String

data Literal = L_void
             | L_undefined
             | L_int Int
             | L_uint Int
             | L_Number String -- TODO replace with Double
             | L_Boolean Bool
             | L_String String
             | L_RegExp String

data CV = Const | Var

data ScopeMod = Public
              | Protected
              | Private
              | Internal
              | Final
              | Override
              | Static

data Statement = EmptyS
               | Block [Statement]
               | Variable [Expression]
               | Constant [Expression]
               | ExpressionStmt Expression
               | If Expression Statement
               | IfElse Expression Statement Statement
               | DoWhile Statement Expression
               | While Expression Statement
              -- | For (Maybe Expression) (Maybe Expression) (Maybe Expression) Statement
               | For (Maybe Statement) (Maybe Expression) (Maybe Expression) Statement
               | ForIn Expression Expression Statement
               | ForEach Expression Expression Statement
               | Continue (Maybe Expression)
               | Break (Maybe Expression)
               | Return (Maybe Expression)
               | With Expression Statement
               | Switch Expression Statement
               | Case Expression (Maybe Statement)
               | Default (Maybe Statement)
               
               | Package (Maybe String) [Statement]
               | Import String
               -- ^ [public] FooClass [extends Bar] [implements Baz] [body]
               | Class [ScopeMod] String (Maybe String) (Maybe [String]) [Statement]
               -- ^ [public] <name> <params> <return> <body>
               | FnDec [ScopeMod] String [Expression] Type [Statement]

data Expression = TODO_E String
                | CommentSingle String
                | CommentBlock [String]
                | Comma [Expression]
                | ParenGroup Expression
                | ObjectLiteral [Expression]
                | KeyValue Expression Expression
                -- ^ "?" is implied since it's the only ternary operator
                | TernOp Expression Expression Expression
                | RBinOp Expression BinaryOp Expression -- ^ Right associative
                | LBinOp BinaryOp Expression Expression -- ^ Left associative
                | Unary UnaryOp Expression
                -- ^ in a function arg list there are no scope modifiers and CV
                -- ^ is implicity Var
                | ClassId [ScopeMod] CV String Type
                | FnId CV String Type
                -- ^ identifier:Type
                | TypedId String Type
                | ExpressionId String
                | Lit Literal
                | Postfix Expression UnaryOp
                | New Expression
                -- ^ caller[expression]
                | ArrayAccess Expression Expression
                -- ^ caller.identifier
                | Call Expression String
                -- ^ 
--                | FnExp (Maybe Expression) [Expression] [Statement]

{-type Expression = Tree NodeData

-- not sure if I need left sub-tree. test on binary and ternary ops
data Tree a = End | Node [Tree a] a [Tree a]

instance Functor Tree where
  fmap f End = End
  fmap f (Node l m r) = Node
    (map (fmap f) l)
    (f m)
    (map (fmap f) r)

instance Foldable Tree where
  foldr f acc End = acc
  foldr f acc (Node l m r) = foldr f (f m (foldr f acc r)) l
-}
