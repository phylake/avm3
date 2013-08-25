module Data.AS3.AST.Def where

import           Control.Monad.State (StateT, liftIO)
import           Data.Foldable
import           Prelude hiding (foldr)
import           Text.Parsec (ParsecT)
import qualified Data.HashTable.IO as H

p :: String -> As3Parser ()
p = liftIO . putStrLn

{- ([class-level identifiers], [function-level identifiers]) -}
--type ScopeChain = ([String], [String])
--type M = StateT ([String], ScopeChain) IO
--type M = StateT ([ClassInterface], ScopeChain) IO

{- (params, type) -}
data ScopeId = ClassId [ScopeMod] Type
             | ClassFn [ScopeMod] [Type] Type -- ^ scopes, params, return
             | InstanceId [ScopeMod] Type
             | InstanceFn [ScopeMod] [Type] Type -- ^ scopes, params, return
type Identifiers = H.BasicHashTable String ScopeId
type ScopeTree = H.BasicHashTable String Identifiers

type This = String
type NothingJustNeededToRecompile = [String]

type M = StateT (IO ScopeTree, This, NothingJustNeededToRecompile) IO

--type ScopeIdParser = ParsecT String () IO [ScopeId]
type As3Parser = ParsecT String () M

data BinaryOp = Addition -- ^ +
              | Subtraction -- ^ -
              | Multiplication -- ^ *
              | Division -- ^ /
              | Modulo -- ^ %
              -- shift
              | LShift -- ^ <<
              | RShift -- ^ >>
              | URShift -- ^ >>
              -- TODO logical
              | LogicalAND -- ^ &&
              | LogicalOR -- ^ ||
              -- TODO bitwise
              | BitwiseAND -- ^ &
              | BitwiseOR -- ^ |
              | BitwiseXOR -- ^ ^
              -- relational
              | LessThan -- ^ <
              | GreaterThan -- ^ >
              | LessThanEq -- ^ <=
              | GreaterThanEq -- ^ >=
              | InstanceOf -- ^ instanceof
              | In -- ^ in
              -- assignment
              | Assignment -- ^ =
              | PlusAssignment -- ^ +=
              | MinusAssignment -- ^ -=
              | MultiplicationAssignment -- ^ *=
              | DivisionAssignment -- ^ /=
              -- equality
              | Equality -- ^ ==
              | StrictEquality -- ^ ===
              | InEquality -- ^ !=
              | StrictInEquality -- ^ !==

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
             | L_Number Double
             | L_Boolean Bool
             | L_String String
             | L_RegExp String

data CV = Const | Var

data ScopeMod = Public
              | Protected
              | Private
              | Final
              | Override
              | Static

{-
TODO start here
global functions for translation
keywords such as super
some expressions such as a new regex /\w+/
control flow statements
everything else
-}

data AST = TODO String
         | CommentSingle String
         | CommentBlock [String]
         | ParenGroup AST
         | Package (Maybe String) [AST]
         | Import String
         -- ^ [public] FooClass [extends Bar] [implements Baz] [body]
         | Class [ScopeMod] String (Maybe String) (Maybe [String]) [AST]
         | Function [ScopeMod] 
         -- ^ "?" is implied since it's the only ternary operator
         | TernOp AST AST AST
         | BinOp AST BinaryOp AST
         | UnaryX UnaryOp AST
         -- ^ in a function arg list there are no scope modifiers and CV is
         -- ^ implicity Var
         | Ident [ScopeMod] (Maybe CV) String Type
         | Lit Literal
         -- ^ conditional, body
         | If AST [AST]
         | For String
         | ForIn String
         | ForEach String
         | While String
         | Switch String
         | PostfixX AST UnaryOp
         | NewX AST

{-type AST = Tree NodeData

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
