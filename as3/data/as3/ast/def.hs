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
type TypeInfo = (Maybe [Type], Type)
type Identifiers = H.BasicHashTable String TypeInfo
type ScopeTree = H.BasicHashTable String Identifiers

type This = String

type M = StateT (IO ScopeTree, This, [String]) IO

--type TypeInfoParser = ParsecT String () IO [TypeInfo]
type As3Parser = ParsecT String () M

data BinaryOp = Plus
              | Minus
              | Multiplication
              | Division
              | Modulo
              | LShift
              | RShift
              | BitwiseAND
              | BitwiseOR
              | Assigment
              | PlusAssignment
              | MinusAssignment
              | MultiplicationAssignment
              | DivisionAssignment

data Type = T_int
          | T_uint
          | T_void
          | T_Number
          | T_Boolean
          | T_String
          | T_Array
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

data Ident = Ident {
                     propName :: String
                   , propType :: Type
                   }

{-
TODO start here
global functions for translation
keywords such as super
some expressions such as a new regex /\w+/
control flow statements
everything else
-}

data NodeData = Stmt
              | CommentSingle String
              | CommentBlock [String]
              | Package String
              | Import String
              | Class [ScopeMod] String (Maybe String) (Maybe [String])
              | ArgList
              | TernOp
              | BinOp BinaryOp
              | UnOp String
              | Id [ScopeMod] CV Ident
              | Lit Literal
              | If String
              | For String
              | ForIn String
              | ForEach String
              | While String
              | Switch String

type AST = Tree NodeData

data Tree a = End | Leaf a | Node (Tree a) a (Tree a)

instance Functor Tree where
  fmap f End = End
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node l m r) = Node (fmap f l) (f m) (fmap f r)

instance Foldable Tree where
  foldr f acc End = acc
  foldr f acc (Leaf a) = f a acc
  foldr f acc (Node l m r) = foldr f (f m (foldr f acc r)) l
