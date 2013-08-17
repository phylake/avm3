module Data.AS3.AST.Def where

import           Control.Monad.State (StateT, liftIO)
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

data Type = T_int
          | T_uint
          | T_void
          | T_Number
          | T_Boolean
          | T_String
          | T_Array
          | T_Vector Type
          | T_UserDefined String
          deriving (Show)

data CV = Const | Var deriving (Show)

data ScopeMod = Public
              | Protected
              | Private
              | Final
              | Override
              | Static
              deriving (Show)

data Package = Package {
                         packageName :: String
                       , packageBody :: Maybe [PackageBody] -- ^ this captures the possibility of multiple classes
                       }
                       deriving (Show)

data PackageBody = PackageImport String
                 | PackageComment String
                 | PackageClass Class
                 deriving (Show)

data Class = Class {
                     classMods :: [ScopeMod]
                   , className :: String
                   , classExtend :: Maybe String
                   , classImpl :: Maybe [String]
                   , classBody :: AST
                   }
                   deriving (Show)

data ClassBody = PropertyDec [ScopeMod] Ident
               | FunctionDec [ScopeMod] FunctionSignature [Expression]
               deriving (Show)

data Ident = Ident {
                     propName :: String
                   , propType :: Type
                   }
                   deriving (Show)

data FunctionSignature = FunctionSignature {
                                             funcName :: String
                                           , funcParams :: Maybe [Ident]
                                           , funcReturn :: Type
                                           }
                                           deriving (Show)

{-
TODO start here
global functions for translation
keywords such as super
some expressions such as a new regex /\w+/
control flow statements
everything else
-}
--data AST = End | Leaf a | Node (AST a) a (AST a)
{-data ClassAST = End
              | ClassStmt    -- ^ implies static
              | ClassDec     -- ^ implies static
              | InstanceStmt
              | InstanceDec
              deriving (Show)-}

data AST = End
         | Stmt AST AST -- ^ ends in a semicolon
         | Eq AST AST
         | Id [ScopeMod] CV Ident
         | Lit Literal
         deriving (Show)

data Literal = S String
             deriving (Show)

{-instance Traversable AST where
  traverse f End = pure End
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r-}

type Expression = String

data ControlFlow = CF_if [Expression] [Expression]
                 | CF_for (Maybe Expression) (Maybe Expression) (Maybe Expression) [Expression]
                 | CF_forin Expression Expression [Expression]
                 | CF_while [Expression] [Expression]
                 | CF_switch Expression [Expression]



