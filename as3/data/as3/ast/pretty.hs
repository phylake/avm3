module Data.AS3.AST.Pretty (PrettyAs(..), inBlock) where

import           Control.Monad (liftM, liftM2)
import           Data.AS3.AST.Def hiding (M,p)
import           Data.AS3.AST.Show
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

{-instance PrettyAs BinaryOp where
  toAs3 = return . show
instance PrettyAs Literal where
  toAs3 = return . show
instance PrettyAs Type where
  toAs3 = return . show
instance PrettyAs ScopeMod where
  toAs3 = return . show
instance PrettyAs CV where
  toAs3 = return . show-}

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
  toAs3 (For me1 me2 me3 s) = return "For"
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
  toAs3 (ObjectLiteral kvps) = do
    kvps <- mapM toAs3 kvps
    close <- p "}"
    return $ "{" ++ intercalate ", " kvps ++ close
  toAs3 (KeyValue k v) = do
    k <- toAs3 k
    v <- toAs3 v
    return $ k ++ ":" ++ v
  toAs3 (ClassId ms cv n t) = p $
    intercalate " " (map show ms ++ [show cv, n]) ++ ":" ++ show t ++ ";"
  toAs3 (FnId cv n t) = return $ intercalate " " [show cv, n] ++ ":" ++ show t
  toAs3 (TypedId n t) = return $ n ++ ":" ++ show t
  toAs3 (ExpressionId a) = return a
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
  toAs3 (Lit a) = return $ show a
  toAs3 (Comma a) = liftM (intercalate ", ") $ mapM toAs3 a
  toAs3 (ParenGroup a) = do
    a' <- toAs3 a
    return $ "(" ++ a' ++ ")"

