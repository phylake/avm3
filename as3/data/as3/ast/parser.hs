module Data.AS3.AST.Parser (parser) where

import           Control.Monad.State
import           Data.AS3.AST.Def
import           Data.AS3.AST.Prims
import           Data.AS3.AST.Scope
import           Data.AS3.AST.ThirdParty
import           Data.List (intersperse)
import           Text.Parsec
import           Util.File (recurseDirs)
import           Util.Misc (t31)
import qualified Control.Applicative as A
import qualified Data.HashTable.IO as H

parser :: As3Parser Package
parser = package

class_id :: As3Parser String
class_id = type_whitelist <?> "class/interface def"

type_whitelist :: As3Parser String
type_whitelist = do
  ht <- gets t31 >>= liftIO
  list <- map fst A.<$> (liftIO $ H.toList ht)
  --p$ show list
  foldl plusfold (string $ head list) (drop 1 list)

type_id :: As3Parser Type
type_id =
      (liftM T_UserDefined $ try type_whitelist)
  <|> (string "int" >> return T_int)
  <|> (string "uint" >> return T_uint)
  <|> (string "void" >> return T_void)
  <|> (string "Number" >> return T_Number)
  <|> (string "Boolean" >> return T_Boolean)
  <|> (string "String" >> return T_String)
  <|> (string "Array" >> return T_Array)
  <|> (string "Vector.<" *> type_id <* string ">" >>= return . T_Vector)
  <?> "type id"

type_defaults :: Maybe String -> Type -> Maybe String
type_defaults init T_int =             Just $ maybe "0"     id init
type_defaults init T_Number =          Just $ maybe "0.0"   id init
type_defaults init T_Boolean =         Just $ maybe "false" id init
type_defaults init T_String =          Just $ maybe ""      id init
type_defaults init (T_Vector _) =      Just $ maybe "null"  id init
type_defaults init (T_UserDefined _) = Just $ maybe "null"  id init

{-scope_id :: As3Parser String
scope_id = do
  (_, ((k:klasses), fns)) <- get
  try $foldl plusfold (string k) (klasses ++ fns)
  <?> "scope id"-}

function_id :: As3Parser String
function_id = many1 $ alphaNum <|> char '_'

var_id :: As3Parser String
var_id = many1 $ alphaNum <|> char '_'

scope_mods :: As3Parser [ScopeMod]
scope_mods = tok (scope_mod `sepEndBy1` (many1 $ char ' ')) <?> "scope modifiers"
  where
    scope_mod :: As3Parser ScopeMod
    scope_mod = (try $
          (string "public"    >> return Public)
      <|> (string "protected" >> return Protected)
      <|> (string "private"   >> return Private)
      <|> (string "final"     >> return Final)
      <|> (string "override"  >> return Override)
      <|> (string "static"    >> return Static)) <?> "scope modifier"

{-----------------
   PACKAGE-LEVEL
-----------------}

package_id :: As3Parser String
package_id = many1 lower `sepBy1` char '.' >>= dots

package :: As3Parser Package
package = do
  string "package" <* ss
  name <- option "" package_id
  packageBody <- between_braces $ optionMaybe body
  spaces
  return Package {
    packageName = name
  , packageBody = packageBody
  }
  <?> "package"
  where
    body = many $spaces *> package_dec <* spaces

package_dec :: As3Parser PackageBody
package_dec =
      (liftM PackageImport imporT)
  <|> (liftM PackageClass as3_class)
  <?> "package declaration"

imporT :: As3Parser String
imporT = do
  string "import "
  package <- package_id
  semi
  return package
  <?> "import stmt"

{-----------------
   CLASS-LEVEL
-----------------}

as3_class :: As3Parser Class
as3_class = do
  spaces
  scopes <- scope_mods
  tok $ string "class"
  name <- tok class_id
  extends <- optionMaybe $ string "extends " *> tok class_id -- make sure "extends" is the first match in order to fail fast and return Nothing
  implements <- optionMaybe $ string "implements " *> csv class_id -- make sure "implements" is the first match in order to fail fast and return Nothing
  decs <- between_braces class_body
  return Class {
    classMods = scopes
  , className = name
  , classExtend = extends
  , classImpl = implements
  , classBody = decs
  }

{-
might be able to generate list of AST and fold them into a single one
-}

class_body :: As3Parser AST
class_body = undefined

{-class_body :: As3Parser (AST -> AST)
class_body = do
  m <- optionMaybe (try class_expression)
  case m of
    Nothing -> return End
    Just a -> return Stmt a End-}

{-class_body :: (AST -> AST) -> As3Parser AST
class_body f = do
  m <- optionMaybe (try class_expression)
  case m of
    Nothing -> f End
    Just a -> return Stmt a End-}

class_expression :: As3Parser AST
class_expression = try assignment <|> class_stmt

class_declaration :: As3Parser AST
class_declaration = undefined

class_stmt :: As3Parser AST
class_stmt = try class_ident

assignment :: As3Parser AST
assignment = class_ident >>= eq

eq :: AST -> As3Parser AST
eq lhs = spaces *> char '=' *> spaces *> rhs <* optional semi >>= return . Eq lhs

rhs :: As3Parser AST
rhs = liftM (Lit . S) (try string_literal)

class_ident :: As3Parser AST
class_ident = liftM3 Id scope_mods cv ident

cv :: As3Parser CV
cv = tok $
      (string "var" >> return Var)
  <|> (string "const" >> return Const)

ident :: As3Parser Ident
ident = liftM2 Ident (tok var_id) (char ':' >> ss >> type_id)

class_property :: As3Parser ClassBody
class_property = do
  spaces
  scopes <- scope_mods
  prop <- function_property
  --push_class_scope $propName prop
  return $ PropertyDec scopes prop
  <?> "class-level property"

class_function :: As3Parser ClassBody
class_function = do
  spaces
  scopes <- scope_mods
  signature <- function_signature True
  body <- function_body
  --exit_fn
  return $ FunctionDec scopes signature body
  <?> "class-level function"

function_signature :: Bool -> As3Parser FunctionSignature
function_signature alters_scope = undefined
{-function_signature :: Bool -> As3Parser FunctionSignature
function_signature alters_scope = do
  spaces
  string "function "
  notFollowedBy accessor
  name <- function_id
  p$ "in function " ++ name
  props <- do
    m <- between_parens $ optionMaybe props
    return $ map (uncurry Ident) A.<$> m
  if alters_scope
    then case props of
      Just props -> mapM (push_fn_scope . propName) props
      Nothing -> return [()]
    else return [()]
  returnT <- ss *> char ':' <* ss >> type_id
  return FunctionSignature {
    funcName = name
  , funcParams = props
  , funcReturn = returnT
  }
  <?> "function signature"
  where
    accessor :: As3Parser String
    accessor = string "get" <|> string "set" <?> "accessor"
    props :: As3Parser [(String, Type)]
    props = csv (ident literal) <?> "function params"-}

function_body :: As3Parser [Expression]
function_body = between_braces $many function_expression

{-----------------
  CONTROL FLOW
-----------------}

control_flow_stmt :: As3Parser ControlFlow
control_flow_stmt =
      if_stmt
  <|> while_stmt
  <?> "control flow stmt"

if_stmt :: As3Parser ControlFlow
if_stmt = commonCF "if" CF_if

while_stmt :: As3Parser ControlFlow
while_stmt = commonCF "while" CF_while

commonCF :: String
         -> ([Expression] -> [Expression] -> ControlFlow)
         -> As3Parser ControlFlow
commonCF str f = do
  string str <* ss
  condition <- between_parens $ many1 expression
  inline <- optionMaybe expression
  body <- case inline of
    Nothing -> between_braces $ many expression
    Just exp -> return [exp]
  return $f condition body

{-----------------
  FUNCTION-LEVEL
-----------------}

function_call :: As3Parser String
function_call = do
  --notFollowedBy keyword
  name <- function_id
  --liftIO.putStrLn $"\tfunction_call name " ++ name
  params <- between_parens $ csv unary_statement
  --return $name ++ (concat $intersperse ", " params)
  let ret = name ++ (concat $ intersperse ", " params)
  liftIO.putStrLn $ "function_call RETURNED " ++ ret
  return ret

{-
  valid expression on the right side of
  assignment or as a function parameter
-}
function_assignment :: As3Parser String
function_assignment = try literal <|> unary_statement

unary_statement :: As3Parser String
unary_statement = do
  --liftIO.putStrLn $"\tunary_statement"
  prefix <- option "" unary_op_pre
  identifier <- scope_or_func `sepBy1` char '.' >>= dots
  liftIO.putStrLn $"\tunary_statement identifier " ++ identifier
  postfix <- option "" unary_op_post
  return $prefix ++ identifier ++ postfix
  where
    scope_or_func :: As3Parser String
-- rev 1
    --scope_or_func = try function_call <|> scope_id
-- rev 2
    scope_or_func = do
      ident <- function_id
      call <- optionMaybe $do
        maybe_params <- between_parens $optionMaybe $csv function_assignment
        return $maybe "()" concat maybe_params
      return $ident ++ maybe "" id call

binary_statement :: As3Parser String
binary_statement = do
  lhs <- unary_statement
  op <- binary_op
  rhs <- function_assignment
  return $lhs ++ " " ++ op ++ " " ++ rhs

function_property :: As3Parser Ident
function_property = undefined
{-function_property :: As3Parser Ident
function_property = do
  (string "var" <|> string "const") <* ss
  (name, t) <- ident function_assignment
  liftIO.putStrLn $"\tfunction_property " ++ name
  push_fn_scope name
  semi
  return Ident {
    propName = name
  , propType = t
  {-, propValue = type_defaults v t-}
  }-}

{-
(null)
(this)
-}

expression :: As3Parser Expression
expression = undefined

function_expression :: As3Parser String
function_expression = undefined

boolean_expression :: As3Parser String
boolean_expression = undefined
--boolean_expression = (try binary_statement <|> unary_statement) `sepBy1` boolean_op

unary_op_pre :: As3Parser String
unary_op_pre =
      string "-"
  <|> string "++"
  <|> string "--"
  <|> string "^"
  <|> string "!"
  <|> string "~"
  <?> "unary PREFIX op"

unary_op_post :: As3Parser String
unary_op_post =
      string "++"
  <|> string "--"
  <?> "unary POSTFIX op"

boolean_op :: As3Parser String
boolean_op =
      string "<"
  <|> string ">"
  <|> string "<="
  <|> string ">="
  <|> string "=="
  <|> string "!="
 -- <|> string "==="
 -- <|> string "!=="
  <|> string "&&"
  <|> string "||"
  <?> "boolean op"

binary_op :: As3Parser String
binary_op =
      boolean_op
  <|> string "+"
  <|> string "-"
  <|> string "*"
  <|> string "/"
  <|> string "%"
  <|> string "<<"
  <|> string ">>"
  <|> string ">>>"
  <|> string "&"
  <|> string "|"
  <|> string "="
  <|> string "+="
  <|> string "-="
  <|> string "*="
  <|> string "/="
 -- <|> string "%="
 -- <|> string "<<="
 -- <|> string ">>="
 -- <|> string ">>>="
 -- <|> string "&="
 -- <|> string "|="
 -- <|> string "^="
  <?> "binary op"

ternary_op :: As3Parser String
ternary_op = undefined





line_terminator :: As3Parser String
line_terminator = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\r"
  <|> string "\n"

identifier :: As3Parser String
identifier =
      comments
  <|> line_terminator
  <|> as3Token

comments :: As3Parser String
comments = undefined

as3Token :: As3Parser String
as3Token =
      punctuator
  <|> numeric_literal
  <|> string_literal

reserved_word :: As3Parser String
reserved_word =
      keyword
  <|> null_literal
  <|> boolean_literal

keyword :: As3Parser String
keyword =
    string "break"
 -- <|> string "do"
 -- <|> string "instanceof"
 -- <|> string "typeof"
  <|> string "case"
  <|> string "else"
  <|> string "new"
  <|> string "var"
  <|> string "catch"
  <|> string "finally"
  <|> string "return"
  <|> string "void"
  <|> string "continue"
  <|> string "for"
  <|> string "switch"
  <|> string "while"
  <|> string "debugger"
  <|> string "function"
  <|> string "this"
  <|> string "with"
  <|> string "default"
  <|> string "if"
  <|> string "throw"
  <|> string "delete"
  <|> string "in"
  <|> string "try"
  <|> string "class"
  <|> string "enum"
  <|> string "extends"
  <|> string "super"
  <|> string "const"
 -- <|> string "export"
 -- <|> string "import"
 -- <|> string "implements"
 -- <|> string "let"
 -- <|> string "private"
 -- <|> string "public"
 -- <|> string "yield"
 -- <|> string "interface"
 -- <|> string "package"
 -- <|> string "protected"
 -- <|> string "static"
  <?> "keyword"

punctuator :: As3Parser String
punctuator =
      string "{"
  <|> string "}"
  <|> string "("
  <|> string ")"
  <|> string "["
  <|> string "]"
  <|> string "."
  <|> string ";"
  <|> string ","
  <|> string "<"
  <|> string ">"
  <|> string "<="
  <|> string ">="
  <|> string "=="
  <|> string "!="
  <|> string "==="
  <|> string "!=="
  <|> string "+"
  <|> string "-"
  <|> string "*"
  <|> string "%"
  <|> string "++"
  <|> string "--"
  <|> string "<<"
  <|> string ">>"
  <|> string ">>>"
  <|> string "&"
  <|> string "|"
  <|> string "^"
  <|> string "!"
  <|> string "~"
  <|> string "&&"
  <|> string "||"
  <|> string "?"
  <|> string ":"
  <|> string "="
  <|> string "+="
  <|> string "-="
  <|> string "*="
  <|> string "%="
  <|> string "<<="
  <|> string ">>="
  <|> string ">>>="
  <|> string "&="
  <|> string "|="
  <|> string "^="
  <?> "punctuator"

div_punctuator :: As3Parser String
div_punctuator = string "/" <|> string "/="

literal :: As3Parser String
literal =
      null_literal
  <|> boolean_literal
  <|> numeric_literal
  <|> string_literal
  <|> regex_literal
  <?> "literal"

null_literal :: As3Parser String
null_literal = string "null"

boolean_literal :: As3Parser String
boolean_literal = string "true" <|> string "false" <?> "boolean literal"

numeric_literal :: As3Parser String
numeric_literal = try decimal_literal <|> hex_integer_literal <?> "numeric literal"

decimal_literal :: As3Parser String
decimal_literal =
      try (decimal_integer_literal >> string "." >> try decimal_digits)
  <|> try (char '.' >> decimal_digits)
  <|> decimal_integer_literal

decimal_integer_literal :: As3Parser String
decimal_integer_literal =
      string "0"
  <|> (non_zero_digit >> try decimal_digits)

decimal_digits :: As3Parser String
decimal_digits = many1 decimal_digit

decimal_digit :: As3Parser Char
decimal_digit = char '0' <|> non_zero_digit

non_zero_digit :: As3Parser Char
non_zero_digit =
      char '1'
  <|> char '2'
  <|> char '3'
  <|> char '4'
  <|> char '5'
  <|> char '6'
  <|> char '7'
  <|> char '8'
  <|> char '9'

hex_integer_literal :: As3Parser String
hex_integer_literal =
      (string "0x" >> many1 hex_digit)
  <|> (string "0X" >> many1 hex_digit)

hex_digit :: As3Parser Char
hex_digit =
      decimal_digit
  <|> char 'a'
  <|> char 'b'
  <|> char 'c'
  <|> char 'd'
  <|> char 'e'
  <|> char 'f'
  <|> char 'A'
  <|> char 'B'
  <|> char 'C'
  <|> char 'D'
  <|> char 'E'
  <|> char 'F'

string_literal :: As3Parser String
string_literal = do
  q <- quote -- ' or "
  str <- many $ satisfy (/= q)
  quote
  return $"\"" ++ str ++ "\""

quote :: As3Parser Char
quote = char '\'' <|> char '\"'
--quote :: As3Parser String
--quote = string "\'" <|> string "\""

regex_literal :: As3Parser String
regex_literal = undefined
