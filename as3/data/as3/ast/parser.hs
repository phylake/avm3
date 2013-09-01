module Data.AS3.AST.Parser (parser) where

import           Control.Monad.State
import           Data.AS3.AST.Def
import           Data.AS3.AST.Prims
import           Data.AS3.AST.Scope
import           Data.AS3.AST.ThirdParty
import           Data.AS3.AST.Show
import           Data.AS3.AST.Grammar.Lexicon
import           Data.List (intersperse)
import           Text.Parsec
import           Util.File (recurseDirs)
import           Util.Misc (t31)
import qualified Control.Applicative as A
import qualified Data.HashTable.IO as H

parser :: As3Parser AST
parser = package

class_id :: As3Parser String
class_id = type_whitelist <?> "class/interface def"

type_whitelist :: As3Parser String
type_whitelist = do
  ht <- gets t31 >>= liftIO
  list <- map fst A.<$> (liftIO $ H.toList ht)
  --p$ "type_whitelist\n" ++ show list
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

package :: As3Parser AST
package = do
  string "package" <* ss
  name <- optionMaybe package_id
  body <- between_braces $ many package_body
  spaces
  return $ Package name body
  <?> "package"

package_body :: As3Parser AST
package_body = tok $ try imporT <|> as3_class

imporT :: As3Parser AST
imporT = liftM Import (string "import " *> package_id <* semi) <?> "import"

{-----------------
   CLASS-LEVEL
-----------------}

as3_class :: As3Parser AST
as3_class = do
  spaces
  scopes <- scope_mods
  tok $ string "class"
  name <- tok class_id
  extends <- optionMaybe $ string "extends " *> class_id <* ss -- make sure "extends" is the first match in order to fail fast and return Nothing
  implements <- optionMaybe $ string "implements " *> csv class_id -- make sure "implements" is the first match in order to fail fast and return Nothing
  decs <- between_braces $ many class_body
  return $ Class scopes name extends implements decs

class_body :: As3Parser AST
class_body =
      try assignment_expression
  <|> try class_ident

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

function_expression :: As3Parser AST
function_expression = fail "function_expression"

-- $7.6 Identifier Names and Identifiers

identifier :: As3Parser AST
identifier = liftM Identifier $ many1 anyChar

ident :: As3Parser AST
ident = liftM4 Ident scope_mods cv var_id (ss *> char ':' *> ss *> type_id <* ss)
{-ident = do
  ast <- liftM4 Ident scope_mods cv var_id (ss *> char ':' *> ss *> type_id <* ss)
  p$ "ident " ++ show ast
  return ast-}

-- TODO classify class id, function id so there's no Maybe
cv :: As3Parser (Maybe CV)
cv = optionMaybe $
      (string "var " >> return Var)
  <|> (string "const " >> return Const)

identifier_name :: As3Parser AST
identifier_name = undefined

-- $11.1 Primary Expressions

primary_expression :: As3Parser AST
primary_expression =
      (liftM TODO (try $ string "this"))
  <|> try ident
  <|> try (liftM ParenGroup $ between_parens expression)
  <|> liftM TODO literal
  <?> "primary_expression"

array_literal :: As3Parser AST
array_literal = undefined

element_list :: As3Parser AST
element_list = undefined

object_literal :: As3Parser AST
object_literal = liftM ObjectLiteralX $ between_braces kvps where
  kvps :: As3Parser [AST]
  kvps = property_assignment `sepBy` comma
  
  property_assignment :: As3Parser AST
  property_assignment = undefined

  property_name :: As3Parser AST
  property_name =
        try identifier_name
    <|> try (liftM (Lit . L_String) string_literal)
    <|> try (liftM (Lit . L_String) numeric_literal)

-- $11.2 Left-Hand-Side Expressions

member_expression :: As3Parser AST
member_expression =
      try primary_expression
  <|> {-try-} function_expression
  -- TODO

new_expression :: As3Parser AST
new_expression =
  try (liftM NewX $ string "new " *> new_expression)
  <|> member_expression
  <?> "new expression"

call_expression :: As3Parser AST
call_expression = fail "call_expression"

arguments :: As3Parser [AST]
arguments = between_parens argument_list

argument_list :: As3Parser [AST]
argument_list = csv assignment_expression

lhs_expression :: As3Parser AST
lhs_expression = try new_expression <|> call_expression <?> "lhs_expression"

-- $11.3 Postfix Expressions

postfix_expression :: As3Parser AST
postfix_expression =
  try (liftM2 PostfixX (lhs_expression <* ss) unary_expression_post)
  <|> lhs_expression
  <?> "postfix_expression"
  where
    unary_expression_post :: As3Parser UnaryOp
    unary_expression_post =
          (string "++" >> return Increment)
      <|> (string "--" >> return Decrement)
      <?> "unary POSTFIX op"

-- $11.4 Unary Operators

unary_expression :: As3Parser AST
unary_expression =
      try (liftM2 UnaryX unary_expression_pre unary_expression)
  <|> postfix_expression
  where
    unary_expression_pre :: As3Parser UnaryOp
    unary_expression_pre =
          (string "delete" >> return Delete)
      <|> (string "void" >> return Void)
      <|> (string "typeof" >> return TypeOf)
      <|> (string "++" >> return Increment)
      <|> (string "--" >> return Decrement)
      <|> (string "+" >> return Positive)
      <|> (string "-" >> return Negative)
      <|> (string "~" >> return BitwiseNOT)
      <|> (string "!" >> return LogicalNOT)
     -- <|> (string "^" >> return BitwiseXOR)
      <?> "unary PREFIX op"

-- $11.5 Multiplicative Operators

multiplicative_expression :: As3Parser AST
multiplicative_expression =
  chainl1 (tok unary_expression) multiplicative_op
  <?> "multiplicative expression"
  where
    multiplicative_op :: As3Parser (AST -> AST -> AST)
    multiplicative_op =
          (linkL Multiplication)
      <|> (linkL Division)
      <|> (linkL Modulo)
      <?> "multiplicative operator"

-- $11.6 Additive Operators

additive_expression :: As3Parser AST
additive_expression =
  chainl1 (tok multiplicative_expression) additive_op
  <?> "additive expression"
  where
    additive_op :: As3Parser (AST -> AST -> AST)
    additive_op =
          (linkL Addition)
      <|> (linkL Subtraction)
      <?> "additive operator"

-- $11.7 Bitwise Shift Operators

shift_expression :: As3Parser AST
shift_expression =
  chainl1 (tok additive_expression) shift_op
  <?> "shift expression"
  where
    shift_op :: As3Parser (AST -> AST -> AST)
    shift_op =
          (linkL LShift)
      <|> (linkL RShift)
      <|> (linkL URShift)
      <?> "bitwise shift operator"

-- $11.8 Relational Operators

relational_expression :: As3Parser AST
relational_expression =
  chainl1 (tok shift_expression) relational_op <?> "relational expression"
  where
    relational_op :: As3Parser (AST -> AST -> AST)
    relational_op =
          (linkL LessThan)
      <|> (linkL GreaterThan)
      <|> (linkL LessThanEq)
      <|> (linkL GreaterThanEq)
      <|> (linkL InstanceOf)
      <|> (linkL In)
      <?> "relational operator"

-- $11.9 Equality Operators

equality_expression :: As3Parser AST
equality_expression =
  chainl1 (tok relational_expression) equality_op <?> "equality expression"
  where
    equality_op :: As3Parser (AST -> AST -> AST)
    equality_op =
          (linkL Equality)
      <|> (linkL StrictEquality)
      <|> (linkL InEquality)
      <|> (linkL StrictInEquality)
      <?> "equality operator"

-- $11.10 Binary Bitwise Operators

bitwiseAND_expression :: As3Parser AST
bitwiseAND_expression =
  chainl1 (tok equality_expression) op <?> "bitwise AND expression"
  where
    op :: As3Parser (AST -> AST -> AST)
    op = linkL BitwiseAND

bitwiseXOR_expression :: As3Parser AST
bitwiseXOR_expression =
  chainl1 (tok bitwiseAND_expression) op <?> "bitwise XOR expression"
  where
    op :: As3Parser (AST -> AST -> AST)
    op = linkL BitwiseXOR

bitwiseOR_expression :: As3Parser AST
bitwiseOR_expression =
  chainl1 (tok bitwiseXOR_expression) (try op) <?> "bitwise OR expression"
  where
    op :: As3Parser (AST -> AST -> AST)
    op = linkL BitwiseOR

-- $11.11 Binary Logical Operators

logicalAND_expression :: As3Parser AST
logicalAND_expression =
  chainl1 (tok bitwiseOR_expression) op <?> "logical AND expression"
  where
    op :: As3Parser (AST -> AST -> AST)
    op = linkL LogicalAND

logicalOR_expression :: As3Parser AST
logicalOR_expression =
   chainl1 (tok logicalAND_expression) op <?> "logical OR expression"
  where
    op :: As3Parser (AST -> AST -> AST)
    op = linkL LogicalOR

-- $11.12 Conditional Operator ( ? : )

conditional_expression :: As3Parser AST
conditional_expression =
  try (liftM3
         TernOp
         (tok logicalOR_expression <* tok (string "?"))
         (tok assignment_expression <* tok (string ":")) -- true
         (tok assignment_expression)) -- false
  <|> logicalOR_expression
  <?> "conditional expression"

-- $11.13 Assignment Operators

assignment_expression :: As3Parser AST
assignment_expression =
  try (liftM3 BinOp
         (tok lhs_expression)
         (tok assignment_op)
         assignment_expression)
  <|> conditional_expression
  <?> "assignment expression"
  where
    assignment_op :: As3Parser BinaryOp
    assignment_op =
          (string (show Assignment) >> return Assignment)
      <|> (string (show PlusAssignment) >> return PlusAssignment)
      <|> (string (show MinusAssignment) >> return MinusAssignment)
      <|> (string (show MultiplicationAssignment) >> return MultiplicationAssignment)
      <|> (string (show DivisionAssignment) >> return DivisionAssignment)
      <|> (string (show ModuloAssignment) >> return ModuloAssignment)
      <|> (string (show LShiftAssignment) >> return LShiftAssignment)
      <|> (string (show RShiftAssignment) >> return RShiftAssignment)
      <|> (string (show URShiftAssignment) >> return URShiftAssignment)
      <|> (string (show BitwiseANDAssignment) >> return BitwiseANDAssignment)
      <|> (string (show BitwiseORAssignment) >> return BitwiseORAssignment)
      <|> (string (show BitwiseXORAssignment) >> return BitwiseXORAssignment)
      <?> "assignment operator"

expression :: As3Parser AST
expression = liftM Expression (assignment_expression `sepBy` comma)

-- $Miscellaneous

class_ident :: As3Parser AST
class_ident = ident

-- $Chain links

linkR :: BinaryOp -> As3Parser (AST -> AST -> AST)
linkR = linkCommon RBinOp

linkL :: BinaryOp -> As3Parser (AST -> AST -> AST)
linkL = linkCommon LBinOp

linkCommon :: (BinaryOp -> AST -> AST -> AST)
           -> BinaryOp
           -> As3Parser (AST -> AST -> AST)
linkCommon f op = try $ tok pop >> notFollowedBy pop >> return (f op)
  where
    pop = string (show op)

