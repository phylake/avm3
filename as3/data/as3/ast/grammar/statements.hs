module Data.AS3.AST.Grammar.Statements where

import           Control.Monad
import           Data.AS3.AST.Def
import           Data.AS3.AST.Grammar.Lexicon
import           Data.AS3.AST.Grammar.Expressions
import           Data.AS3.AST.Prims
import           Data.AS3.AST.ThirdParty
import           Text.Parsec

-- $Package-level

package_id :: As3Parser String
package_id = many1 lower `sepBy1` char '.' >>= dots

package :: As3Parser Statement
package = do
  string "package" <* ss
  name <- optionMaybe package_id
  body <- between_braces $ many $ tok package_body
  return $ Package name body
  <?> "package"

package_body :: As3Parser Statement
package_body = try imporT <|> as3_class

imporT :: As3Parser Statement
imporT = liftM Import (string "import " *> package_id <* semi) <?> "import"

-- $Class-level

as3_class :: As3Parser Statement
as3_class = do
  scopes <- scope_mods
  tok $ string "class"
  name <- tok class_id
  extends <- optionMaybe $ string "extends " *> class_id <* ss -- make sure "extends" is the first match in order to fail fast and return Nothing
  implements <- optionMaybe $ string "implements " *> csv class_id -- make sure "implements" is the first match in order to fail fast and return Nothing
  --body <- between_braces $ many class_body       for [Expression]
  body <- between_braces $ many $ tok (statement <* optional semi)
  return $ Class scopes name extends implements body

class_body :: As3Parser Expression
class_body =
      try assignment_expression
  <|> try class_ident


class_ident :: As3Parser Expression
class_ident = ident

{-class_body :: As3Parser (Expression -> Expression)
class_body = do
  m <- optionMaybe (try class_expression)
  case m of
    Nothing -> return End
    Just a -> return Stmt a End-}

{-class_body :: (Expression -> Expression) -> As3Parser Expression
class_body f = do
  m <- optionMaybe (try class_expression)
  case m of
    Nothing -> f End
    Just a -> return Stmt a End-}

-- $ECMA

statement :: As3Parser Statement
statement =
      try block_statement
  <|> try variable_statement
  <|> try constant_statement -- ^ not part of ECMA-262
  <|> try empty_statement
  <|> try expression_statement
  <|> try if_statement
  <|> try iteration_statement
  <|> try continue_statement
  <|> try break_statement
  <|> try return_statement
  <|> try with_statement
  <|> try labelled_statement
  <|> try switch_statement
  <|> try throw_statement
  <|>     try_statement

block_statement :: As3Parser Statement
block_statement = liftM Block $ between_braces $ many statement

variable_statement :: As3Parser Statement
variable_statement = string "var " *> liftM2 Variable assignment_expression (return Nothing)

constant_statement :: As3Parser Statement
constant_statement = string "const " *> liftM2 Constant assignment_expression (return Nothing)

empty_statement :: As3Parser Statement
empty_statement = semi *> return EmptyStatement

expression_statement :: As3Parser Statement
expression_statement = do
  notFollowedBy $ char '{'
  notFollowedBy $ string "function"
  liftM ExpressionStmt expression

if_statement :: As3Parser Statement
if_statement = liftM2 If
  (string "if" *> between_parens expression)
  (between_braces statement)

iteration_statement :: As3Parser Statement
iteration_statement = undefined

continue_statement :: As3Parser Statement
continue_statement = undefined

break_statement :: As3Parser Statement
break_statement = undefined

return_statement :: As3Parser Statement
return_statement = undefined

with_statement :: As3Parser Statement
with_statement = undefined

labelled_statement :: As3Parser Statement
labelled_statement = undefined

switch_statement :: As3Parser Statement
switch_statement = undefined

throw_statement :: As3Parser Statement
throw_statement = undefined

try_statement :: As3Parser Statement
try_statement = undefined

