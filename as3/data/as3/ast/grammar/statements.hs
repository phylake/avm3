module Data.AS3.AST.Grammar.Statements where

import           Control.Monad
import           Data.AS3.AST.Def
import           Data.AS3.AST.Grammar.Expressions
import           Data.AS3.AST.Grammar.Lexicon
import           Data.AS3.AST.Prims
import           Data.AS3.AST.Scope
import           Data.AS3.AST.ThirdParty
import           Text.Parsec

import Data.AS3.AST.Show

-- $Package-level

package_id :: As3Parser String
package_id = many1 lower `sepBy1` char '.' >>= dots

package :: As3Parser Statement
package = do
  string "package" <* ss
  name <- optionMaybe package_id
  body <- between_braces $ many package_body
  return $ Package name body
  <?> "package"

package_body :: As3Parser Statement
package_body = try (tok imporT) <|> as3_class

imporT :: As3Parser Statement
imporT = liftM Import (string "import " *> package_id <* semi) <?> "import"

-- $Class-level

as3_class :: As3Parser Statement
as3_class = do
  scopes <- scope_mods
  tok $ string "class"
  name <- tok user_defined_type
  extends <- optionMaybe $ string "extends " *> extendable_type <* ss -- make sure "extends" is the first match in order to fail fast and return Nothing
  implements <- optionMaybe $ string "implements " *> csv implementable_type -- make sure "implements" is the first match in order to fail fast and return Nothing
  body <- with_scope PS_Class $ between_braces $ many $ tok (source_element <* optional semi)
  return $ Class scopes name extends implements body

source_element :: As3Parser Statement
source_element = try tstatement <|> function_declaration

function_declaration :: As3Parser Statement
function_declaration = do
  mods <- scope_mods <* string "function "
  name <- var_id
  params <- with_scope PS_TypedIds $ between_parens $ assignment_expression `sepBy` comma
  returnType <- type_declaration
  body <- with_scope PS_Function $ between_braces $ many tstatement
  return $ FnDec mods name params returnType body

tstatement :: As3Parser Statement
tstatement = tok (statement <* optional semi)

statement :: As3Parser Statement
statement =
      try block_statement
  <|> try variable_statement
  <|> try constant_statement -- ^ ‡
  <|> try empty_statement
  <|> try if_statement
  <|> try iteration_statement
  <|> try continue_statement
  <|> try break_statement
  <|> try return_statement
  <|> try with_statement
  <|> try expression_statement -- TODO this is capturing keywords. need to lock down identifiers
  {-<|> try switch_statement
  <|> try labelled_statement
  <|> try throw_statement
  <|>     try_statement-}

block_statement :: As3Parser Statement
block_statement = liftM Block $ between_braces $ many tstatement

variable_statement :: As3Parser Statement
variable_statement =
  string "var " *> with_scope PS_TypedIds idents <* optional semi where
    idents = liftM Variable $ assignment_expression `sepBy1` comma

constant_statement :: As3Parser Statement
constant_statement =
  string "const " *> with_scope PS_TypedIds idents <* optional semi where
    idents = liftM Constant $ assignment_expression `sepBy1` comma

empty_statement :: As3Parser Statement
empty_statement = semi *> return EmptyS

expression_statement :: As3Parser Statement
expression_statement = do
  notFollowedBy $ char '{'
  notFollowedBy $ string "function"
  liftM ExpressionStmt expression <* optional semi

if_statement :: As3Parser Statement
if_statement = liftM2 If
  (string "if" *> between_parens expression)
  tstatement

iteration_statement :: As3Parser Statement
iteration_statement =
      try do_while
  <|> try while
  <|> try for
  <|> try for_in
  <|>     for_each
  <?> "iteration stmt"
  where
    do_while :: As3Parser Statement
    do_while = liftM2 DoWhile
                 (string "do" *> tok statement)
                 (string "while" *> between_parens expression)

    while :: As3Parser Statement
    while = liftM2 While
              (string "while" *> between_parens expression)
              (statement)

    for :: As3Parser Statement
    for = liftM4 For
            --(forward *> optionMaybe expressionNoIn <* tok semi) TODO
            (forward *> optionMaybe (tok variable_statement) <* optSemi)
            (optionMaybe expression <* optSemi)
            (optionMaybe expression <* epilogue)
            (statement)
          where
            forward = tok (string "for") *> tok (char '(')
            optSemi = optional semi

    for_in :: As3Parser Statement
    for_in = liftM3 ForIn
               (forward *> lhs_expression <* string " in ")
               (expression <* epilogue)
               (statement)
             where
               forward = tok (string "for") *> tok (char '(')

    for_each :: As3Parser Statement
    for_each = liftM3 ForEach
                 (forward *> lhs_expression <* string " in ")
                 (expression <* epilogue)
                 (statement)
               where
                 forward = tok (string "for each") >> tok (char '(')
    
    epilogue :: As3Parser String
    epilogue = spaces *> string ")" <* spaces

continue_statement :: As3Parser Statement
continue_statement = liftM Continue
                       (string "continue" *> ss *> optionMaybe expression)

break_statement :: As3Parser Statement
break_statement = liftM Break
                    (string "break" *> ss *> optionMaybe expression)

return_statement :: As3Parser Statement
return_statement = liftM Return
                     (string "return" *> ss *> optionMaybe expression)

with_statement :: As3Parser Statement
with_statement = liftM2 With
                  (string "with" *> between_parens expression)
                  (statement)

switch_statement :: As3Parser Statement
switch_statement = undefined

labelled_statement :: As3Parser Statement
labelled_statement = undefined

throw_statement :: As3Parser Statement
throw_statement = undefined

try_statement :: As3Parser Statement
try_statement = undefined

