module Data.AS3.AST.Grammar.Expressions where

import           Control.Monad
import           Data.AS3.AST.Def
import           Data.AS3.AST.Grammar.Lexicon
import           Data.AS3.AST.Prims
import           Data.AS3.AST.Scope
import           Data.AS3.AST.ThirdParty
import           Text.Parsec

-- $Helpers

add_ids :: [Expression] -> As3Parser [Expression]
add_ids exps = loop exps >> return exps where
  loop :: [Expression] -> As3Parser ()
  loop (ClassId _ _ id _:exps) = add_fn_id id >> loop exps
  loop (FnId _ id _:exps) = add_fn_id id >> loop exps
  loop (FnParamId id _:exps) = add_fn_id id >> loop exps
  loop (ExpressionId id:exps) = add_fn_id id >> loop exps
  loop (_:exps) = loop exps
  loop [] = return ()

-- $11.1 Primary Expressions

primary_expression :: As3Parser Expression
primary_expression =
      (liftM TODO_E (try $ string "this"))
  <|> try scoped_identifier
  {-
    identifiers already captured in function params and local var/consts.
    
    this is at odds with the initial capture above (scoped_identifier)
    making it more obvious i need a pre-parse step to pluck out identifiers
    for the scope OR assume all identifiers are valid and deal with problems
    in the AST.
    I'M CONFUSING GRAMMATICAL CORRECTNESS AND PROGRAM CORRECTNESS
    program correctness is verified by analyzing the very AST i'm trying to build
    meaning there's no pre-parse step and i need to build my scope tree as i parse
    so i have a useful tool with which to analyze the tree

    statements, being a superset of expressions are allowed in fewer places.
    i.e. it's not about where i can put expressions, it's about where i can't
    put statements

    commenting this out for now and letting all identifiers be valid
  -}
  <|> try (liftM ExpressionId function_ids)
  <|> try (liftM ParenGroup $ between_parens comma_expression)
  <|> try (liftM TODO_E literal)
  {-<|> liftM TODO_E array_literal-}
  <|> try object_literal
  <?> "primary_expression"

array_literal :: As3Parser Expression
array_literal = undefined

element_list :: As3Parser Expression
element_list = undefined

object_literal :: As3Parser Expression
object_literal = liftM ObjectLiteral $ between_braces kvps where
  kvps :: As3Parser [Expression]
  kvps = property_assignment `sepBy` comma

  property_assignment :: As3Parser Expression
  property_assignment = liftM2 KeyValue
    (property_name <* ss <* char ':' <* ss)
    assignment_expression

  property_name :: As3Parser Expression
  property_name =
        try expression_id
    <|> try (liftM (Lit . L_String) string_literal)
    <|> try (liftM (Lit . L_Number) numeric_literal)

-- $11.2 Left-Hand-Side Expressions

member_expression :: As3Parser Expression
member_expression =
      try primary_expression
  {-<|> try function_expression-}

function_expression :: As3Parser Expression
function_expression = undefined
{-function_expression = liftM3 FunctionExp
                        (optionMaybe identifier)
                        comma_expression
                        (many statement)-}

new_expression :: As3Parser Expression
new_expression =
      try (liftM New $ string "new " *> new_expression)
  <|> member_expression
  <?> "new expression"

call_expression :: As3Parser Expression
call_expression = fail "call_expression"

arguments :: As3Parser [Expression]
arguments = between_parens argument_list

argument_list :: As3Parser [Expression]
argument_list = csv assignment_expression

lhs_expression :: As3Parser Expression
lhs_expression = try new_expression <|> call_expression <?> "lhs_expression"

-- $11.3 Postfix Expressions

postfix_expression :: As3Parser Expression
postfix_expression =
      try (liftM2 Postfix (lhs_expression <* ss) unary_expression_post)
  <|> lhs_expression
  <?> "postfix_expression"
  where
    unary_expression_post :: As3Parser UnaryOp
    unary_expression_post =
          (string "++" >> return Increment)
      <|> (string "--" >> return Decrement)
      <?> "unary POSTFIX op"

-- $11.4 Unary Operators

unary_expression :: As3Parser Expression
unary_expression =
      try (liftM2 Unary unary_expression_pre unary_expression)
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

multiplicative_expression :: As3Parser Expression
multiplicative_expression =
  chainl1 (tok unary_expression) multiplicative_op
  <?> "multiplicative expression"
  where
    multiplicative_op :: As3Parser (Expression -> Expression -> Expression)
    multiplicative_op =
          (linkL Multiplication)
      <|> (linkL Division)
      <|> (linkL Modulo)
      <?> "multiplicative operator"

-- $11.6 Additive Operators

additive_expression :: As3Parser Expression
additive_expression =
  chainl1 (tok multiplicative_expression) additive_op
  <?> "additive expression"
  where
    additive_op :: As3Parser (Expression -> Expression -> Expression)
    additive_op =
          (linkL Addition)
      <|> (linkL Subtraction)
      <?> "additive operator"

-- $11.7 Bitwise Shift Operators

shift_expression :: As3Parser Expression
shift_expression =
  chainl1 (tok additive_expression) shift_op
  <?> "shift expression"
  where
    shift_op :: As3Parser (Expression -> Expression -> Expression)
    shift_op =
          (linkL LShift)
      <|> (linkL RShift)
      <|> (linkL URShift)
      <?> "bitwise shift operator"

-- $11.8 Relational Operators

{- TODO ...NoIn
   relational_expression :: Bool -> As3Parser Expression -}

relational_expression :: As3Parser Expression
relational_expression =
  chainl1 (tok shift_expression) relational_op <?> "relational expression"
  where
    relational_op :: As3Parser (Expression -> Expression -> Expression)
    relational_op =
          (linkL LessThan)
      <|> (linkL GreaterThan)
      <|> (linkL LessThanEq)
      <|> (linkL GreaterThanEq)
      <|> (linkL InstanceOf)
      <|> (linkL In)
      <?> "relational operator"

-- $11.9 Equality Operators

equality_expression :: As3Parser Expression
equality_expression =
  chainl1 (tok relational_expression) equality_op <?> "equality expression"
  where
    equality_op :: As3Parser (Expression -> Expression -> Expression)
    equality_op =
          (linkL Equality)
      <|> (linkL StrictEquality)
      <|> (linkL InEquality)
      <|> (linkL StrictInEquality)
      <?> "equality operator"

-- $11.10 Binary Bitwise Operators

bitwiseAND_expression :: As3Parser Expression
bitwiseAND_expression =
  chainl1 (tok equality_expression) (try op) -- try op so part of && isn't consumed
  <?> "bitwise AND expression"
  where
    op :: As3Parser (Expression -> Expression -> Expression)
    op = linkL BitwiseAND

bitwiseXOR_expression :: As3Parser Expression
bitwiseXOR_expression =
  chainl1 (tok bitwiseAND_expression) op <?> "bitwise XOR expression"
  where
    op :: As3Parser (Expression -> Expression -> Expression)
    op = linkL BitwiseXOR

bitwiseOR_expression :: As3Parser Expression
bitwiseOR_expression =
  chainl1 (tok bitwiseXOR_expression) (try op)  -- try op so part of || isn't consumed
  <?> "bitwise OR expression"
  where
    op :: As3Parser (Expression -> Expression -> Expression)
    op = linkL BitwiseOR

-- $11.11 Binary Logical Operators

logicalAND_expression :: As3Parser Expression
logicalAND_expression =
  chainl1 (tok bitwiseOR_expression) op <?> "logical AND expression"
  where
    op :: As3Parser (Expression -> Expression -> Expression)
    op = linkL LogicalAND

logicalOR_expression :: As3Parser Expression
logicalOR_expression =
   chainl1 (tok logicalAND_expression) op <?> "logical OR expression"
  where
    op :: As3Parser (Expression -> Expression -> Expression)
    op = linkL LogicalOR

-- $11.12 Conditional Operator ( ? : )

conditional_expression :: As3Parser Expression
conditional_expression =
  try (liftM3
         TernOp
         (tok logicalOR_expression <* tok (string "?"))
         (tok assignment_expression <* tok (string ":")) -- true
         (tok assignment_expression)) -- false
  <|> logicalOR_expression
  <?> "conditional expression"

-- $11.13 Assignment Operators

assignment_expression :: As3Parser Expression
assignment_expression =
  try (liftM3 RBinOp
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

-- $11.14 Comma Operator

-- TODO Expression == ArgumentList. resolve this
comma_expression :: As3Parser Expression
comma_expression = liftM Comma (assignment_expression `sepBy1` comma)

expression :: As3Parser Expression
expression = comma_expression

expressionNoIn :: As3Parser Expression
expressionNoIn = expression -- TODO

-- $Chain links

{-linkR :: BinaryOp -> As3Parser (Expression -> Expression -> Expression)
linkR = linkCommon RBinOp-}

linkL :: BinaryOp -> As3Parser (Expression -> Expression -> Expression)
linkL = linkCommon LBinOp

linkCommon :: (BinaryOp -> Expression -> Expression -> Expression)
           -> BinaryOp
           -> As3Parser (Expression -> Expression -> Expression)
linkCommon f op = try $ tok pop >> notFollowedBy pop >> return (f op)
  where
    pop = string (show op)
