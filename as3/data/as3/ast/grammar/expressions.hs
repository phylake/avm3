module Data.AS3.AST.Grammar.Expressions where

import           Control.Monad
import           Data.AS3.AST.Def
import           Data.AS3.AST.Grammar.Lexicon
import           Data.AS3.AST.Prims
import           Data.AS3.AST.ThirdParty
import           Text.Parsec


function_expression :: As3Parser AST
function_expression = fail "function_expression"



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
  chainl1 (tok equality_expression) (try op) -- try op so part of && isn't consumed
  <?> "bitwise AND expression"
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
  chainl1 (tok bitwiseXOR_expression) (try op)  -- try op so part of || isn't consumed
  <?> "bitwise OR expression"
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

-- $11.14 Comma Operator

expression :: As3Parser AST
expression = liftM Expression (assignment_expression `sepBy` comma)

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
