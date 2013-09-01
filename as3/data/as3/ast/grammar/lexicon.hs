module Data.AS3.AST.Grammar.Lexicon where

import           Text.Parsec
import           Data.AS3.AST.Def

line_terminator :: As3Parser String
line_terminator = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\r"
  <|> string "\n"

{-identifier :: As3Parser String
identifier =
      comments
  <|> line_terminator
  <|> as3Token-}

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
  {-<|> regex_literal-}
  <?> "literal"

null_literal :: As3Parser String
null_literal = string "null"

boolean_literal :: As3Parser String
boolean_literal = string "true" <|> string "false" <?> "boolean literal"

numeric_literal :: As3Parser String
numeric_literal = try hex_integer_literal <|> decimal_literal <?> "numeric literal"

decimal_literal :: As3Parser String
decimal_literal =
      try (decimal_integer_literal >> string "." >> decimal_digits)
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
