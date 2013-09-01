module Data.AS3.AST.Parser (parser) where

import           Control.Monad.State
import           Data.AS3.AST.Def
import           Data.AS3.AST.Grammar.Expressions
import           Data.AS3.AST.Grammar.Lexicon
import           Data.AS3.AST.Grammar.Statements
import           Data.AS3.AST.Prims
import           Data.AS3.AST.Scope
import           Data.AS3.AST.Show
import           Data.AS3.AST.ThirdParty
import           Data.List (intersperse)
import           Text.Parsec
import qualified Control.Applicative as A
import qualified Data.HashTable.IO as H

parser :: As3Parser Statement
parser = package

type_defaults :: Maybe String -> Type -> Maybe String
type_defaults init T_int =             Just $ maybe "0"     id init
type_defaults init T_Number =          Just $ maybe "0.0"   id init
type_defaults init T_Boolean =         Just $ maybe "false" id init
type_defaults init T_String =          Just $ maybe ""      id init
type_defaults init (T_Vector _) =      Just $ maybe "null"  id init
type_defaults init (T_UserDefined _) = Just $ maybe "null"  id init

-- $Package-level

package_id :: As3Parser String
package_id = many1 lower `sepBy1` char '.' >>= dots

package :: As3Parser Statement
package = do
  string "package" <* ss
  name <- optionMaybe package_id
  body <- between_braces $ many package_body
  spaces
  return $ Package name body
  <?> "package"

package_body :: As3Parser Statement
package_body = tok $ try imporT <|> as3_class

imporT :: As3Parser Statement
imporT = liftM Import (string "import " *> package_id <* semi) <?> "import"

-- $Class-level

as3_class :: As3Parser Statement
as3_class = do
  spaces
  scopes <- scope_mods
  tok $ string "class"
  name <- tok class_id
  extends <- optionMaybe $ string "extends " *> class_id <* ss -- make sure "extends" is the first match in order to fail fast and return Nothing
  implements <- optionMaybe $ string "implements " *> csv class_id -- make sure "implements" is the first match in order to fail fast and return Nothing
  decs <- between_braces $ many class_body
  return $ Class scopes name extends implements decs

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
