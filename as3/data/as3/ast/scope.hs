module Data.AS3.AST.Scope (
  build_scope_tree
, exit_fn
, get_scope
, get_scopes
, push_fn_scope
) where

import           Control.Monad (forM_, liftM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (modify, gets)
import           Data.AS3.AST.Def
import           Data.AS3.AST.Prims
import           System.FilePath
import           Text.Parsec
import           Util.File (recurseDirs)
import           Util.Misc (t31)
import qualified Control.Applicative as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.IO as H

-- | Build all 
build_scope_tree :: [FilePath] -> IO ScopeTree
build_scope_tree files = do
  tree <- H.new
  
  idents <- build_identifiers "Foo.as" >>= H.toList
  --putStrLn $ unlines $ map show $ idents
  
  forM_ files $ add_identifier tree
  return tree
  where
    add_identifier :: ScopeTree -> FilePath -> IO ()
    add_identifier tree fp = do
      idents <- build_identifiers fp
      H.insert tree klass idents
      return ()
      where
        klass = dropExtensions fp
    build_identifiers :: FilePath -> IO Identifiers
    build_identifiers fp = do
      either <- liftM BS.unpack (BS.readFile fp)
            >>= runParserT type_info_p () ""
      case either of
        Left err -> fail "build_identifiers failed"
        Right infos -> do
          ht <- H.new
          forM_ infos $ \(ident, ti) -> H.insert ht ident ti
          return ht
      where
        -- TODO make this real
        type_info_p :: ParsecT String () IO [(String, TypeInfo)]
        type_info_p = return $ [
            ("a", (Nothing, T_int))
          , ("_equipment", (Nothing, T_int))
          , ("_gadgets", (Nothing, T_int))
          , ("_actions", (Nothing, T_int))
          ]

get_scope :: String -> String -> As3Parser (Maybe TypeInfo)
get_scope klass ident = do
  ht1 <- gets t31 >>= liftIO
  liftIO $ do
    maybe_ht2 <- H.lookup ht1 klass
    case maybe_ht2 of
      Nothing -> return Nothing
      Just ht -> H.lookup ht ident

get_scopes :: String -> As3Parser (Maybe [(String, TypeInfo)])
get_scopes klass = do
  ht1 <- gets t31 >>= liftIO
  liftIO $do
    maybe_ht2 <- H.lookup ht1 klass
    case maybe_ht2 of
      Nothing -> return Nothing
      Just ht -> liftM Just $H.toList ht

get_scope_ids :: String -> As3Parser (Maybe [String])
get_scope_ids klass = get_scopes klass >>= \m -> return (map fst A.<$> m)

match_scope_ids :: String -> As3Parser String
match_scope_ids klass = do
  m <- get_scope_ids klass
  case m of
    Nothing -> string ""
    Just [] -> string ""
    Just (id0:ids) -> try $ foldl plusfold (string id0) ids

-- add function-level scope identifier
push_fn_scope :: String -> As3Parser ()
push_fn_scope new = modify $ \(a, b, func) -> (a, b, new:func)

-- remove function-level scope identifiers
exit_fn :: As3Parser ()
exit_fn = modify $ \(a, b, _) -> (a, b, [])

-- TODO deprecated by build_scope_tree ?
push_class_scope :: String -- ^ class
                 -> String -- ^ identifier
                 -> TypeInfo
                 -> As3Parser ()
--push_class_scope new = modify $\(ts, (klass, func)) -> (ts, (new:klass, func))
push_class_scope klass ident info = do
  ht1 <- gets t31 >>= liftIO
  ht2 <- liftIO $ H.lookup ht1 klass >>= maybe H.new return

  return ()
