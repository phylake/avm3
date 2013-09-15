module Data.AS3.AST.Scope (
  build_scope_tree
--, exit_fn
--, add_fn_id
--, function_ids
--, get_scope
--, get_scopes
) where

import           Control.Monad (forM_, liftM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (modify, get, gets)
import           Control.Monad.Trans.Class (lift)
import           Data.AS3.AST.Def
import           Data.AS3.AST.Prims
import           System.FilePath
import           Text.Parsec
import           Util.File (recurseDirs)
import           Util.Misc (t31)
import qualified Control.Applicative as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashTable.IO as H

-- $Tree built as a pre-parse step in order to perform static analysis

-- | { class:String => { id:String => scopeAndType:ScopeId }}
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
        type_info_p :: ParsecT String () IO [(String, ScopeId)]
        type_info_p = return $ [
            ("a", (InstanceId [Public] T_int))
          , ("b", (InstanceId [Public] T_int))
          , ("_equipment", (InstanceId [Public] T_int))
          , ("_gadgets", (InstanceId [Public] T_int))
          , ("_actions", (InstanceId [Public] T_int))
          , ("STATIC", (ClassId2 [Public] T_String))
          ]

-- TODO deprecated by build_scope_tree ?
{-push_class_scope :: String -- ^ class
                 -> String -- ^ identifier
                 -> ScopeId
                 -> As3Parser ()
--push_class_scope new = modify $\(ts, (klass, func)) -> (ts, (new:klass, func))
push_class_scope klass ident info = do
  ht1 <- gets t31 >>= liftIO
  ht2 <- liftIO $ H.lookup ht1 klass >>= maybe H.new return

  return ()-}

get_scope :: String -- ^ The class containing an identifier
          -> String -- ^ The public identifier (property or function) to fetch
          -> As3Parser (Maybe ScopeId)
get_scope klass ident = do
  ht1 <- gets t31 >>= liftIO
  liftIO $ do
    maybe_ht2 <- H.lookup ht1 klass
    case maybe_ht2 of
      Nothing -> return Nothing
      Just ht -> H.lookup ht ident

get_scopes :: String -> As3Parser (Maybe [(String, ScopeId)])
get_scopes klass = do
  ht1 <- gets t31 >>= liftIO
  liftIO $ do
    maybe_ht2 <- H.lookup ht1 klass
    case maybe_ht2 of
      Nothing -> return Nothing
      Just ht -> liftM Just $ H.toList ht

get_scope_ids :: String -> As3Parser (Maybe [String])
get_scope_ids klass = get_scopes klass >>= \m -> return (map fst A.<$> m)

match_scope_ids :: String -> As3Parser String
match_scope_ids klass = do
  m <- get_scope_ids klass
  case m of
    Nothing -> string ""
    Just [] -> string ""
    Just (id0:ids) -> try $ foldl plusfold (string id0) ids
