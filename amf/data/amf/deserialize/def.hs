module Data.Amf.Deserialize.Def (Parser, AmfPrim(..)) where

import           Data.Amf.Def (Tables)
import           Data.Conduit (ConduitM, ResourceT)
import           Data.Conduit.Binary as CB
import           Data.Conduit.List as CL
import           Data.Void
import qualified Data.ByteString as B
import qualified MonadLib as ML

type Parser = ML.StateT Tables (ConduitM B.ByteString Void (ResourceT IO))

class AmfPrim a where
  fromAmf :: Parser a
