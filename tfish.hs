module TFish where {- Truish and Falsish -}
import Control.Applicative
import Data.Monoid
import Data.Word
import Data.Int
import Data.Maybe
import qualified Data.ByteString.Lazy as DBL

infixr 2 <||>

(<||>) :: TFish a => a -> a -> a 
a <||> b = let (Just c) = coerce a <|> coerce b in c

class TFish a where
    coerce :: a -> Maybe a

instance TFish [a] where
    coerce [] = Nothing
    coerce xs = Just xs

instance TFish DBL.ByteString where
    coerce bs = if DBL.null bs then Nothing else Just bs

instance TFish (Maybe a) where
    coerce m@(Just _) = Just m
    coerce Nothing = Nothing

instance TFish Bool where
    coerce True = Just True
    coerce False = Nothing

instance TFish Int where
    coerce 0 = Nothing
    coerce i = Just i

instance TFish Int8 where
    coerce 0 = Nothing
    coerce i = Just i

instance TFish Int16 where
    coerce 0 = Nothing
    coerce i = Just i

instance TFish Int32 where
    coerce 0 = Nothing
    coerce i = Just i

instance TFish Int64 where
    coerce 0 = Nothing
    coerce i = Just i

instance TFish Word where
    coerce 0 = Nothing
    coerce i = Just i

instance TFish Word8 where
    coerce 0 = Nothing
    coerce i = Just i

instance TFish Word16 where
    coerce 0 = Nothing
    coerce i = Just i

instance TFish Word32 where
    coerce 0 = Nothing
    coerce i = Just i

instance TFish Word64 where
    coerce 0 = Nothing
    coerce i = Just i

instance TFish Integer where
    coerce 0 = Nothing
    coerce i = Just i

instance TFish Float where
    coerce 0 = Nothing
    coerce i = Just i

instance TFish Double where
    coerce 0 = Nothing
    coerce i = Just i
