module Util.Misc where

import           Control.Monad.State (get)
import qualified Data.ByteString.Lazy as BS

forNState :: (Ord n, Num n, Monad m) => m a -> n -> m [a]
forNState f n = if n > 0
    then do
        x <- f
        xs <- forNState f (n-1)
        return $ x:xs
    else do return []

allBytes f = do
    bs <- get
    if BS.null bs
        then return []
        else do
            x <- f
            xs <- allBytes f
            return $ x:xs

forN :: (Ord n, Num n, Monad m)
     => (a -> m a)
     -> a
     -> n
     -> m a
forN f m n
    | n > 0     = return m >>= f >>= \m' -> forN f m' (n-1)
    | otherwise = return m

forN' f a n
    | n > 0 = forN' f (f a) (n-1)
    | otherwise = a

t21 = fst
t22 = snd

t31 (a,_,_) = a
t32 (_,a,_) = a
t33 (_,_,a) = a

t41 (a,_,_,_) = a
t42 (_,a,_,_) = a
t43 (_,_,a,_) = a
t44 (_,_,_,a) = a
