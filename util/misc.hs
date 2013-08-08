module Util.Misc where

import           Control.Monad (replicateM)
import           Control.Monad.State (get)
import qualified Data.ByteString as B
import qualified MonadLib as ML

instance Integral Float where
  quotRem a b = (fab, (ab - fab)*b)
    where
      ab = a/b
      fab = floor ab
  toInteger = floor

instance Integral Double where
  quotRem a b = (fab, (ab - fab)*b)
    where
      ab = a/b
      fab = floor ab
  toInteger = floor

replicateM' :: (Monad m, Integral i) => m a -> i -> m [a]
replicateM' m i = Control.Monad.replicateM (fromIntegral i) m

allBytes f = do
  bs <- get
  if B.null bs
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

t51 (a,_,_,_,_) = a
t52 (_,a,_,_,_) = a
t53 (_,_,a,_,_) = a
t54 (_,_,_,a,_) = a
t55 (_,_,_,_,a) = a
