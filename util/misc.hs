module Util.Misc where

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
