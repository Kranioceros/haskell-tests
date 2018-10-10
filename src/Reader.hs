{-# LANGUAGE InstanceSigs #-}

module Reader where

import Control.Applicative

hurr = (*2)
durr = (+10)

m = hurr . durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> hurr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

data Func a b = Func (a -> b)

(.$) :: Func a b -> a -> b
(Func f) .$ x = f x

instance Functor (Func a) where
  fmap :: (b -> c) -> Func a b -> Func a c
  fmap g (Func f) = Func (\x -> g (f x))

instance Applicative (Func a) where
  pure x = Func (\_ -> x)
  (<*>) :: Func a (b -> c) -> Func a b -> Func a c
  (Func f) <*> (Func g) = Func (\x -> f x (g x))
