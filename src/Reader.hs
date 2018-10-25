{-# LANGUAGE InstanceSigs #-}

module Reader where

import Data.Char
import Control.Applicative

newtype Fun r a = Fun (r -> a)

data Point = Point {
  getX :: Float,
  getY :: Float
} deriving (Eq, Show)

origin = Point { getX = 0, getY = 0 }

distance :: Point -> Float
distance = hip <$> getX <*> getY
  where hip :: Float -> Float -> Float
        hip x y = sqrt $ x^2 + y^2

instance Functor (Fun r) where
  fmap g (Fun f) = Fun (g . f)

instance Applicative (Fun r) where
  pure x = Fun $ \_ -> x
  (<*>) :: Fun r (a -> b) -> Fun r a -> Fun r b
  (Fun f) <*> (Fun g) = Fun $ \r -> f r (g r)

instance Monad (Fun r) where
  return = pure
  (Fun ra) >>= aRb = Fun $ \r ->
    let (Fun aRb') = aRb (ra r)
    in aRb' r

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

tup :: [Char] -> [Char] -> ([Char], [Char])
tup = (,)

composed = rev . cap
fmapped = fmap rev cap
tupled = (,) <$> rev <*> cap
tupled' = rev >>= (\r -> cap >>= (\c -> return (r, c)))
