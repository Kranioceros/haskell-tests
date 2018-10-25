{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Applicative (List(..), listCat, singleton, ZipList(..)) where

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

listCat :: List a -> List a -> List a
listCat Nil ys = ys
listCat x Nil = x
listCat (x `Cons` xs) ys = x `Cons` (listCat xs ys)

singleton :: a -> List a
singleton x = Cons x Nil

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (x `Cons` xs) = f x `Cons` fmap f xs

instance Applicative List where
  pure x = x `Cons` Nil
  (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _ = Nil
  (f `Cons` fs) <*> x = (fmap f x) `listCat` (fs <*> x)

instance Foldable List where
  foldMap f Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons f fs) = Cons <$> f <*> (sequenceA fs)

-- The ziplist monoid and applicative

newtype ZipList a = ZList [a]
  deriving(Eq, Show)

instance Semigroup a => Semigroup (ZipList a) where
  (ZList []) <> xs  = xs
  xs <> (ZList []) = xs
  (ZList xs) <> (ZList ys) = ZList $ zipWith (<>) xs ys

instance Monoid a => Monoid (ZipList a) where
  mempty = ZList []
  mappend = (<>)

instance Functor ZipList where
  fmap f (ZList xs) = ZList $ fmap f xs

instance Applicative ZipList where
  pure x = ZList [x]
  (ZList fs) <*> (ZList xs) = ZList $ fs <*> xs
