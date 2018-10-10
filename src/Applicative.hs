{-# LANGUAGE InstanceSigs #-}
module Applicative (List, listCat) where

data List a =
  Nil
  | Cons a (List a)

listCat :: List a -> List a -> List a
listCat Nil ys = ys
listCat x Nil = x
listCat (x `Cons` xs) ys = x `Cons` (listCat xs ys)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (x `Cons` xs) = f x `Cons` fmap f xs

instance Applicative List where
  pure x = x `Cons` Nil
  (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _ = Nil
  (f `Cons` fs) <*> x = (fmap f x) `listCat` (fs <*> x)
