module ApplicativeSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Utils(batch)
import Data.Monoid(Sum(..))

import Applicative(List(..), singleton, ZipList(..))

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency
    [(1, pure Nil),
     (6, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = fmap ZList arbitrary

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

main = hspec spec

types = undefined :: (Int, Char, Bool)

spec = do
  describe "List applicative" $ batch $
    applicative (singleton types)
  describe "Ziplist monoid" $ batch $
    monoid (undefined :: ZipList (Sum Int))
  describe "Ziplist applicative" $ batch $
    applicative (ZList . pure $ types)
