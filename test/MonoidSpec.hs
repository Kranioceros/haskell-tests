module MonoidSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Monoid(Bull(..))

instance Arbitrary Bull where
  arbitrary =
    oneof [ pure Fools, pure Twoo ]

instance EqProp Bull where
  (=-=) = eq

main = hspec spec

batch :: TestBatch -> SpecWith (Arg Property)
batch tb =
  let tests = unbatch tb
      addProp :: (String, Property) -> SpecWith (Arg Property) -> SpecWith (Arg Property)
      addProp (str, prop) acc = acc >> (it str prop)
  in foldr addProp (pure ()) tests

spec :: Spec
spec = do
  describe "Bull bad monoid" $ batch $
    monoid Fools
  describe "List applicative" $ batch $
    applicative [("b", "w", 1 :: Int)]
