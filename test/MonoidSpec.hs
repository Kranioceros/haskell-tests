module MonoidSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Utils(batch)

import Monoid(Bull(..))

instance Arbitrary Bull where
  arbitrary =
    oneof [ pure Fools, pure Twoo ]

instance EqProp Bull where
  (=-=) = eq

main = hspec spec

spec :: Spec
spec = do
  describe "Bull bad monoid" $ batch $
    monoid Fools
  describe "List applicative" $ batch $
    applicative [("b", "w", 1 :: Int)]
