module FunctorSpec where

import Functor(Listona(..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main = hspec spec

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity x = x == fmap id x

functorComposition :: (Functor f, Eq (f c)) =>
                      Fun a b
                   -> Fun b c
                   -> f a
                   -> Bool
functorComposition (Fun _ f) (Fun _ g) x =
  fmap (g . f) x == ((fmap g) . (fmap f)) x

instance (Arbitrary a) => Arbitrary (Listona a) where
  arbitrary = mkIx >>= go
    where go :: Arbitrary a => Int -> Gen (Listona a)
          go 0 = pure ListonaVacia
          go n = Listona <$> arbitrary <*> go (n-1)
          mkIx :: Gen Int
          mkIx = fmap abs arbitrary

spec :: Spec
spec = do
  describe "listona" $ do
    it "functor identity law" $ property $
      \f -> functorIdentity (f :: Listona Int)

    it "functor composition law" $ property $
      \f g func -> functorComposition (f :: Fun Int String) (g :: Fun String Char) (func :: Listona Int)

