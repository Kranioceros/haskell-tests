module ApplicativeSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main = hspec spec

spec = do
  describe "bobo" $ do
    it "does nothing" $ do
      3 `shouldBe` 3
