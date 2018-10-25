module Test.Utils(batch) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

batch :: TestBatch -> SpecWith (Arg Property)
batch tb =
  let tests = unbatch tb
      addProp :: (String, Property) -> SpecWith (Arg Property) -> SpecWith (Arg Property)
      addProp (str, prop) acc = acc >> (it str prop)
  in foldr addProp (pure ()) tests
