module P11To20Spec where
import Test.Hspec
import Test.QuickCheck.Arbitrary
import Control.Exception (evaluate)

import P11To20

spec :: Spec
spec = do

  describe "Problem 1 - Find the last element of a list" $ do
    context "last" $ do
      it "should return last element of a list" $ do
        last [23 .. 25] `shouldBe` (25 :: Int)
      it "should return exception for empty list" $ do
        last [] `shouldThrow` errorCall "Prelude.last: empty list"
