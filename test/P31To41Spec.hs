module P31To41Spec where
import Test.Hspec
import Test.QuickCheck.Arbitrary
import Control.Exception (evaluate)

import P31To41

spec :: Spec
spec = do

  describe "Problem 31 - Determine whether a given integer number is prime." $ do
    context "isPrime" $ do
      it "should return True if Prime" $ do
        isPrime 29 `shouldBe` (True)

  describe "Problem 32 - Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm." $ do
    context "myGCD" $ do
      it "should give us the greatest common divisor between 36 and 63 " $ do
        myGCD 36 63 `shouldBe` (9)
        myGCD (-3) (-6) `shouldBe` (3)
        myGCD (-3) (6) `shouldBe` (3)

  describe "Problem 23 - Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1." $ do
    context "coprime" $ do
      it "should return True fro 35 and 64" $ do
        coprime 35 64 `shouldBe` (True)

  -- describe "Problem 14 - Duplicate the elements of a list." $ do
  --   context "dupli" $ do
  --     it "should duplicate each character" $ do
  --       dupli "abccd" `shouldBe` ("aabbccccdd")

  -- describe "Problem 15 - Replicate the elements of a list a given number of times." $ do
  --   context "repli" $ do
  --     it "should replicate 3 times" $ do
  --       repli "abc" 3 `shouldBe` ("aaabbbccc")

  -- describe "Problem 16 - Drop every N'th element from a list." $ do
  --   context "dropEvery" $ do
  --     it "should drop every third element" $ do
  --       dropEvery "abcdefghik" 3 `shouldBe` ("abdeghk")

  -- describe "Problem 17 - Split a list into two parts; the length of the first part is given" $ do
  --   context "split" $ do
  --     it "should drop every third element" $ do
  --       split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

  -- describe "Problem 18 - Extract a slice from a list" $ do
  --   context "slice" $ do
  --     it "should give a list given 2 indices" $ do
  --       slice "abcdefghik" 3 7 `shouldBe` ("cdefg")

  -- describe "Problem 19 - Rotate a list N places to the left" $ do
  --   context "rotate" $ do
  --     it "should get a number and rotate the arr times that number" $ do
  --       rotate "abcdefgh" 3 `shouldBe` ("defghabc")
  --       rotate "abcdefgh" (-2) `shouldBe` ("ghabcdef")

  -- describe "Problem 19 - Remove the K'th element from a list." $ do
  --   context "removeAt" $ do
  --     it "should remove the nth element of the list" $ do
  --       removeAt "abcd" 2 `shouldBe` ("acd")

