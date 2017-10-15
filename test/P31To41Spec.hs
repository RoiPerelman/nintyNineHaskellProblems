module P11To20Spec where
import Test.Hspec
import Test.QuickCheck.Arbitrary
import Control.Exception (evaluate)

import P11To20

spec :: Spec
spec = do

  describe "Problem 11 - Modified run-length encoding" $ do
    context "encodeModified" $ do
      it "should return a list of Single a | Mulitple Int a" $ do
        encodeModified "aaaabccaadeeee" `shouldBe` ([Multiple 4 'a',Single 'b',Multiple 2 'c',
                                                     Multiple 2 'a',Single 'd',Multiple 4 'e'])

  describe "Problem 12 - Decode a run-length encoded list" $ do
    context "decodeModified" $ do
      it "should gets a list of Single a | Multuple Int a and return a list " $ do
        decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',
                        Multiple 2 'a',Single 'd',Multiple 4 'e'] `shouldBe` ("aaaabccaadeeee")

  describe "Problem 13 - direct Modified run-length encoding" $ do
    context "encodeDirect" $ do
      it "should work like encodeModified" $ do
        encodeDirect "aaaabccaadeeee" `shouldBe` ([Multiple 4 'a',Single 'b',Multiple 2 'c',
                                                     Multiple 2 'a',Single 'd',Multiple 4 'e'])

  describe "Problem 14 - Duplicate the elements of a list." $ do
    context "dupli" $ do
      it "should duplicate each character" $ do
        dupli "abccd" `shouldBe` ("aabbccccdd")

  describe "Problem 15 - Replicate the elements of a list a given number of times." $ do
    context "repli" $ do
      it "should replicate 3 times" $ do
        repli "abc" 3 `shouldBe` ("aaabbbccc")

  describe "Problem 16 - Drop every N'th element from a list." $ do
    context "dropEvery" $ do
      it "should drop every third element" $ do
        dropEvery "abcdefghik" 3 `shouldBe` ("abdeghk")

  describe "Problem 17 - Split a list into two parts; the length of the first part is given" $ do
    context "split" $ do
      it "should drop every third element" $ do
        split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

  describe "Problem 18 - Extract a slice from a list" $ do
    context "slice" $ do
      it "should give a list given 2 indices" $ do
        slice "abcdefghik" 3 7 `shouldBe` ("cdefg")

  describe "Problem 19 - Rotate a list N places to the left" $ do
    context "rotate" $ do
      it "should get a number and rotate the arr times that number" $ do
        rotate "abcdefgh" 3 `shouldBe` ("defghabc")
        rotate "abcdefgh" (-2) `shouldBe` ("ghabcdef")

  describe "Problem 19 - Remove the K'th element from a list." $ do
    context "removeAt" $ do
      it "should remove the nth element of the list" $ do
        removeAt "abcd" 2 `shouldBe` ("acd")

