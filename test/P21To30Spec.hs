module P21To30Spec where
import Test.Hspec
import Test.QuickCheck.Arbitrary
import Control.Exception (evaluate)

import P21To30

spec :: Spec
spec = do

  describe "Problem 21 - Insert an element at a given position into a list." $ do
    context "insertAt" $ do
      it "should insert element to an array at the nth position" $ do
        insertAt 'X' "abcd" 2 `shouldBe` ("abXcd")

  describe "Problem 22 - Create a list containing all integers within a given range." $ do
    context "range" $ do
      it "should return an i" $ do
        range 4 9 `shouldBe` ([4,5,6,7,8,9])


  describe "Problem 23 - Extract a given number of randomly selected elements from a list." $ do
    context "rnd_select" $ do
      it "should return 3 random letters" $ do
        rnd_select "roiperelman" 3 >>= (`shouldSatisfy` (\xs -> length xs == 3))

  -- describe "Problem 4 - Find the number of elements of a list" $ do
  --   context "myLength" $ do
  --     it "should return the number of elements" $ do
  --       myLength [1 .. 100] `shouldBe` (100 :: Integer)

  -- describe "Problem 5 - Find the number of elements of a list" $ do
  --   context "myReverse" $ do
  --     it "should return the number of elements" $ do
  --       myReverse [1,2,3,4,5] `shouldBe` ([5,4,3,2,1])

  -- describe "Problem 6 - Find out whether a list is a palindrome" $ do
  --   context "isPalindrome" $ do
  --     it "should return true if cab be read forward and backward the same" $ do
  --       isPalindrome [1,2,3,2,1] `shouldBe` (True :: Bool)

  -- describe "Problem 7 - Flatten a nested list structure." $ do
  --   context "flatten" $ do
  --     it "should return true if cab be read forward and backward the same" $ do
  --       flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` ([1,2,3,4,5])

  -- describe "Problem 8 - Eliminate consecutive duplicates of list elements" $ do
  --   context "compress" $ do
  --     it "should return compressed list" $ do
  --       compress "aaaabccaadeeee" `shouldBe` ("abcade")

  -- describe "Problem 9 - Pack consecutive duplicates of list elements into sublists.\n\
  -- \  If a list contains repeated elements they should be placed in separate sublists." $ do
  --   context "pack" $ do
  --     it "should pack (like group) together repeating elements" $ do
  --       pack "aaaabccaadeeee" `shouldBe` ["aaaa","b","cc","aa","d","eeee"]

  -- describe "Problem 10 - Find out whether a list is a palindrome" $ do
  --   context "isPalindrome" $ do
  --     it "should return true if cab be read forward and backward the same" $ do
  --       encode "aaaabccaadeeee" `shouldBe` ([(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')])
