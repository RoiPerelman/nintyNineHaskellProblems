module P1To10Spec where
import Test.Hspec
import Test.QuickCheck.Arbitrary
import Control.Exception (evaluate)

import P1To10

spec :: Spec
spec = do

  describe "Problem 1 - Find the last element of a list" $ do
    context "last" $ do
      it "should return last element of a list" $ do
        last [23 .. 25] `shouldBe` (25 :: Int)
      it "should return exception for empty list" $ do
        last [] `shouldThrow` errorCall "Prelude.last: empty list"

  describe "Problem 2 - Find the last but one element of a list" $ do
    context "myButLast" $ do
      it "should return the last but one" $ do
        myButLast [1 .. 100] `shouldBe` (99 :: Int)


  describe "Problem 3 - Find the K'th element of a list. The first element in the list is number 1" $ do
    context "elementAt" $ do
      it "should return the fifth element" $ do
        elementAt [1 .. 100] 5 `shouldBe` (5 :: Int)

  describe "Problem 4 - Find the number of elements of a list" $ do
    context "myLength" $ do
      it "should return the number of elements" $ do
        myLength [1 .. 100] `shouldBe` (100 :: Integer)

  describe "Problem 5 - Find the number of elements of a list" $ do
    context "myReverse" $ do
      it "should return the number of elements" $ do
        myReverse [1,2,3,4,5] `shouldBe` ([5,4,3,2,1])

  describe "Problem 6 - Find out whether a list is a palindrome" $ do
    context "isPalindrome" $ do
      it "should return true if cab be read forward and backward the same" $ do
        isPalindrome [1,2,3,2,1] `shouldBe` (True :: Bool)

  describe "Problem 7 - Flatten a nested list structure." $ do
    context "flatten" $ do
      it "should return true if cab be read forward and backward the same" $ do
        flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` ([1,2,3,4,5])

  describe "Problem 8 - Eliminate consecutive duplicates of list elements" $ do
    context "compress" $ do
      it "should return compressed list" $ do
        compress "aaaabccaadeeee" `shouldBe` ("abcade")

  describe "Problem 9 - Pack consecutive duplicates of list elements into sublists.\n\
  \  If a list contains repeated elements they should be placed in separate sublists." $ do
    context "pack" $ do
      it "should pack (like group) together repeating elements" $ do
        pack "aaaabccaadeeee" `shouldBe` ["aaaa","b","cc","aa","d","eeee"]

  describe "Problem 10 - Find out whether a list is a palindrome" $ do
    context "isPalindrome" $ do
      it "should return true if cab be read forward and backward the same" $ do
        encode "aaaabccaadeeee" `shouldBe` ([(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')])
