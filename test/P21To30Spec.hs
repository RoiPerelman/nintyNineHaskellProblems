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
      it "should return a range between start and end" $ do
        range 4 9 `shouldBe` ([4,5,6,7,8,9])


  describe "Problem 23 - Extract a given number of randomly selected elements from a list." $ do
    context "rnd_select" $ do
      it "should return 3 random letters" $ do
        rnd_select "roiperelman" 3 >>= (`shouldSatisfy` (\xs -> length xs == 3))

  describe "Problem 24 - Draw N different random numbers from the set 1..M." $ do
    context "diff_select" $ do
      it "should select 3 random number between 1 and 7 should have 3 numbers" $ do
        diff_select 3 7 >>= (`shouldSatisfy` (\xs -> length xs == 3))

  describe "Problem 25 - Generate a random permutation of the elements of a list." $ do
    context "rnd_permu" $ do
      it "should return a random permutation" $ do
        rnd_permu [1,2,3,4,5] >>= (`shouldSatisfy` (\xs -> length xs == 5))

  describe "Problem 26 - Generate the combinations of K distinct objects chosen from the N elements of a list" $ do
    context "combinations" $ do
      it "should return 6 combinations for choosing 2 out of 4 " $ do
        combinations 2 [1,2,3,4] `shouldBe` ([[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]])

  describe "Problem 27 - Group the elements of a set into disjoint subsets." $ do
    context "group" $ do
      it "should return true if cab be read forward and backward the same" $ do
        group' [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"] `shouldSatisfy` (\x -> length x == 1260)

  describe "Problem 28a - Sorting a list of lists according to length of sublists" $ do
    context "lsort" $ do
      it "should return compressed list" $ do
        lsort ["abc","de","fgh","de","ijkl","mn","o"] `shouldBe` (["o","de","de","mn","abc","fgh","ijkl"])

  describe "Problem 28b - Sorting a list of lists according to length of sublists" $ do
    context "lfsort" $ do
      it "should return compressed list" $ do
        lfsort ["abc","de","fgh","de","ijkl","mn","o"] `shouldBe` (["o","ijkl","abc","fgh","de","de","mn"])
