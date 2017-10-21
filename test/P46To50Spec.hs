module P46To50Spec where
import Test.Hspec

import P46To50

spec :: Spec
spec =  do

  describe "Problem 46 and 47 - boolean application" $ do
    context "table" $ do
      it "should get a function and show truth table" $ do
        tableStrArr (\a b -> (and' a (or' a b))) `shouldBe` (["True True True","True False True","False True False","False False False"])

  describe "Problem 49 - gray codes" $ do
    context "gray" $ do
      it "should give all binary options in a specific order" $ do
        gray 3 `shouldBe` (["000","001","011","010","110","111","101","100"])

  describe "Problem 50 - Huffman codes" $ do
    context "huffman" $ do
      it "" $ do
        huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)] `shouldBe` ([('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")])
