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

  describe "Problem 24 - Calculate Euler's totient function phi(m)." $ do
    context "totient" $ do
      it "should return the number of coprime numbers to n" $ do
        totient 10 `shouldBe` (4)

  describe "Problem 25 - Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order." $ do
    context "primeFactors" $ do
      it "should return prime factors of an integer" $ do
        primeFactors 315 `shouldBe` ([3, 3, 5, 7])

  describe "Problem 36 - Determine the prime factors of a given positive integer." $ do
    context "prime_factors_mult" $ do
      it "should get a list containing the prime factors and their multiplicity" $ do
        prime_factors_mult 315 `shouldBe` ([(3,2),(5,1),(7,1)])

  -- describe "Problem 17 - Split a list into two parts; the length of the first part is given" $ do
  --   context "split" $ do
  --     it "should drop every third element" $ do
  --       split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

  -- describe "Problem 18 - Extract a slice from a list" $ do
  --   context "slice" $ do
  --     it "should give a list given 2 indices" $ do
  --       slice "abcdefghik" 3 7 `shouldBe` ("cdefg")

  describe "Problem 39 - A list of prime numbers" $ do
    context "primesR" $ do
      it "should give all prime numbers in range" $ do
        primesR 10 20 `shouldBe` ([11, 13, 17, 19])

  -- describe "Problem 19 - Remove the K'th element from a list." $ do
  --   context "removeAt" $ do
  --     it "should remove the nth element of the list" $ do
  --       removeAt "abcd" 2 `shouldBe` ("acd")

