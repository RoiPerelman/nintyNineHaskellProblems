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

  describe "Problem 39 - A list of prime numbers" $ do
    context "primesR" $ do
      it "should give all prime numbers in range" $ do
        primesR 10 20 `shouldBe` ([11, 13, 17, 19])

  describe "Problem 40 - Goldbach's conjecture" $ do
    context "goldbach" $ do
      it "should return 2 prime numbers that compose an even number" $ do
        goldbach 28 `shouldBe` (5, 23)

  describe "Problem 41 - Goldbach's conjecture" $ do
    context "goldbachList" $ do
      it "should return all instances of goldbach between 2 values (only for even numbers)" $ do
        goldbachList 9 20 `shouldBe` ([(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)])
        goldbachList' 4 2000 50 `shouldBe` ([(73,919),(61,1321),(67,1789),(61,1867)])
