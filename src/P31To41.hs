module P31To41 where

isPrime :: Int -> Bool
isPrime n = not $ or $ map isPrimeHelper [2..(n-1)] where
  isPrimeHelper d = n `mod` d == 0

-- Problem 32
myGCD :: Int -> Int -> Int
myGCD a b
 | a `mod` b == 0 = abs b
 | otherwise = myGCD b $ a `mod` b

-- Problem 33
coprime :: Int -> Int -> Bool
coprime a b = myGCD a b == 1

-- Problem 34
