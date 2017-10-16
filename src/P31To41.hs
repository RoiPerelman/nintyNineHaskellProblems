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
totient :: Int -> Int
totient n = sum $ map (\x -> if coprime n x then 1 else 0) [1..n-1]

totient' :: Int -> Int
totient' n = length [x | x <- [1..n], coprime x n]

-- Problem 35
findNextPrime :: Int -> Int
findNextPrime a
  | isPrime $ a + 1 = a + 1
  | otherwise = findNextPrime $ a + 1

primeFactors :: Int -> [Int]
primeFactors n = primeFactorsHelper n 2
  where
    primeFactorsHelper n p
      | isPrime n = [n]
      | n `mod` p == 0 = p:primeFactorsHelper (n `div` p) p
      | otherwise = primeFactorsHelper n $ findNextPrime p

-- Problem 36
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = tail $ reverse $ foldl (\acc p ->
                                if (fst . head $ acc) == p
                                then (p, (snd . head $ acc) + 1):(tail acc)
                                else (p, 1):acc)
                       [(1,0)] $ primeFactors n

-- Problem 37 - skipped
-- Problem 38 - skipped
-- Problem 39
primesR :: Int -> Int -> [Int]
primesR n1 n2
  | n1 > n2 = []
  | isPrime n1 = n1:(primesR (findNextPrime n1) n2)
  | otherwise = primesR (findNextPrime n1) n2

-- Problem 40
