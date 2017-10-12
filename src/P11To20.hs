module P11To20
    ( ListItem (Single, Multiple)
    , encodeModified
    , decodeModified
    , encodeDirect
    , dupli
    , repli
    , dropEvery
    ) where

import P1To10

-- Problem 11
data ListItem a = Single a | Multiple Int a
  deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified xs = map encodeHelper $ encode xs
  where
    encodeHelper (1,x) = Single x
    encodeHelper (n,x) = Multiple n x

-- Problem 12
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
    where
      decodeHelper (Single x)     = [x]
      decodeHelper (Multiple n x) = replicate n x

-- Problem 13
encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
    where
      helper x [] = [(1,x)]
      helper x (y@(a,b):ys)
        | x == b    = (1+a,x):ys
        | otherwise = (1,x):y:ys
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = map encodeHelper . encode'
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x

-- Problem 14
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x,x])

-- Problem 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- Problem 16
dropEvery :: Eq a => [a] -> Int -> [a]
dropEvery (xs) n = reverse $ fst $ foldl (\acc x ->
                                        if (snd acc `mod` n) == 0
                                        then (fst acc, (snd acc + 1))
                                        else
                                          let arr = fst acc
                                          in ((x:arr), (snd acc + 1))) ([], 1) xs
