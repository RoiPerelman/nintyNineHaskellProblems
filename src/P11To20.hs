module P11To20
  ( ListItem(Single, Multiple)
  , encodeModified
  , decodeModified
  , encodeDirect
  , dupli
  , repli
  , dropEvery
  , split
  , slice
  , rotate
  , removeAt
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
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x

-- Problem 13
encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
  where
    helper x [] = [(1, x)]
    helper x (y@(a, b):ys)
      | x == b = (1 + a, x) : ys
      | otherwise = (1, x) : y : ys
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = map encodeHelper . encode'
  where
    encodeHelper (1, x) = Single x
    encodeHelper (n, x) = Multiple n x

-- Problem 14
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x,x])

-- Problem 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- Problem 16
dropEvery :: Eq a => [a] -> Int -> [a]
dropEvery (xs) n =
  reverse $
  fst $
  foldl
    (\acc x ->
       if (snd acc `mod` n) == 0
         then (fst acc, (snd acc + 1))
         else let arr = fst acc
              in ((x : arr), (snd acc + 1)))
    ([], 1)
    xs

-- Problem 17
split :: [a] ->Int -> ([a], [a])
split xs n
  | n < 0 = (xs, [])
  | otherwise =
    let (ys, zs) =
          fst $
          foldl
            (\((accLeft, accRight), i) x ->
               if (i < n)
                 then (((x : accLeft), accRight), i + 1)
                 else ((accLeft, (x : accRight)), i + 1))
            (([], []), 0)
            xs
    in (reverse ys, reverse zs)

slice :: [a] -> Int -> Int -> [a]
slice xs n1 n2
  | n1 < 0 = xs
  | n2 < 0 = xs
  | otherwise =
    take (n2 - n1 + 1) $ drop (n1 - 1) xs

rotate :: [a] -> Int -> [a]
rotate xs n
  | n < 0 = rotate xs $ length xs + n
  | otherwise = let (ys, zs) = split xs n in
      concat [zs,ys]

removeAt :: [a] -> Int -> [a]
removeAt xs n
  | n < 0 = xs
  | otherwise =
    concat [take (n-1) xs, drop (length xs - n) xs]
