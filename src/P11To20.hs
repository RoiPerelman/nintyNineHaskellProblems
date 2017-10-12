module P11To20
    ( myLast
    , myButLast
    , elementAt
    , myLength
    , myReverse
    , isPalindrome
    , flatten
    , NestedList (Elem, List)
    , compress
    , pack
    , encode
    ) where

import Control.Monad
import Data.List

-- Problem 1
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myLast2 :: [a] -> a
myLast2 = head . reverse
--last2 xs = head $ reverse xs

-- Problem 2
myButLast = head . drop 1 . reverse

-- foldl :: (b -> a -> b) -> b -> t a -> b
-- where b is acc pair (,) and a is []
myButLast2 :: Foldable f => f a -> a
myButLast2 = fst . foldl (\(a,b) x -> (b,x)) (err1,err2)
  where
    err1 = error "lastbut1: Empty list"
    err2 = error "lastbut1: Singleton"

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:_) 1  = x
elementAt (_:xs) i = elementAt xs (i - 1)
elementAt _ _      = error "Index out of bounds"

-- Problem 4
myLength :: Num a => [a] -> a
myLength = sum . map (\_->1)

-- Problem 5
myReverse :: [a] -> [a]
myReverse xs = foldl (\acc x -> x:acc) [] xs

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

isPalindrome2 :: (Eq a) => [a] -> Bool
isPalindrome2 = liftM2 (==) id reverse

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a   )   = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

flatten2 :: NestedList a -> [a]
flatten2 (Elem x) = [x]
flatten2 (List x) = concatMap flatten x

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress x = reverse $ foldl (\acc x -> if (head acc) == x then acc else x:acc) [head x] x

compress2 :: Eq a => [a] -> [a]
compress2 = map head . group

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : (pack $ dropWhile (==x) xs)

pack2 :: Eq a => [a] -> [[a]]
pack2 = foldr packFold [] where
  packFold x [] = [[x]]
  packFold x (ys:yss) =
    if head ys == x then ((x:ys):yss) else ([x]:ys:yss)

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode xss = map (\xs -> (length xs, head xs)) $ pack xss

