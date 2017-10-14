module P21To30
    ( insertAt
    , range
    , rnd_select
    , diff_select
    , combinations
    , rnd_permu
    , combinations
    ) where

import qualified P11To20
import Control.Monad
import System.Random
import Data.List
import Debug.Trace

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt c xs n = let (ys,zs) = P11To20.split xs n in
  concat [ys,(c:zs)]

-- Problem 22
range :: Int -> Int -> [Int]
range start end = [start .. end]

-- Problem 23
-- can select the same one twice
rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select xs n
  | n < 0 = return []
  | otherwise = do
    ys <- replicateM n $ getStdRandom $ randomR (0, length xs - 1)
    return [xs !! y | y <- ys]

-- can't select the same one twice
rnd_select2 l count = getStdRandom $ rnd_select_helper l count

rnd_select_helper :: RandomGen g => [a] -> Int -> g -> ([a], g)
rnd_select_helper _ 0 gen = ([], gen)
rnd_select_helper [] _ gen = ([], gen)
rnd_select_helper l count gen
  | count > (length l) = ([], gen)
  | otherwise = rnd_select_helper (P11To20.removeAt l (k + 1)) count gen'
  where
    (k, gen') = randomR (0, (length l) - 1) gen

-- Problem 24
diff_select :: Int -> Int -> IO [Int]
diff_select n m = rnd_select [1 .. m] n

-- Problem 25
rnd_permu :: [a] -> IO [a]
rnd_permu xs = rnd_select xs $ length xs

-- Problem 26
-- for every subset find all the combinations and append them to the first index
-- tails for [1,2,3] returns -> (1:2:3:[], 2:3:[], 3:[])
combinations :: Show a => Int -> [a] -> [[a]]
combinations 0 _  = return []
combinations n xs = do
  y:xs' <- tails xs
  ys <- combinations (n - 1) xs'
  return (y : ys)
