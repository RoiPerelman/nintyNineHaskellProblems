module P21To30
    ( insertAt
    , range
    , rnd_select
    ) where

import qualified P11To20
import Control.Monad
import System.Random

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt c xs n = let (ys,zs) = P11To20.split xs n in
  concat [ys,(c:zs)]

-- Problem 22
range :: Int -> Int -> [Int]
range start end = [start .. end]

-- Problem 23
rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select xs n
  | n < 0 = return []
  | otherwise = do ys <- replicateM n $ getStdRandom $ randomR (0, length xs - 1)
                   return [xs!!y | y <- ys]

-- Problem 24
