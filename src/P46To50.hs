module P46To50 where

import System.IO (putStrLn)
import Data.List (sortBy, insertBy)
import Data.Ord (comparing)

-- NOT negates a single Boolean argument
not' :: Bool -> Bool
not' True  = False
not' False = True

-- Type signature for remaining logic functions
and',or',nor',nand',xor',impl',equ' :: Bool -> Bool -> Bool

-- AND is True if both a and b are True
and' True True = True
and' _    _    = False

-- OR is True if a or b or both are True
or' False False = False
or' _     _     = True

-- NOR is the negation of 'or'
nor'  a b = not' $ or'  a b

-- NAND is the negation of 'and'
nand' a b = not' $ and' a b

-- XOR is True if either a or b is True, but not if both are True
xor' True  False = True
xor' False True  = True
xor' _     _     = False

-- IMPL is True if a implies b, equivalent to (not a) or (b)
impl' a b = (not' a) `or'` b

-- EQU is True if a and b are equal
equ' True  True  = True
equ' False False = True
equ' _     _     = False

-- functions as in solution 46
infixl 4 `or'`
infixl 6 `and'`
-- "not" has fixity 9 by default

table :: (Bool -> Bool -> Bool) -> IO ()
table f =
  mapM_
    putStrLn
    [ show a ++ " " ++ show b ++ " " ++ show (f a b)
    | a <- [True, False]
    , b <- [True, False]
    ]

table' = mapM_ putStrLn . tableStrArr

tableStrArr ::  (Bool -> Bool -> Bool) -> [String]
tableStrArr f = do
   a <- [True, False]
   b <- [True, False]
   return $ show a ++ " " ++ show b ++ " " ++ show (f a b)

gray :: Int -> [String]
gray 0 = [""]
gray n = let xs = gray (n-1) in map ('0':) xs ++ map ('1':) (reverse xs)

data HTree a = Leaf a | Branch (HTree a) (HTree a)
                deriving Show

huffman :: (Ord a, Ord w, Num w) => [(a,w)] -> [(a,[Char])]
huffman freq = sortBy (comparing fst) $ serialize $
        htree $ sortBy (comparing fst) $ [(w, Leaf x) | (x,w) <- freq]
  where htree [(_, t)] = t
        htree ((w1,t1):(w2,t2):wts) =
                htree $ insertBy (comparing fst) (w1 + w2, Branch t1 t2) wts
        serialize (Branch l r) =
                [(x, '0':code) | (x, code) <- serialize l] ++
                [(x, '1':code) | (x, code) <- serialize r]
        serialize (Leaf x) = [(x, "")]