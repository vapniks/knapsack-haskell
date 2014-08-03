module Main where

import Data.List
import Data.Ord
-- modules for parallel processing
import Control.Parallel.Strategies
import Control.DeepSeq
-- modules for argument handling
import System.Environment
import System.Exit
import System.IO

------------------------------------------------------------
-- Utility.
------------------------------------------------------------

-- | Compare with the first function. In the event of a tiebreak, compare with the next.
-- | I expect there's a built-in for this, but I can't find it.
tiebreak :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
tiebreak f g a b
  | f a b == EQ = g a b
  | otherwise = f a b

------------------------------------------------------------
-- The knapsack algorithm
------------------------------------------------------------

class Costed a where
  cost :: a -> Integer

totalCost :: Costed a => [a] -> Integer
totalCost = sum . fmap cost

costWithin :: Costed a => Integer -> a -> Bool
costWithin w a = w >= cost a

largestSolution :: Costed a => [[a]] -> [a]
largestSolution [] = []
largestSolution is = maximumBy (tiebreak (comparing totalCost) (flip (comparing length))) is

-- TODO : Memoize.
knapsack :: Costed a => (a -> [a] -> [a]) -> Integer -> [a] -> Strategy [[a]] -> [a]
knapsack _ _ [] _ = []
knapsack prune w xs strategy = case (w > 0) of
                                 True -> largestSolution possibleSolutions
                                 False -> []
         where validItems = filter (costWithin w) xs 
               possibleSolutions = map knapsackWithout validItems `using` strategy
               knapsackWithout i = i : knapsack prune (w - cost i) (prune i validItems) (evalList rseq)

-- | Unbounded Knapsack. There is an unlimited number of each item.
uks :: Costed a => Integer -> [a] -> Strategy [[a]] -> [a]
uks = knapsack (flip const) 

-- | Bounded Knapsack. Items can only be consumed once.
bks :: (Eq a, Costed a) => Integer -> [a] -> Strategy [[a]] -> [a]
bks = knapsack delete 

------------------------------------------------------------
-- Items to put in the knapsack
------------------------------------------------------------

data Item = Item String Integer
              deriving (Read, Show, Eq)

instance NFData Item where
  rnf (Item s i) = rnf s `seq` rnf i

instance Costed Item where
  cost (Item _ w) = w

items :: [Item]
items = [
           Item "1" 1,
           Item "2" 2,
           Item "3" 3,
           Item "4" 4,
           Item "5" 5,
           Item "6" 6,
           Item "7" 5,
           Item "8" 4,
           Item "9" 3,
           Item "10" 2
        ]

 ------------------------------------------------------------

-- main :: IO ()
-- main = do
--   print (totalCost b, b)
--   print (totalCost u, u)
--   where n = 30
--         b = bks n items True
--         u = uks n items True

main :: IO ()
main = do args <- getArgs
          case args of
            ["t"] -> doit (parBuffer 100 rdeepseq)
            ["f"] -> doit (evalList rseq)
            _ -> do hPutStrLn stderr "Usage: ./Main t/f"
                    exitWith $ ExitFailure 1 

doit :: Strategy [[Item]] -> IO ()
doit strategy = do print (totalCost b, b)
                   print (totalCost u, u)
                where n = 20
                      b = bks n items strategy
                      u = uks n items strategy
