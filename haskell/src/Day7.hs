module Day7
  ( example7
  , day7a
  ) where

import           Common
import           Data.List

------------
-- Part 1
------------
median :: [Int] -> Int
median list = sort list !! (length list `div` 2)

fuelToMedian :: [Int] -> Int
fuelToMedian list = sum $ map (abs . (med -)) list
  where
    med = median list

----------------
-- reading Input
----------------
example7list :: [Int]
example7list = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

example7 :: Int
example7 = fuelToMedian example7list

day7a :: IO Int
day7a = fuelToMedian <$> makeListInt "../input/day7.txt"
