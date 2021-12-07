module Day6
  ( day6a
  ) where

import           Common

-----------
-- Part 1
-----------
offspring :: [Int] -> [Int]
offspring repDays = repDays >>= go []
  where
    go childlist remdays
      | remdays <= 0 = childlist
      | otherwise = go ((remdays - 9) : childlist) (remdays - 7)

example6list :: [Int]
example6list = [3, 4, 3, 1, 2]

-- this is a bit inefficient
allOffspring :: Int -> Int
allOffspring initial =
  sum $ fmap length $ take (days `div` 7) $ iterate offspring [days]
  where
    days = 80 - initial

day6a :: IO Int
day6a = sum . map allOffspring <$> makeListInt "../input/day6.txt"
