module Day06
  ( fasterOffspring
  ) where

import           Common
import           Data.IntMap as IM

-----------
-- Part 1
-----------
offspring :: Int -> [Int]
offspring repDays = go [] repDays
  where
    go childlist remdays
      | remdays <= 0 = childlist
      | otherwise = go ((remdays - 9) : childlist) (remdays - 7)

example6list :: [Int]
example6list = [3, 4, 3, 1, 2]
 --   days = 80 - initial

-- this is a bit inefficient
--allOffspring :: Int -> Int
--allOffspring initial =
--  sum $ fmap length $ take (days `div` 7) $ iterate offspring [days]
--  where
--
----day6a :: IO Int
--day6a = sum . fmap allOffspring <$> makeListInt "../input/day6.txt"
allOffspring' recFn x = concatMap recFn (offspring x)

--f_list :: [Int]
offspringList = Prelude.map (allOffspring' fasterOffspring) [0 ..]

--fasterOffspring :: Int -> Int
fasterOffspring n = offspringList !! n
