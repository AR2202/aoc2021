module Day7
  ( example7
  , day7a
  , day7b
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

-----------
-- Part 2
-----------
mean list = round (fromIntegral (sum list) / fromIntegral (length list))

roundupmean list =
  ceiling (fromIntegral (sum list) / fromIntegral (length list))

rounddownmean list = sum list `div` length list

fuelToMean list = sum $ map (sumTon . abs . (m -)) list
  where
    m = mean list

fuelToRD list = sum $ map (sumTon . abs . (m -)) list
  where
    m = rounddownmean list

fuelToRU list = sum $ map (sumTon . abs . (m -)) list
  where
    m = roundupmean list

sumTon n = sum [1 .. n]

minimumFuel list = minimum [fuelToRU list, fuelToRD list]

day7b = minimumFuel <$> makeListInt "../input/day7.txt"
