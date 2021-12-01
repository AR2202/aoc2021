module Day1
  ( example1a
  , example1b
  , day1a
  , day1b
  ) where

import           Common

-------------------
-- day 1 part 1
-------------------
increases :: Int -> String -> IO Int
increases n input =
  let x = readLines input
   in fmap (length . filter (> 0)) $ zipWith (-) <$> fmap (drop n) x <*> x

example1a :: IO Int
example1a = increases 1 "example1.txt"

day1a :: IO Int
day1a = increases 1 "day1.txt"

---------------------
-- day 1 part 2
---------------------
example1b :: IO Int
example1b = increases 3 "example1.txt"

day1b :: IO Int
day1b = increases 3 "day1.txt"
