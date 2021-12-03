module Day3
  ( example3a
  , day3a
  , example3b
  , day3b
  ) where

import           Common
import           Data.List

-------------------
-- day 3 part 1
-------------------
example3a = solutionDay3a "../input/example3.txt"

day3a = solutionDay3a "../input/day3.txt"

solutionDay3a file = do
  input <- loadAndSplitLines file
  let bitlist = transpose input
  let mostCommon = map mostCommonBit bitlist
  let leastCommon = map invertState mostCommon
  print $ string2decimal mostCommon * string2decimal leastCommon

mostCommonBit bitstates
  | zeros > ones = '0'
  | otherwise = '1'
  where
    ones = length . filter (== '1') $ bitstates
    zeros = length . filter (== '0') $ bitstates

invertState '0' = '1'
invertState '1' = '0'

string2decimal "" = 0
string2decimal string =
  (readChar2digit . head) string * 2 ^ (length string - 1) +
  string2decimal (tail string)

readChar2digit '1' = 1
readChar2digit '0' = 0

-------------------
-- day 3 part 2
-------------------
example3b = solutionDay3b "../input/example3.txt"

day3b = solutionDay3b "../input/day3.txt"

solutionDay3b file = do
  input <- loadAndSplitLines file
  let ox = oxygenRating input
  let co2 = co2Rating input
  print $ string2decimal ox * string2decimal co2

keepMostCommon bitlist = filter (\x -> head x == mostCommon bitlist) bitlist

mostCommon bitlist = mostCommonBit $ map head bitlist

keepLeastCommon bitlist = filter (\x -> head x == leastCommon bitlist) bitlist

leastCommon bitlist = invertState $ mostCommon bitlist

rating handler1 handler2 bitlist = go bitlist ""
  where
    go [x] ys = reverse ys ++ x
    go xs ys  = go (map tail $ handler1 xs) (handler2 xs : ys)

oxygenRating = rating keepMostCommon mostCommon

co2Rating = rating keepLeastCommon leastCommon
