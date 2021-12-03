module Day3
  ( example3a
  , day3a
  ) where

import           Common
import Data.List
import Data.Binary


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
  |ones > zeros = '1'
  |otherwise = '0'
    where ones = length .  filter (== '1') $ bitstates
          zeros = length .  filter (== '0')$ bitstates 

invertState '0' = '1' 
invertState '1' = '0'

string2decimal "" = 0
string2decimal string =
  (readChar2digit . head) string * 2 ^ (length string - 1) +
  string2decimal (tail string)

readChar2digit '1' = 1
readChar2digit '0' = 0
