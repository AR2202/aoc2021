{-# LANGUAGE BangPatterns #-}

module Day21
  ( addToPos
  , example21a
  , determiniticDie
  , day21a
  , diracDice
  , allScores
  , winningPlayer
  , Player(..)
  , isScore21
  , rounds
  ) where

import           Common

import           Data.List.Split (chunksOf)

-----------
-- Types
-----------
data Player
  = Player1
  | Player2
  deriving (Show, Read, Eq)

------------
-- Part 1
------------
determiniticDie :: [[Int]]
determiniticDie = chunksOf 3 $ cycle [1 .. 100]

addToPos :: Int -> [[Int]] -> Int
addToPos pos chunks =
  if newpos `mod` 10 == 0
    then 10
    else newpos `mod` 10
  where
    newpos = pos + sum (head chunks)

switchPlayer :: Player -> Player
switchPlayer Player1 = Player2
switchPlayer Player2 = Player1

diracDice :: Int -> Int -> Int
diracDice start1 start2 = go start1 start2 0 0 determiniticDie 0 Player1
  where
    go pos1 pos2 score1 score2 dieVals dieRolls player
      | maximum [score1, score2] >= 1000 = dieRolls * minimum [score1, score2]
      | player == Player1 =
        go
          pos1new
          pos2
          (score1 + pos1new)
          score2
          (tail dieVals)
          (dieRolls + 3)
          nextPlayer
      | otherwise =
        go
          pos1
          pos2new
          score1
          (score2 + pos2new)
          (tail dieVals)
          (dieRolls + 3)
          nextPlayer
      where
        pos1new = addToPos pos1 dieVals
        pos2new = addToPos pos2 dieVals
        nextPlayer = switchPlayer player

example21a :: Int
example21a = diracDice 4 8

day21a :: Int
day21a = diracDice 1 5

--------
-- Part2
--------
addToPos' :: Int -> Int -> Int
addToPos' pos sumOfDieRolls =
  if newpos `mod` 10 == 0
    then 10
    else newpos `mod` 10
  where
    newpos = pos + sumOfDieRolls

allPos currentpos =
  map (addToPos' currentpos) $
  fmap (+) (fmap (+) [1, 2, 3] <*> [1, 2, 3]) <*> [1, 2, 3]

allScores (currentpos, currentscore) =
  zip (allPos currentpos) (map (+ currentscore) $allPos currentpos)

isScore21 :: (a, Int) -> Bool
isScore21 = (>= 21) . snd

winningPlayer :: Int -> Int -> Player
winningPlayer roundsP1 roundsP2 =
  if roundsP1 > roundsP2
    then Player2
    else Player1

winners :: [Int] -> [Int] -> [Player]
winners allroundsP1 allroundsP2 = winningPlayer <$> allroundsP1 <*> allroundsP2

rounds startpos = go [(startpos, 0)] [] 0
  where
    go [] rounds currentRound = rounds
    go !vals !rounds currentRound = go newvals newrounds (currentRound + 1)
      where
        newvals = filter (not . isScore21) vals >>= allScores
        newrounds = rounds ++ map (const currentRound) (filter isScore21 vals)
