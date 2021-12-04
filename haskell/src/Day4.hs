module Day4
  ( day4a
  , day4b
  , example4b
  ) where

import           Common
import           Data.List
import           Data.Maybe (fromMaybe)

-----------------------
-- reading Input
-----------------------
readBoards solver filename = do
  input <- splitOnBlankLine filename
  let drawn = map read $ splitOnComma $ head input
  let boards =
        map (listToBingoBoard . map filterOutSpaces . splitOnSpace . lines) $
        tail input
  let winning = solver boards drawn
  let score = countRemaining $ head $ fst winning
  let lastNumber = fromMaybe 0 $ snd winning
  let answer = score * lastNumber
  return answer

----------------------
-- Types
----------------------
data BingoBoard =
  BingoBoard
    { rows    :: [[Int]]
    , columns :: [[Int]]
    }
  deriving (Show, Read, Eq)

----------------------
-- Part 1
----------------------
day4a = readBoards playBingo "../input/day4.txt"

filterOutSpaces = filter (/= "")

listToBingoBoard list =
  BingoBoard ((map . map) read list) ((map . map) read $ transpose list)

isWinning board = completeRow || completeColumn
  where
    completeRow = elem [] $ rows board
    completeColumn = elem [] $ columns board

crossOut number board = BingoBoard newrows newcolumns
  where
    newrows = map (filter (/= number)) $ rows board
    newcolumns = map (filter (/= number)) $ columns board

countRemaining board = sum $ map sum $ rows board

playBingo boards [] = ([], Nothing)
playBingo boards (x:xs)
  | winningBoards /= [] = (winningBoards, Just x)
  | otherwise = playBingo newboardstates xs
  where
    winningBoards = filter isWinning newboardstates
    newboardstates = map (crossOut x) boards

--------------------
-- Part 2
--------------------
losingBingo boards [] = ([], Nothing)
losingBingo boards (x:xs)
  | (length boards == 1) && (winningBoards /= []) = (winningBoards, Just x)
  | otherwise = losingBingo removedWinning xs
  where
    removedWinning = filter (not . isWinning) newboardstates
    newboardstates = map (crossOut x) boards
    winningBoards = filter isWinning newboardstates

day4b = readBoards losingBingo "../input/day4.txt"

example4b = readBoards losingBingo "../input/example4.txt"
