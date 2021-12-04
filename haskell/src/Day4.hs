module Day4
  ( day4a
  ) where

import           Common
import           Data.List
import           Data.Maybe (fromMaybe)

-----------------------
-- reading Input
-----------------------
readBoards filename = do
  input <- splitOnBlankLine filename
  let drawn = map read $ splitOnComma $ head input
  let boards =
        map (listToBingoBoard . map filterOutSpaces . splitOnSpace . lines) $
        tail input
  let winning = playBingo boards drawn
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
day4a = readBoards "../input/day4.txt"

filterOutSpaces = filter (/= "")

listToBingoBoard list =
  BingoBoard ((map . map) read list) ((map . map) read $ transpose list)

isWinning board = completeRow || completeColumn
  where
    completeRow = any (== []) $ rows board
    completeColumn = any (== []) $ columns board

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
    newboardstates = map (crossOut x) boards
    winningBoards = filter isWinning newboardstates
