module Day04
  ( day4a
  , day4b
  , example4b
  ) where

import           Common
import           Data.List
import           Data.Maybe (fromMaybe)

----------------------
-- Types
----------------------
data BingoBoard =
  BingoBoard
    { rows    :: [[Int]]
    , columns :: [[Int]]
    }
  deriving (Show, Read, Eq)

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
-- Part 1
----------------------
day4a :: IO Int
day4a = readBoards playBingo "../input/day4.txt"

filterOutSpaces :: [String] -> [String]
filterOutSpaces = filter (/= "")

listToBingoBoard :: [[String]] -> BingoBoard
listToBingoBoard list =
  BingoBoard ((map . map) read list) ((map . map) read $ transpose list)

isWinning :: BingoBoard -> Bool
isWinning board = completeRow || completeColumn
  where
    completeRow = elem [] $ rows board
    completeColumn = elem [] $ columns board

crossOut :: Int -> BingoBoard -> BingoBoard
crossOut number board = BingoBoard newrows newcolumns
  where
    newrows = map (filter (/= number)) $ rows board
    newcolumns = map (filter (/= number)) $ columns board

countRemaining :: BingoBoard -> Int
countRemaining board = sum $ map sum $ rows board

playBingo :: [BingoBoard] -> [Int] -> ([BingoBoard], Maybe Int)
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
losingBingo :: [BingoBoard] -> [Int] -> ([BingoBoard], Maybe Int)
losingBingo boards [] = ([], Nothing)
losingBingo boards (x:xs)
  | (length boards == 1) && (winningBoards /= []) = (winningBoards, Just x)
  | otherwise = losingBingo removedWinning xs
  where
    removedWinning = filter (not . isWinning) newboardstates
    newboardstates = map (crossOut x) boards
    winningBoards = filter isWinning newboardstates

day4b :: IO Int
day4b = readBoards losingBingo "../input/day4.txt"

example4b :: IO Int
example4b = readBoards losingBingo "../input/example4.txt"
