module Day10
  ( day10a
  , day10b
  ) where

import           Common
import           Data.List  (sort)
import qualified Data.Map   as M
import           Data.Maybe (fromJust, isJust)

-------------
-- Part 1
-------------
day10a :: IO ()
day10a = loadDay10 "../input/day10.txt"

loadDay10 :: String -> IO ()
loadDay10 filename = do
  contents <- loadAndSplitLines filename
  let corrupt = map (corrupted []) contents
  let scores = map seScore $ filter isJust corrupt
  print $ sum scores

opening :: [Char]
opening = ['{', '[', '(', '<']

isOpening :: Char -> Bool
isOpening = flip elem opening

closing :: [Char]
closing = ['}', ']', ')', '>']

isClosing :: Char -> Bool
isClosing = flip elem closing

delimMap :: M.Map Char Char
delimMap = M.fromList $ zip closing opening

delimMap2 :: M.Map Char Char
delimMap2 = M.fromList $ zip opening closing

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

corrupted :: [Char] -> [Char] -> Maybe Char
corrupted _ [] = Nothing
corrupted open (x:xs)
  | isOpening x = corrupted (x : open) xs
  | isClosing x =
    case lup x of
      Nothing -> Nothing
      Just y ->
        if Just y == safeHead open
          then corrupted (tail open) xs
          else Just x
  where
    lup x = M.lookup x delimMap

seScore :: Maybe Char -> Int
seScore Nothing    = 0
seScore (Just ')') = 3
seScore (Just ']') = 57
seScore (Just '}') = 1197
seScore (Just '>') = 25137

-------
-- Part 2
---------
day10b :: IO ()
day10b = solvePart2 "../input/day10.txt"

corrupted2 :: [Char] -> [Char] -> Maybe String
corrupted2 open [] = traverse (`M.lookup` delimMap2) open
corrupted2 open (x:xs)
  | isOpening x = corrupted2 (x : open) xs
  | isClosing x =
    case lup x of
      Nothing -> Nothing
      Just y ->
        if Just y == safeHead open
          then corrupted2 (tail open) xs
          else Nothing
  where
    lup x = M.lookup x delimMap

solvePart2 :: String -> IO ()
solvePart2 filename = do
  contents <- loadAndSplitLines filename
  let incomplete = map fromJust . filter isJust $ map (corrupted2 []) contents
  let scores = sort $ map autocompleteScore incomplete
  print $ scores !! (length scores `div` 2)

score2 :: Char -> Int
score2 ')' = 1
score2 ']' = 2
score2 '}' = 3
score2 '>' = 4

autocompleteScore :: String -> Int
autocompleteScore = foldl (\acc delim -> acc * 5 + score2 delim) 0
