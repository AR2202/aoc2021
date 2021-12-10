module Day10
  ( day10a
  ) where

import           Common
import qualified Data.Map   as M
import           Data.Maybe (isJust)

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
