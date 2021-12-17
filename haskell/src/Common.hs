{-# LANGUAGE ScopedTypeVariables #-}

module Common
  ( loadInput
  , loadAndSplitLines
  , splitOnBlankLine
  , readLines
  , loadPadded
  , readTuple
  , splitOnEq
  , splitOnSpace
  , splitOnComma
  , makeListInt
  , readChar2digit
  , string2decimal
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.List.Split

dir = "../input/"

filepath filename = dir ++ filename

loadInput filename = readFile $ filepath filename

loadAndSplitLines filename = do
  contents <- loadInput filename
  let linesOfFile = lines contents
  return linesOfFile

makeListInt :: String -> IO [Int]
makeListInt filename = map read . splitOnComma <$> loadInput filename

readLines :: Read a => String -> IO [a]
readLines filename = map read <$> loadAndSplitLines filename

splitOnBlankLine filename = splitOn "\n\n" <$> loadInput filename

splitOnEq filename = map (splitOn " = ") <$> loadAndSplitLines filename

splitOnSpace = map (splitOn " ")

splitOnComma = splitOn ","

loadPadded filename = do
  contents <- loadInput filename
  let linesOfFile = lines contents
  let linesWPadding = map (\line -> '.' : line ++ ".") linesOfFile
  let len = length $ head linesOfFile
  let paddingline = replicate len '.'
  let paddingadded = paddingline : linesWPadding ++ [paddingline]
  return paddingadded

readTuple :: (Read a, Read b) => (String, String) -> (a, b)
readTuple (x, y) = (read x, read y)

string2decimal "" = 0
string2decimal string =
  (readChar2digit . head) string * 2 ^ (length string - 1) +
  string2decimal (tail string)

readChar2digit '1' = 1
readChar2digit '0' = 0
