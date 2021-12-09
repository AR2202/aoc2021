module Day8
  ( day8a
  , day8b
  ) where

import           Common

import           Control.Monad
import qualified Data.IntMap     as IM
import           Data.List       as L
import           Data.List.Split
import qualified Data.Map        as M
import           Data.Maybe      (fromJust)

---------------
-- Part 1
---------------
read_day8 filename = do
  contents <- loadAndSplitLines filename
  let splitnotes = map (splitOn "|") contents
  let outputs = map (map words . tail) splitnotes
  let allsignals = join outputs
  let wires = map head splitnotes
  return (wires, allsignals)

solvepart1 filename = do
  (_, allsignals) <- read_day8 filename
  let lengths = map length $ join allsignals
  let simple = length $filter simplesignal lengths
  print simple

day8a :: IO ()
day8a = solvepart1 "../input/day8.txt"

simplesignal x = x == 2 || x == 3 || x == 4 || x == 7

---------------
-- Part 2
---------------
solvepart2 filename = do
  (wires, allsignals) <- read_day8 filename
  let wiremaps = map makemap wires
  let changed = zipWith changeWirings wiremaps allsignals
  let decoded = map (read . map decode) changed
  print $ sum decoded

day8b = solvepart2 "../input/day8.txt"

isB list char = (length . filter (== char)) (concat (words list)) == 6

isE list char = (length . filter (== char)) (concat (words list)) == 4

isF list char = (length . filter (== char)) (concat (words list)) == 9

isG list char =
  ((length . filter (== char)) (concat (words list)) == 7) &&
  char `notElem` four
  where
    four = head . filter (\x -> (length x) == 4) $words list

isD list char =
  ((length . filter (== char)) (concat (words list)) == 7) && char `elem` four
  where
    four = head . filter (\x -> (length x) == 4) $words list

isA list char =
  ((length . filter (== char)) (concat (words list)) == 8) &&
  char `notElem` four
  where
    four = head . filter (\x -> (length x) == 4) $words list

isC list char =
  ((length . filter (== char)) (concat (words list)) == 8) && char `elem` four
  where
    four = head . filter (\x -> (length x) == 4) $words list

findChar fn list = head . filter (fn list) $ list

makemap list =
  M.insert (findChar isA list) 'a' $
  M.insert (findChar isB list) 'b' $
  M.insert (findChar isC list) 'c' $
  M.insert (findChar isD list) 'd' $
  M.insert (findChar isE list) 'e' $
  M.insert (findChar isF list) 'f' $ M.insert (findChar isG list) 'g' M.empty

changeWiring m word = L.sort [fromJust (M.lookup x m) | x <- word]

changeWirings m = map (changeWiring m)

decode "abcefg"  = '0'
decode "cf"      = '1'
decode "acdeg"   = '2'
decode "acdfg"   = '3'
decode "bcdf"    = '4'
decode "abdfg"   = '5'
decode "abdefg"  = '6'
decode "acf"     = '7'
decode "abcdefg" = '8'
decode "abcdfg"  = '9'
