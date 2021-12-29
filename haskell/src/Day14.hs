module Day14
  ( day14a
  ) where

import           Common
import           Data.List       (foldl', nub, tails)
import           Data.List.Split (splitOn)
import qualified Data.Map        as M
import           Data.Maybe      (fromMaybe)

----------
-- Types
----------
type Polymer = String

type Replacements = M.Map String String

----------
-- Part 1
----------
day14a :: IO ()
day14a = solve14a "../input/day14.txt"

solve14a :: String -> IO ()
solve14a filename = do
  (polymer, replacements) <- readDay14 filename
  let replaced = iterate (step replacements) polymer !! 10
  let occurrances = countElements replaced
  let difference = maximum occurrances - minimum occurrances
  print difference

windows :: Int -> [a] -> [[a]]
windows n xs = map (take n) (tails xs)

step :: Replacements -> Polymer -> Polymer
step replacements polymer =
  head polymer :
  (concat $ map (fromMaybe "" . flip M.lookup replacements) $ windows 2 polymer)

countElements :: Polymer -> [Int]
countElements finalPolymer =
  map (\c -> (length . filter (== c)) finalPolymer) $ nub finalPolymer

---------------------
-- Parsing the input
---------------------
readDay14 :: String -> IO (Polymer, Replacements)
readDay14 filename = do
  contents <- splitOnBlankLine filename
  let polymer = head contents
  let replacements = makeReplacementMap $ lines $ last contents
  return (polymer, replacements)

makeReplacementMap :: [String] -> Replacements
makeReplacementMap xs = M.fromList $ map parseReplacement xs

parseReplacement :: String -> (String, String)
parseReplacement s = (original, insertLetter : rest)
  where
    original = (head . words) s
    insertLetter = (head . last . words) s
    rest = tail original
