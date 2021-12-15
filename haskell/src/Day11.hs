module Day11
  ( day11a
  ) where

import           Common
import           Data.Char   (digitToInt)
import qualified Data.IntMap as IM

------------
-- Part 1
------------
day11a :: IO ()
day11a = loadData "../input/day11.txt"

----------------
-- reading input
----------------
loadData :: String -> IO ()
loadData filename = do
  contents <- loadAndSplitLines filename
  let flattened = map digitToInt $ concat contents
  let indexed = zip [0 .. 99] flattened
  let intmap = IM.fromList indexed
  let step100 = iterate step (intmap, 0) !! 100
  print $ snd step100

step :: (IM.IntMap Int, Int) -> (IM.IntMap Int, Int)
step (m, n) = recursiveStep IM.empty n (IM.map (+ 1) m)
  where
    recursiveStep flashed nFlashed newmap =
      if IM.null flashing
        then ( IM.map flash (IM.union flashed newmap)
             , nFlashed + IM.size flashed)
        else recursiveStep (IM.union flashing flashed) nFlashed $
             IM.difference
               (foldl
                  (\m k ->
                     IM.union
                       (IM.difference
                          (IM.union (IM.map (+ 1) (neighboursOfKey m k)) m)
                          flashed)
                       m)
                  newmap $
                IM.keys $ IM.difference flashing flashed)
               flashed
      where
        flashing = IM.filter (>= 10) newmap

neighboursOfKey :: IM.IntMap Int -> Int -> IM.IntMap Int
neighboursOfKey m k = IM.filterWithKey (\key val -> key `elem` (adjacent k)) m

adjacent :: Int -> [Int]
adjacent k
  | k `mod` 10 == 0 =
    filter (`elem` [0 .. 99]) [k + 1, k - 10, k + 10, k + 11, k - 9]
  | k `mod` 10 == 9 =
    filter (`elem` [0 .. 99]) [k - 1, k - 10, k + 10, k - 11, k + 9]
  | otherwise =
    filter
      (`elem` [0 .. 99])
      [k - 1, k + 1, k - 10, k + 10, k - 11, k + 11, k - 9, k + 9]

flash :: Int -> Int
flash a =
  if a >= 10
    then 0
    else a
