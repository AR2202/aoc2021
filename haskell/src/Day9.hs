module Day9
  ( read_day9
  , day9a
  ) where

import           Common
import           Data.Char (digitToInt)

data Point =
  Point
    { x   :: Int
    , y   :: Int
    , val :: Int
    }
  deriving (Show, Read, Eq)

------------------------
-- Part 1
-----------------------
-- very inefficient
read_day9 :: String -> IO Int
read_day9 filename = do
  contents <- loadAndSplitLines filename
  let withY = zipWith zip (repeat [1 ..]) contents
  let withX = concat $ zipWith zip (map repeat [1 ..]) withY
  let points = map tripleToPoint withX
  let lowpoints = lowPoints points
  let risks = sum $ map ((+ 1) . val) lowpoints
  return risks

day9a :: IO ()
day9a = read_day9 "../input/day9.txt" >>= print

tripleToPoint :: (Int, (Int, Char)) -> Point
tripleToPoint (a, (b, c)) = Point a b (digitToInt c)

neighbours :: Point -> [Point] -> [Point]
neighbours a = filter (isNeighbour a)

isNeighbour a b =
  x a == x b && y b `elem` [y a + 1, y a - 1] ||
  y a == y b && x b `elem` [x a + 1, x a - 1]

lowPoints list = filter (\x -> all (smallerVal x) (neighbours x list)) list

smallerVal p1 p2 = val p1 < val p2
