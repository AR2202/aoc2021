module Day13
  ( solve13
  ) where

import           Common
import           Data.List       (nub)
import           Data.List.Split (splitOn)

-----------
-- Types
-----------
newtype Point =
  Point (Int, Int)
  deriving (Show, Read, Eq)

------------
-- read input
-------------
readDay13 filename = do
  contents <- splitOnBlankLine filename
  let points = map (read . appendPointToString) (lines $ head contents)
  let foldInstructions =
        map (splitOn "=" . flip (!!) 2 . words) $ lines $ contents !! 1
  return (points, foldInstructions)

solve13 = do
  (points, foldInstructions) <- readDay13 "../input/day13.txt"
  let foldDirection = map (foldingFunction . head) foldInstructions
  let foldLine = map (read . flip (!!) 1) foldInstructions
  let firstFoldDir = head foldDirection
  let firstLine = head foldLine
  let afterFirstFold = length $ nub $ firstFoldDir firstLine points
  putStrLn "Part 1:"
  print afterFirstFold
  let allFolds = zipWith ($) foldDirection foldLine
  let part2 = foldl (\p foldinst -> nub $ foldinst p) points allFolds
  putStrLn "Part 2:"
  rendering part2

appendPointToString s = "Point (" ++ s ++ ")"

belowFold f (Point (x, y)) = y > f

rightOfFold f (Point (x, y)) = x > f

afterVerticalFold f (Point (x, y))
  | belowFold f (Point (x, y)) = Point (x, 2 * f - y)
  | otherwise = Point (x, y)

foldVertical f = map (afterVerticalFold f)

afterHorizFold f (Point (x, y))
  | rightOfFold f (Point (x, y)) = Point (2 * f - x, y)
  | otherwise = Point (x, y)

foldHoriz f = map (afterHorizFold f)

foldingFunction "x" = foldHoriz
foldingFunction _   = foldVertical

xval (Point (x, y)) = x

yval (Point (x, y)) = y

minx pointlist = minimum $ map xval pointlist

maxx pointlist = maximum $ map xval pointlist

miny pointlist = minimum $ map yval pointlist

maxy pointlist = maximum $ map yval pointlist

oneline pointlist y =
  [ if Point (x, y) `elem` pointlist
    then '#'
    else '.'
  | x <- [minx pointlist .. maxx pointlist]
  ]

image pointlist = map (oneline pointlist) [miny pointlist .. maxy pointlist]

rendering pointlist = mapM_ putStrLn (image pointlist)
