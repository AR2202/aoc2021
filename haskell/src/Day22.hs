{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Day22
  ( readDay22
  , inArea
  , area1
  , area2
  , area3
  , cubeInArea
  , Cubestate(..)
  , testCubes
  , applyInstructions
  , solve22a
  ) where

import           Common
import           Control.Lens
import           Control.Lens.Regex.Text
import           Data.Char               (toUpper)
import           Data.List.Split         (splitOn)
import qualified Data.Map                as M
import           Data.Maybe
import qualified Data.Text               as T

import           Text.Read

----------
-- Types
----------
data Cubestate
  = On
  | Off
  deriving (Show, Read, Eq)

type Coord = (Int, Int, Int)

type Cube = (Coord, Cubestate)

data Area =
  Area
    { x_start :: Int
    , x_end   :: Int
    , y_start :: Int
    , y_end   :: Int
    , z_start :: Int
    , z_end   :: Int
    }
  deriving (Show, Read, Eq)

--------------------
-- reading the input
--------------------
readDay22 filename = do
  instructions <- loadAndSplitLines filename
  let splitIntsructions = map words instructions
  let onstates = map (read . capitalise . head) splitIntsructions
  let areas = map (parseAsArea . last) splitIntsructions
  let instlist = zip areas onstates
  return instlist

parseAsIntList :: String -> [[Int]]
parseAsIntList = map (map read . splitOn ".." . drop 2) . splitOn ","

parseIntListAsArea :: [[Int]] -> Area
parseIntListAsArea list =
  Area
    (list !! 0 !! 0)
    (list !! 0 !! 1)
    (list !! 1 !! 0)
    (list !! 1 !! 1)
    (list !! 2 !! 0)
    (list !! 2 !! 1)

parseAsArea :: String -> Area
parseAsArea = parseIntListAsArea . parseAsIntList

capitalise []     = []
capitalise (x:xs) = toUpper x : xs

----------------
-- Part 1
----------------
day22a :: IO ()
day22a = solve22a "../input/day22.txt"

solve22a :: String -> IO ()
solve22a filename = do
  instlist <- readDay22 filename
  let relevantInst = filter fstInTotalArea instlist
  let states = applyInstructions allCubesInArea relevantInst
  print $ fst states

inArea :: Area -> Area -> Bool
inArea a1 a2 = xIn && yIn && zIn
  where
    xIn = (x_start a1) >= (x_start a2) && (x_end a1) <= (x_end a2)
    yIn = (y_start a1) >= (y_start a2) && (y_end a1) <= (y_end a2)
    zIn = (z_start a1) >= (z_start a2) && (z_end a1) <= (z_end a2)

isOutsideArea :: Area -> Area -> Bool
isOutsideArea a1 a2 =
  (a1 `isOutsideX` a2) || (a1 `isOutsideY` a2) || (a1 `isOutsideZ` a2)

isOutside start1 start2 end1 end2 = (start1 > end2) || (start2 > end1)

isOutsideX :: Area -> Area -> Bool
isOutsideX a1 a2 = isOutside (x_start a1) (x_start a2) (x_end a1) (x_end a2)

isOutsideY :: Area -> Area -> Bool
isOutsideY a1 a2 = isOutside (y_start a1) (y_start a2) (y_end a1) (y_end a2)

isOutsideZ :: Area -> Area -> Bool
isOutsideZ a1 a2 = isOutside (z_start a1) (z_start a2) (z_end a1) (z_end a2)

cubeInArea :: Coord -> Area -> Bool
cubeInArea (x, y, z) area =
  x_start area <= x &&
  x <= x_end area &&
  y_start area <= y && y <= y_end area && z_start area <= z && z <= z_end area

changeCubeStates [] cube = Off
changeCubeStates (x:xs) cube
  | cubeInArea cube (fst x) = snd x
  | otherwise = changeCubeStates xs cube

allCubesInArea =
  [(x, y, z) | x <- [-50 .. 50], y <- [-50 .. 50], z <- [-50 .. 50]]

addToCubeState list cube (on, off)
  | changeCubeStates list cube == Off = (on, off + 1)
  | otherwise = (on + 1, off)

applyInstructions cubelist instlist =
  foldr (addToCubeState (reverse instlist)) (0, 0) cubelist

fstInTotalArea (x, y) = inTotalArea x

-------------
-- test data
-------------
area1 = Area 1 4 1 4 1 4

area2 = Area 2 3 2 3 2 3

area3 = Area (-1) 3 (-2) 3 (-2) 3

totalArea = Area (-50) 50 (-50) 50 (-50) 50

inTotalArea = flip inArea totalArea

testCubes :: [Coord]
testCubes = [(x, y, z) | x <- [-1 .. 1], y <- [-1 .. 1], z <- [-1 .. 1]]
