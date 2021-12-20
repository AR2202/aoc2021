module Day20
  ( day20a
  ) where

import           Common
import           Control.Comonad.Store
import           Data.List             (nub)
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

------------
-- Types
------------
data Pixel
  = Light
  | Dark
  deriving (Show, Read, Eq)

type Coord = (Integer, Integer)

type Algorithm = [Pixel]

--preparing Start Values-----------------------------------
initialImage :: [(Coord, Pixel)] -> Coord -> Store Coord Pixel
initialImage initial coord = mkStore (M.fromList initial) coord Dark

checkPixel :: M.Map Coord Pixel -> Pixel -> Coord -> Pixel
checkPixel pixelmap infstate coord
  | coord `M.member` pixelmap = fromJust $ M.lookup coord pixelmap
  | otherwise = infstate

mkStore :: M.Map Coord Pixel -> Coord -> Pixel -> Store Coord Pixel
mkStore initial ind infstate = store checkFunction ind
  where
    checkFunction = checkPixel initial infstate

--logic of updating one cell-------------------------
neighbours (x, y) = [(x1, y1) | y1 <- [y - 1 .. y + 1], x1 <- [x - 1 .. x + 1]]

checkNeighbours :: Store Coord Pixel -> [Pixel]
checkNeighbours pixels = experiment neighbours pixels

toBinNeighbours :: Store Coord Pixel -> String
toBinNeighbours = map pixelToBin . checkNeighbours

neighboursToPixelState :: Algorithm -> Store Coord Pixel -> Pixel
neighboursToPixelState algorithm pixels =
  algorithm !! (string2decimal . map pixelToBin . checkNeighbours) pixels

pixelToBin :: Pixel -> Char
pixelToBin Light = '1'
pixelToBin Dark  = '0'

--taking one step-------------------------------------------------
step ::
     (Store Coord Pixel, M.Map Coord Pixel, Pixel, Algorithm)
  -> (Store Coord Pixel, M.Map Coord Pixel, Pixel, Algorithm)
step (pixels, img, infstate, algorithm) =
  (newpixs, newImage, newInfstate, algorithm)
  where
    extended = extend (neighboursToPixelState algorithm) pixels
    newpixs = updateStore img extended infstate algorithm
    newImage = updateMap img extended infstate
    newInfstate =
      if infstate == Dark
        then head algorithm
        else last algorithm

updateMap ::
     M.Map Coord Pixel -> Store Coord Pixel -> Pixel -> M.Map Coord Pixel
updateMap currentMap currentStore infstate = M.fromList $ zip newkeys newvals
  where
    newkeys = nub $ concatMap neighbours (M.keys currentMap)
    newvals = map (`peek` currentStore) newkeys

updateStore currentMap currentStore infstate algorithm =
  mkStore newMap ind newInfstate
  where
    newMap = updateMap currentMap currentStore infstate
    ind = pos currentStore
    newInfstate =
      if infstate == Dark
        then head algorithm
        else last algorithm

-----------finding light pixels --------
numLight = length . M.keys . M.filter (== Light)

---------input----------
take3steps :: String -> IO ()
take2steps filename = do
  contents <- splitOnBlankLine filename
  let alg = head contents
  let im = last contents
  let algorithm = map algo2state alg
  let ls = lines im
  let xvals = map (zip [0 ..]) ls
  let states = concat $ zipWith toStates [0 ..] xvals
  let istore = initialImage states (0, 0)
  let initialmap = M.fromList states
  let step2 = step $ step (istore, initialmap, Dark, algorithm)
  let lightpix = numLight $ mapFromStep step2
  print lightpix

toStates x = map (toState x)

toState x (a, b) = ((a, x), algo2state b)

storeFromStep (s, m, i, a) = s

mapFromStep (s, m, i, a) = m

------------
-- Part 1
------------
day20a :: IO ()
day20a = take2steps "../input/day20.txt"

example20a :: IO ()
example20a = take2steps "../input/example20.txt"

----------- testdata from example ------
testInitial =
  [ ((0, 0), Dark)
  , ((0, 1), Dark)
  , ((0, 2), Dark)
  , ((1, 0), Light)
  , ((1, 1), Dark)
  , ((1, 2), Dark)
  , ((2, 0), Dark)
  , ((2, 1), Light)
  , ((2, 2), Dark)
  ]

teststore = initialImage testInitial (1, 1)

testmap = M.fromList testInitial

algo2state '.' = Dark
algo2state '#' = Light

testalgorithm =
  map
    algo2state
    "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##"

testStep = step (teststore, testmap, Dark, testalgorithm)

middle = extract $storeFromStep testStep
