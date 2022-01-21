module Day02
  ( example2a
  , example2b
  , day2a
  , day2b
  ) where

import           Common
import           Data.Char

---------------------------------
-- Types and Typeclass Instances
---------------------------------
data Instructions
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show, Read)

data Position =
  Position
    { horizontal :: Int
    , depth      :: Int
    }
  deriving (Show, Read, Eq)

data PosAim =
  PosAim
    { pos :: Position
    , aim :: Int
    }
  deriving (Show, Read, Eq)

-------------------
-- day 2 part 1
-------------------
parseInstructions :: String -> IO [Instructions]
parseInstructions input = map (read . capitalise) <$> loadAndSplitLines input

capitalise :: [Char] -> [Char]
capitalise []     = []
capitalise (x:xs) = toUpper x : xs

move :: Position -> Instructions -> Position
move p (Forward x) = p {horizontal = horizontal p + x}
move p (Down x)    = p {depth = depth p + x}
move p (Up x)      = p {depth = depth p - x}

multiplyPosition :: Position -> Int
multiplyPosition p = horizontal p * depth p

solution2a :: String -> IO Int
solution2a input =
  multiplyPosition . foldl move (Position 0 0) <$> parseInstructions input

example2a :: IO Int
example2a = solution2a "example2.txt"

day2a :: IO Int
day2a = solution2a "day2.txt"

------------------------
-- day 2 part 2
------------------------
move2 :: PosAim -> Instructions -> PosAim
move2 p (Forward x) =
  p {pos = Position (horizontal (pos p) + x) (depth (pos p) + x * aim p)}
move2 p (Down x) = p {aim = aim p + x}
move2 p (Up x) = p {aim = aim p - x}

solution2b :: String -> IO Int
solution2b input =
  multiplyPosition . pos . foldl move2 (PosAim (Position 0 0) 0) <$>
  parseInstructions input

example2b :: IO Int
example2b = solution2b "example2.txt"

day2b :: IO Int
day2b = solution2b "day2.txt"
