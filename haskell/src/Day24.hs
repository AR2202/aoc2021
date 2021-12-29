module Day24
  ( ProgramState(..)
  , Variable(..)
  , Instruction(..)
  , execute
  , runInstruction
  , runInstructions
  , readMaybeInt
  , readDay24
  , allEndValues
  ) where

import           Common
import           Data.Char  (toUpper)
import           Data.List  (foldl', nub)
import           Data.Maybe (fromJust)
import           Text.Read  (readMaybe)

-------------
-- Types
-------------
data Instruction
  = Input Variable
  | Add Variable (Either Variable Int)
  | Mul Variable (Either Variable Int)
  | Div Variable (Either Variable Int)
  | Mod Variable (Either Variable Int)
  | Eql Variable (Either Variable Int)
  deriving (Show, Read, Eq)

data ProgramState =
  ProgramState
    { w :: Int
    , x :: Int
    , y :: Int
    , z :: Int
    }
  deriving (Show, Read, Eq)

data Variable
  = W
  | X
  | Y
  | Z
  deriving (Show, Read, Eq)

type Instructions = [Instruction]

---------------------
-- parsing the Input
---------------------
readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

readMaybeVar :: String -> Maybe Variable
readMaybeVar = readMaybe

readVar :: String -> Variable
readVar = fromJust . readMaybeVar

readVarOrInt :: String -> Either Variable Int
readVarOrInt s =
  case readMaybeInt s of
    Nothing -> Left $ readVar s
    Just i  -> Right i

splitInst :: String -> [[String]]
splitInst = map words . lines

capitalise :: String -> String
capitalise []     = []
capitalise (x:xs) = toUpper x : xs

parseAsInst :: [String] -> Instruction
parseAsInst strs =
  case head strs of
    "inp" -> Input (readVar $capitalise $strs !! 1)
    "add" ->
      Add
        (readVar $capitalise $strs !! 1)
        (readVarOrInt $ capitalise $ strs !! 2)
    "mul" ->
      Mul
        (readVar $capitalise $strs !! 1)
        (readVarOrInt $ capitalise $ strs !! 2)
    "div" ->
      Div
        (readVar $capitalise $strs !! 1)
        (readVarOrInt $ capitalise $ strs !! 2)
    "mod" ->
      Mod
        (readVar $capitalise $strs !! 1)
        (readVarOrInt $ capitalise $ strs !! 2)
    "eql" ->
      Eql
        (readVar $capitalise $strs !! 1)
        (readVarOrInt $ capitalise $ strs !! 2)

parseInstructions :: String -> Instructions
parseInstructions = map parseAsInst . splitInst

allEndStates = ProgramState <$> [0 .. 9] <*> [0 .. 9] <*> [0 .. 9] <*> [0]

allEndValues :: [(ProgramState, [Int])]
allEndValues = zip allEndStates (repeat [])

------------
-- Part 1
------------
readDay24 :: String -> IO ()
readDay24 filename = do
  s <- loadInput filename
  let inst = parseInstructions s
  let endstates = allEndValues
  let inputs = runInstructionsBackwards endstates inst
  let max = maximum $ map (readMaybeInt . concatMap show) inputs
  print max

inputNumber :: Variable -> Int -> ProgramState -> ProgramState
inputNumber W i p = p {w = i}
inputNumber X i p = p {x = i}
inputNumber Y i p = p {y = i}
inputNumber Z i p = p {z = i}

retrieveVar :: Variable -> ProgramState -> Int
retrieveVar W p = w p
retrieveVar X p = x p
retrieveVar Y p = y p
retrieveVar Z p = z p

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail l  = tail l

execute :: Instruction -> Maybe Int -> ProgramState -> ProgramState
execute (Input var) (Just i) p = inputNumber var i p
execute (Input var) Nothing p = p
execute (Add var (Left var2)) _ p =
  inputNumber var (retrieveVar var p + retrieveVar var2 p) p
execute (Add var (Right i)) _ p = inputNumber var (retrieveVar var p + i) p
execute (Mul var (Left var2)) _ p =
  inputNumber var (retrieveVar var p * retrieveVar var2 p) p
execute (Mul var (Right i)) _ p = inputNumber var (retrieveVar var p * i) p
execute (Div var (Left var2)) _ p =
  inputNumber var (retrieveVar var p `div` retrieveVar var2 p) p
execute (Div var (Right i)) _ p = inputNumber var (retrieveVar var p `div` i) p
execute (Mod var (Left var2)) _ p =
  inputNumber var (retrieveVar var p `mod` retrieveVar var2 p) p
execute (Mod var (Right i)) _ p = inputNumber var (retrieveVar var p `mod` i) p
execute (Eql var (Left var2)) _ p =
  inputNumber
    var
    (if retrieveVar var p == retrieveVar var2 p
       then 1
       else 0)
    p
execute (Eql var (Right i)) _ p =
  inputNumber
    var
    (if retrieveVar var p == i
       then 1
       else 0)
    p

runInstruction :: (ProgramState, [Int]) -> Instruction -> (ProgramState, [Int])
runInstruction (p, nums) (Input var) =
  (execute (Input var) (safeHead nums) p, safeTail nums)
runInstruction (p, nums) inst = (execute inst Nothing p, nums)

runInstructions :: Instructions -> [Int] -> ProgramState
runInstructions instlist nums =
  fst $ foldl' runInstruction (ProgramState 0 0 0 0, nums) instlist

digs :: Integer -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [fromInteger (x `mod` 10)]

--this doesn't work - too many possibilities
testModelNumber :: Instructions -> Integer -> Integer
testModelNumber instlist number
  | 0 `elem` digs number = testModelNumber instlist (number - 1)
  | z (runInstructions instlist (digs number)) == 0 = number
  | otherwise = testModelNumber instlist (number - 1)

runInstructionsBackwards :: [(ProgramState, [Int])] -> Instructions -> [[Int]]
runInstructionsBackwards pro instructions =
  map snd $
  filter (allZero . fst) $
  foldr
    (\inst acc ->
       (nub . filter (not . tooLarge . snd)) acc >>= executeBackwards inst)
    pro
    instructions

tooLarge :: [Int] -> Bool
tooLarge = any (\x -> x > 9 || x < 1)

allZero :: ProgramState -> Bool
allZero (ProgramState w x y z) = all (== 0) [w, x, y, z]

executeBackwards ::
     Instruction -> (ProgramState, [Int]) -> [(ProgramState, [Int])]
executeBackwards (Input var) (p, is) =
  zip (allStates var p) (repeat $ retrieveVar var p : is)
executeBackwards (Add var (Left var2)) (p, is) =
  [(inputNumber var (retrieveVar var p - retrieveVar var2 p) p, is)]
executeBackwards (Add var (Right i)) (p, is) =
  [(inputNumber var (retrieveVar var p - i) p, is)]
executeBackwards (Mul var (Right 0)) (p, is) =
  case retrieveVar var p of
    0 -> zip (allStates var p) (repeat is)
    _ -> []
executeBackwards (Mul var (Right i)) (p, is) =
  case retrieveVar var p `mod` i of
    0 -> [(inputNumber var (retrieveVar var p `div` i) p, is)]
    _ -> []
executeBackwards (Mul var (Left var2)) (p, is) =
  case retrieveVar var2 p of
    0 ->
      case retrieveVar var p of
        0 -> zip (allStates var p) (repeat is)
        _ -> []
    _ ->
      case retrieveVar var p `mod` retrieveVar var2 p of
        0 ->
          [(inputNumber var (retrieveVar var p `div` retrieveVar var2 p) p, is)]
        _ -> []
executeBackwards (Div var (Left var2)) (p, is) =
  zip (allDivisions var (retrieveVar var2 p) p) (repeat is)
executeBackwards (Div var (Right i)) (p, is) =
  zip (allDivisions var i p) (repeat is)
executeBackwards (Mod var (Left var2)) (p, is) =
  zip (allMod var (retrieveVar var2 p) p) (repeat is)
executeBackwards (Mod var (Right i)) (p, is) = zip (allMod var i p) (repeat is)
executeBackwards (Eql var (Right i)) (p, is) =
  if retrieveVar var p == 1
    then [(inputNumber var i p, is)]
    else zip (allNonEql var i p) (repeat is)
executeBackwards (Eql var (Left var2)) (p, is) =
  if retrieveVar var p == 1
    then [(inputNumber var (retrieveVar var2 p) p, is)]
    else zip (allNonEql var (retrieveVar var2 p) p) (repeat is)

allStates :: Variable -> ProgramState -> [ProgramState]
allStates var p = map (flip (inputNumber var) p) ([0 ..] ++ map ((-1) *) [1 ..])

allDivisions :: Variable -> Int -> ProgramState -> [ProgramState]
allDivisions var i p =
  map (flip (inputNumber var) p . (retrieveVar var p *)) [i .. (2 * i - 1)]

allNonEql :: Variable -> Int -> ProgramState -> [ProgramState]
allNonEql var i p =
  map (flip (inputNumber var) p) (filter (/= i) ([0 ..] ++ map ((-1) *) [1 ..]))

allMod :: Variable -> Int -> ProgramState -> [ProgramState]
allMod var i p =
  map
    (flip (inputNumber var) p)
    [x | x <- [0 ..], x `mod` i == retrieveVar var p]
