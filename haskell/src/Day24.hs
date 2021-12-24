module Day24
  ( ProgramState(..)
  , Variable(..)
  , Instruction(..)
  , execute
  , runInstruction
  , runInstructions
  , readMaybeInt
  , readDay24
  ) where

import           Common
import           Data.Char  (toUpper)
import           Data.List  (foldl')
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

------------
-- Part 1
------------
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
    (if (retrieveVar var p) == (retrieveVar var2 p)
       then 1
       else 0)
    p
execute (Eql var (Right i)) _ p =
  inputNumber
    var
    (if (retrieveVar var p) == i
       then 1
       else 0)
    p

inputNumber W i p = p {w = i}
inputNumber X i p = p {x = i}
inputNumber Y i p = p {y = i}
inputNumber Z i p = p {z = i}

retrieveVar W p = w p
retrieveVar X p = x p
retrieveVar Y p = y p
retrieveVar Z p = z p

safeHead []     = Nothing
safeHead (x:xs) = Just x

safeTail [] = []
safeTail l  = tail l

runInstruction :: (ProgramState, [Int]) -> Instruction -> (ProgramState, [Int])
runInstruction (p, nums) (Input var) =
  (execute (Input var) (safeHead nums) p, safeTail nums)
runInstruction (p, nums) inst = (execute inst Nothing p, nums)

runInstructions :: Instructions -> [Int] -> ProgramState
runInstructions instlist nums =
  fst $ foldl' runInstruction (ProgramState 0 0 0 0, nums) instlist

digs :: Integer -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ ([fromInteger (x `mod` 10)])

--this doesn't work - too many possibilities
testModelNumber instlist number
  | 0 `elem` digs number = testModelNumber instlist (number - 1)
  | z (runInstructions instlist (digs number)) == 0 = number
  | otherwise = testModelNumber instlist (number - 1)

--------------------
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

splitInst = map words . lines

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

readDay24 filename = do
  s <- loadInput filename
  let inst = parseInstructions s
  let modelnumber = testModelNumber inst 99999999999999
  print modelnumber
