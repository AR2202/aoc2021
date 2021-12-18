module Day16
  ( day16a
  , day16b
  ) where

import           Common

------------
-- Types
------------
type Version = Int

type ID = Int

data PackageType
  = Lit
  | Op
  deriving (Show, Read, Eq)

data LengthTypeID
  = Zero
  | One
  deriving (Show, Read, Eq)

data Package
  = Literal Version Int
  | Operator Version ID LengthTypeID [Package]
  deriving (Show, Read, Eq)

----------
-- Part 1
----------
day16a :: IO ()
day16a = readDay16 "../input/day16.txt"

-----------
-- Part 2
-----------
day16b :: IO ()
day16b = readDay16b "../input/day16.txt"

-------------
-- read Input
-------------
readDay16 :: String -> IO ()
readDay16 filename = do
  hex <- loadInput filename
  let bin = concatMap hexStr2BinStr hex
  let packages = fst $ parsePackage bin
  let versions = packageVersion packages
  print versions

readDay16b :: String -> IO ()
readDay16b filename = do
  hex <- loadInput filename
  let bin = concatMap hexStr2BinStr hex
  let packages = fst $ parsePackage bin
  let number = packageNumber packages
  print number

hexStr2BinStr :: Char -> String
hexStr2BinStr '0' = "0000"
hexStr2BinStr '1' = "0001"
hexStr2BinStr '2' = "0010"
hexStr2BinStr '3' = "0011"
hexStr2BinStr '4' = "0100"
hexStr2BinStr '5' = "0101"
hexStr2BinStr '6' = "0110"
hexStr2BinStr '7' = "0111"
hexStr2BinStr '8' = "1000"
hexStr2BinStr '9' = "1001"
hexStr2BinStr 'A' = "1010"
hexStr2BinStr 'B' = "1011"
hexStr2BinStr 'C' = "1100"
hexStr2BinStr 'D' = "1101"
hexStr2BinStr 'E' = "1110"
hexStr2BinStr 'F' = "1111"

bin2Version str = string2decimal $ take 3 str

packageType "100" = Lit
packageType _     = Op

whichPackageType = packageType . take 3 . drop 3

lengthTypeID '0' = Zero
lengthTypeID '1' = One

operatorId = string2decimal . take 3 . drop 3

parsePackage :: String -> (Package, String)
parsePackage binStr =
  case whichPackageType binStr of
    Lit ->
      ( Literal (bin2Version binStr) (fst $ parseLiteralNumber $ drop 6 binStr)
      , snd $ parseLiteralNumber $ drop 6 binStr)
    Op ->
      ( Operator
          (bin2Version binStr)
          (operatorId binStr)
          lID
          (parseSub lID binStr)
      , reststr lID binStr)
  where
    lID = (lengthTypeID . head . drop 6) binStr
    parseSub Zero s = parseByLength [] $ take (lengthSub s) $ drop 22 s
    parseSub One s  = fst $ parsePackages (drop 18 s) (numSub s) []
    reststr One s  = snd $ parsePackages (drop 18 s) (numSub s) []
    reststr Zero s = drop (22 + lengthSub s) s

parseLiteralNumber s = go s ""
  where
    go s n
      | head s == '0' = (string2decimal $ n ++ (tail . take 5) s, drop 5 s)
      | head s == '1' = go (drop 5 s) (n ++ (tail . take 5) s)

lengthSub = string2decimal . take 15 . drop 7

numSub = string2decimal . take 11 . drop 7

parsePackages binstr 0 packagelist = (packagelist, binstr)
parsePackages binstr n packagelist =
  parsePackages reststr (n - 1) (package : packagelist)
  where
    package = fst $ parseOne binstr
    reststr = snd $ parseOne binstr

parseOne = parsePackage

parseByLength packagelist "" = packagelist
parseByLength packagelist s = parseByLength (package : packagelist) reststr
  where
    package = fst $ parseOne s
    reststr = snd $ parseOne s

packageVersion :: Package -> Version
packageVersion (Literal v i)           = v
packageVersion (Operator v id ltid ps) = v + sum (map packageVersion ps)

packageSum :: Package -> Int
packageSum (Literal v i)           = i
packageSum (Operator v id ltid ps) = sum $ map packageSum ps

packageProduct :: Package -> Int
packageProduct (Literal v i)           = i
packageProduct (Operator v id ltid ps) = product $ map packageProduct ps

packageNumber :: Package -> Int
packageNumber (Literal v i) = i
packageNumber (Operator v 0 ltid ps) = sum $ map packageNumber ps
packageNumber (Operator v 1 ltid ps) = product $ map packageNumber ps
packageNumber (Operator v 2 ltid ps) = minimum $ map packageNumber ps
packageNumber (Operator v 3 ltid ps) = maximum $ map packageNumber ps
packageNumber (Operator v 5 ltid ps)
  | firstpackage > secondpackage = 1
  | otherwise = 0
  where
    firstpackage = (map packageNumber ps) !! 1
    secondpackage = (map packageNumber ps) !! 0
packageNumber (Operator v 6 ltid ps)
  | firstpackage < secondpackage = 1
  | otherwise = 0
  where
    firstpackage = (map packageNumber ps) !! 1
    secondpackage = (map packageNumber ps) !! 0
packageNumber (Operator v 7 ltid ps)
  | firstpackage == secondpackage = 1
  | otherwise = 0
  where
    firstpackage = (map packageNumber ps) !! 1
    secondpackage = (map packageNumber ps) !! 0
