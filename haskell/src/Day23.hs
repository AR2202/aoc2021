module Day23
  ( example23
  , canMove
  , canMoveTo
  , isCorrect
  , allMovesOne
  , allMovesAll
  , allNewStates
  , runUntilCorrect
  , steps
  , energy
  , example23energy
  , testInsert
  , newStatesToMap
  , testMap
  , day23Map
  ) where

import           Common
import           Control.Applicative (liftA2)
import           Data.List           (foldl', minimumBy, nub)
import qualified Data.Map            as M
import           Data.Ord            (comparing)
import qualified Data.Vector         as V

data Amphipod
  = A
  | B
  | C
  | D
  deriving (Show, Read, Eq, Ord)

type Energy = Int

example23 =
  V.fromList
    [ Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Just B
    , Just C
    , Just B
    , Just D
    , Just A
    , Just D
    , Just C
    , Just A
    ]

day23 =
  V.fromList
    [ Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Just C
    , Just B
    , Just D
    , Just D
    , Just B
    , Just C
    , Just A
    , Just A
    ]

example23energy :: (V.Vector (Maybe Amphipod), Maybe Energy)
example23energy = (example23, Just 0)

day23energy :: (V.Vector (Maybe Amphipod), Maybe Energy)
day23energy = (day23, Just 0)

day23Map :: M.Map (V.Vector (Maybe Amphipod)) (Maybe Energy)
day23Map = M.fromList [day23energy]

canMove v ix1 ix2
  | isNothing v ix1 = False
  | isCorrect v ix1 = False
  | inRoomTop ix1 && not (inRoom ix2) =
    all (isNothing v) [(minimum [ix1 - 4, ix2]) .. (maximum [ix1 - 5, ix2])]
  | inRoomTop ix1 && inRoomTop ix2 =
    all
      (isNothing v)
      [(minimum [ix1 - 4, ix2 - 4]) .. (maximum [ix1 - 5, ix2 - 5])] &&
    isNothing v ix2 && v V.! (ix2 + 4) == v V.! ix1
  | not (inRoom ix1) && inRoomTop ix2 =
    all
      (isNothing v)
      (ix2 : [(minimum [ix1, ix2 - 4]) .. (maximum [ix1, ix2 - 5])]) &&
    v V.! (ix2 + 4) == v V.! ix1 && isCorrect (v V.// [(ix2, v V.! ix1)]) ix2
  | inRoomBottom ix1 && not (inRoom ix2) =
    all
      (isNothing v)
      ((ix1 - 4) : [(minimum [ix1 - 8, ix2]) .. (maximum [ix1 - 9, ix2])])
  | not (inRoom ix1) && inRoomBottom ix2 =
    all
      (isNothing v)
      (ix2 : (ix2 - 4) : [(minimum [ix1, ix2 - 8]) .. (maximum [ix1, ix2 - 9])]) &&
    isCorrect (v V.// [(ix2, v V.! ix1)]) ix2
  | inRoomTop ix1 && inRoomBottom ix2 =
    all
      (isNothing v)
      (ix2 :
       (ix2 - 4) :
       [(minimum [ix1 - 4, ix2 - 5]) .. (maximum [ix1 - 5, ix2 - 9])]) &&
    isCorrect (v V.// [(ix2, v V.! ix1)]) ix2
  | inRoomBottom ix1 && inRoomTop ix2 =
    all
      (isNothing v)
      ((ix1 - 4) :
       ix2 : [(minimum [ix1 - 8, ix2 - 4]) .. (maximum [ix1 - 9, ix2 - 5])]) &&
    v V.! (ix2 + 4) == v V.! ix1 && isCorrect (v V.// [(ix2, v V.! ix1)]) ix2
  | otherwise = False

isNothing :: V.Vector (Maybe Amphipod) -> Int -> Bool
isNothing v i = v V.! i == Nothing

inRoom ix = ix `elem` [7 .. 14]

inRoomTop ix = ix `elem` [7 .. 10]

inRoomBottom ix = ix `elem` [11 .. 14]

inRoom1 ix = ix `elem` [7, 11]

inRoom2 ix = ix `elem` [8, 12]

inRoom3 ix = ix `elem` [9, 13]

inRoom4 ix = ix `elem` [10, 14]

canMoveTo :: V.Vector (Maybe Amphipod) -> Int -> [Int]
canMoveTo v ix = filter (canMove v ix) [0 .. 14]

allMovesOne ::
     (V.Vector (Maybe Amphipod), Maybe Energy)
  -> Int
  -> [(V.Vector (Maybe Amphipod), Maybe Energy)]
allMovesOne (v, e) ix =
  map
    (\i -> (v V.// [(i, v V.! ix), (ix, Nothing)], (+) <$> e <*> energy v ix i)) $
  canMoveTo v ix

allMovesAll ::
     (V.Vector (Maybe Amphipod), Maybe Energy)
  -> [(V.Vector (Maybe Amphipod), Maybe Energy)]
allMovesAll (v, e) =
  concatMap (allMovesOne (v, e)) $ filter (not . isCorrect v) [0 .. 14]

allNewStates ::
     [(V.Vector (Maybe Amphipod), Maybe Energy)]
  -> [(V.Vector (Maybe Amphipod), Maybe Energy)]
allNewStates oldstates = oldstates >>= allMovesAll

newStatesToMap ::
     M.Map (V.Vector (Maybe Amphipod)) (Maybe Energy)
  -> M.Map (V.Vector (Maybe Amphipod)) (Maybe Energy)
newStatesToMap m = foldl' insertEnergy m $ allNewStates $ M.toList m

runUntilCorrect ::
     M.Map (V.Vector (Maybe Amphipod)) (Maybe Energy)
  -> Maybe (V.Vector (Maybe Amphipod), Maybe Energy)
runUntilCorrect oldMap
  | null notExceedingMinEnergy = minEnergyCorrect
  | otherwise = runUntilCorrect $ newStatesToMap validStates
  where
    validStates =
      case minEnergyCorrect of
        Nothing     -> oldMap
        Just minEnC -> M.fromList (minEnC : notExceedingMinEnergy)
    minEnergyCorrect = minimumByEnergy $ M.toList $ fst $ splitOnCorrect oldMap
    notExceedingMinEnergy =
      filter (lowerEnergy minEnergyCorrect) $
      M.toList $ snd $ splitOnCorrect oldMap

allCorrect :: (V.Vector (Maybe Amphipod), Maybe Energy) -> Bool
allCorrect (v, e) = all (isCorrect v) [0 .. 14]

allCorrect' :: V.Vector (Maybe Amphipod) -> Bool
allCorrect' v = all (isCorrect v) [0 .. 14]

isCorrect :: V.Vector (Maybe Amphipod) -> Int -> Bool
isCorrect v ix =
  case v V.! ix of
    Nothing -> not (inRoom ix)
    Just A  -> ix == 11 || (ix == 7 && v V.! 11 == Just A)
    Just B  -> ix == 12 || (ix == 8 && v V.! 12 == Just B)
    Just C  -> ix == 13 || (ix == 9 && v V.! 13 == Just C)
    Just D  -> ix == 14 || (ix == 10 && v V.! 14 == Just D)

multiplier :: Amphipod -> Int
multiplier A = 1
multiplier B = 10
multiplier C = 100
multiplier D = 1000

steps :: Int -> Int -> Int
steps ix1 0 = 1 + steps ix1 1
steps ix1 6 = 1 + steps ix1 5
steps ix1 ix2
  | inRoomBottom ix1 = 1 + steps (ix1 - 4) ix2
  | inRoomTop ix1 && inRoomTop ix2 = 2 + 2 * abs (ix1 - ix2)
  | inRoomTop ix1 && not (inRoom ix2) && ix1 - ix2 `elem` [5, 6] = 2
  | inRoomTop ix1 && not (inRoom ix2) && ix1 - ix2 `elem` [4, 7] = 4
  | inRoomTop ix1 && not (inRoom ix2) && ix1 - ix2 `elem` [3, 8] = 6
  | inRoomTop ix1 && not (inRoom ix2) && ix1 - ix2 `elem` [2, 9] = 8
  | otherwise = steps ix2 ix1

energy :: V.Vector (Maybe Amphipod) -> Int -> Int -> Maybe Energy
energy v ix1 ix2 = fmap ((*) (steps ix1 ix2) . multiplier) $ v V.! ix1

testMap :: M.Map (V.Vector (Maybe Amphipod)) (Maybe Energy)
testMap = M.fromList [example23energy]

testInsert = M.insertWith (liftA2 smaller) example23 (Just 5) testMap

insertEnergy ::
     M.Map (V.Vector (Maybe Amphipod)) (Maybe Energy)
  -> (V.Vector (Maybe Amphipod), Maybe Energy)
  -> M.Map (V.Vector (Maybe Amphipod)) (Maybe Energy)
insertEnergy m (v, e) = M.insertWith (liftA2 smaller) v e m

smaller :: Int -> Int -> Int
smaller x y = minimum [x, y]

splitOnCorrect ::
     M.Map (V.Vector (Maybe Amphipod)) (Maybe Energy)
  -> ( M.Map (V.Vector (Maybe Amphipod)) (Maybe Energy)
     , M.Map (V.Vector (Maybe Amphipod)) (Maybe Energy))
splitOnCorrect = M.partitionWithKey (\k _ -> allCorrect' k)

minimumByEnergy ::
     [(V.Vector (Maybe Amphipod), (Maybe Energy))]
  -> Maybe (V.Vector (Maybe Amphipod), (Maybe Energy))
minimumByEnergy [] = Nothing
minimumByEnergy xs = Just . minimumBy (comparing snd) $ xs

lowerEnergy ::
     Maybe (V.Vector (Maybe Amphipod), (Maybe Energy))
  -> (V.Vector (Maybe Amphipod), (Maybe Energy))
  -> Bool
lowerEnergy Nothing _              = True
lowerEnergy (Just (_, e1)) (_, e2) = e2 < e1
