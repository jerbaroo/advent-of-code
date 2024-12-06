module Advent.Day6 where

import Prelude hiding (Left, Right)

import Advent.Prelude
import Data.List (find, nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

data Orientation = Up | Down | Left | Right deriving (Eq, Ord, Show)
type Position    = (Int, Int)
type Guard       = (Orientation, Position)
data StopReason  = Loop | OffMap deriving Eq

type Crates'     = Map Int (Set Int)
-- | We maintain two representations of crates for fast lookup of all the crates
-- in one row/column, one i indexed first, the other j indexed first.
type Crates      = (Crates', Crates')

insertCrate :: Position -> Crates -> Crates
insertCrate (i, j) (iFirst, jFirst) =
  ( Map.insertWith Set.union i (Set.singleton j) iFirst
  , Map.insertWith Set.union j (Set.singleton i) jFirst
  )

isCrate :: Position -> Crates -> Bool
isCrate (i, j) (m, _) = (Map.lookup i m <&> Set.member j) == Just True

main :: IO ()
main = print =<< (readMap <&> (uncurry3 part1 &&& uncurry3 part2))

-- | Count distinct positions guard will visit.
part1 :: Position -> Crates -> Guard -> Int
part1 maxIndices crates =
  Set.size . Set.fromList . map snd . snd . patrol False maxIndices [] crates

-- | Find positions without loops.
part2 :: Position -> Crates -> Guard -> Int
part2 maxIndices crates guard' = do
  let originalPath = filter (/= snd guard') $ -- Without initial position.
        nub $ map snd $ snd $ patrol False maxIndices [] crates guard'
  length $ filter id $ originalPath <&> isLoop
 where
  isLoop newCrate = (== Loop) . fst $
    patrol True maxIndices [] (insertCrate newCrate crates) guard'

-- | Patrol until either off the map or a loop detected.
patrol :: Bool -> Position -> [Guard] -> Crates -> Guard -> (StopReason, [Guard])
patrol fast maxIndices prevPath crates guard'@(ori, _) = do
  let path    = guard' : prevPath
  let nextPos = nextPosition fast maxIndices crates guard'
  if   outOfBounds maxIndices nextPos then (OffMap, path)
  else do
    let nextGuard = avoidCrate crates (ori, nextPos)
    if   nextGuard `elem` prevPath then (Loop, prevPath)
    else patrol fast maxIndices path crates nextGuard

avoidCrate :: Crates -> Guard -> Guard
avoidCrate crates (ori, pos) =
  if isCrate pos crates then (turnRight ori, stepBack (ori, pos)) else (ori, pos)

outOfBounds :: Position -> Position -> Bool
outOfBounds (maxI, maxJ) (i, j) = i < 0 || j < 0 || i > maxI || j > maxJ

nextPosition :: Bool -> Position -> Crates -> Guard -> Position
nextPosition fast maxIndices crates =
  if fast then stepForwardFast maxIndices crates else stepForward

stepBack :: Guard -> Position
stepBack (Up   , (i, j)) = (i+1, j  )
stepBack (Down , (i, j)) = (i-1, j  )
stepBack (Left , (i, j)) = (i  , j+1)
stepBack (Right, (i, j)) = (i  , j-1)

stepForward :: Guard -> Position
stepForward (Up   , (i, j)) = (i-1, j  )
stepForward (Down , (i, j)) = (i+1, j  )
stepForward (Left , (i, j)) = (i  , j-1)
stepForward (Right, (i, j)) = (i  , j+1)

stepForwardFast :: Position -> Crates -> Guard -> Position
stepForwardFast (maxI, maxJ) (iFirst, jFirst)  (ori, (i, j)) = f ori
 where
  f Up    = upDown (-1)          Set.lookupLT
  f Down  = upDown (maxI + 1)    Set.lookupGT
  f Left  = leftRight (-1)       Set.lookupLT
  f Right = leftRight (maxJ + 1) Set.lookupGT
  leftRight def lookup' = (i,) $ fromMaybe def $ lookup' j =<< Map.lookup i iFirst
  upDown    def lookup' = (,j) $ fromMaybe def $ lookup' i =<< Map.lookup j jFirst

turnRight :: Orientation -> Orientation
turnRight Up    = Right
turnRight Down  = Left
turnRight Left  = Up
turnRight Right = Down

-- | Read orientation and position of guard, and positions of crates
readMap :: IO (Position, Crates, Guard)
readMap = readFile "data/Day6.txt" <&> \string -> do
  let rows = lines string
  let cells = concat [ zip ((i,) <$> [0 .. ]) row | (i, row) <- zip [0 .. ] rows ]
  let crateCells = map fst $ filter ((== '#') . snd) cells
  let (guardPos, guardChar) = fromMaybe (error "Can't find guard") $
        find (\(_, c) -> c /= '.' && c /= '#') cells
  let guardOrientation = case guardChar of
        '^' -> Up; '>' -> Right; 'v' -> Down; '<' -> Left;
        c   -> error $ "Player orientation not recognised: '" <> [c] <> "'"
  let crates f g = foldl' (\m x -> Map.insertWith Set.union (f x) (Set.singleton $ g x) m) Map.empty crateCells
  ( ( length rows - 1, length (head rows) - 1 )
    , ( crates fst snd, crates snd fst )
    , (guardOrientation, guardPos)
    )
