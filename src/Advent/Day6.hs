module Advent.Day6 where

import Prelude hiding (Left, Right)

import Advent.Prelude
import Data.List (find)
import Data.Set (Set)
import Data.Set qualified as Set

data Orientation = Up | Down | Left | Right deriving (Eq, Ord, Show)
type Position    = (Int, Int)
type Guard       = (Orientation, Position)
type Crates      = Set Position
data StopReason  = Loop | OffMap deriving Eq

main :: IO ()
main = print =<< (readMap <&> (uncurry4 part1 &&& uncurry4 part2))

-- | Count distinct positions guard will visit.
part1 :: Int -> Int -> Crates -> Guard -> Int
part1 maxI maxJ crates =
  Set.size . Set.map snd . snd . walk maxI maxJ Set.empty crates

-- | Find positions without loops.
part2 :: Int -> Int -> Crates -> Guard -> Int
part2 maxI maxJ crates guard' = do
  let path = Set.toList $ Set.filter (/= snd guard') $
        Set.map snd $ snd $ walk maxI maxJ Set.empty crates guard'
  length $ filter id $ path <&> \newCrate ->
    isLoop maxI maxJ (Set.insert newCrate crates) guard'

isLoop :: Int -> Int -> Crates -> Guard -> Bool
isLoop maxI maxJ crates = (== Loop) . fst . walk maxI maxJ Set.empty crates

-- | Walk until wither off the map or a loop detected.
walk :: Int -> Int -> Set Guard -> Crates -> Guard -> (StopReason, Set Guard)
walk maxI maxJ seen crates guard'@(ori, pos) =
  case forwardMay maxI maxJ guard' of
    Nothing         -> (OffMap, Set.insert guard' seen) -- Walked off the map.
    Just forwardPos -> do                               -- Still on the map.
      let crateBlocking = forwardPos `Set.member` crates
      let nextGuard =
            if   crateBlocking
            then (turnRight ori, pos       ) -- Either change orientation..
            else (ori          , forwardPos) -- ..or move forward.
      if   nextGuard `elem` seen && not crateBlocking
      then (Loop, seen)                                             -- Loop!
      else walk maxI maxJ (Set.insert guard' seen) crates nextGuard -- No loop.

turnRight :: Orientation -> Orientation
turnRight Up    = Right
turnRight Down  = Left
turnRight Left  = Up
turnRight Right = Down

forwardMay :: Int -> Int -> Guard -> Maybe Position
forwardMay maxI maxJ guard' =
  let pos@(i, j) = forward guard'
  in  if i < 0 || j < 0 || i > maxI || j > maxJ then Nothing else Just pos

forward :: Guard -> Position
forward (Up   , (i, j)) = (i-1, j  )
forward (Down , (i, j)) = (i+1, j  )
forward (Left , (i, j)) = (i  , j-1)
forward (Right, (i, j)) = (i  , j+1)

-- | Read orientation and position of guard, and positions of crates
readMap :: IO (Int, Int, Crates, Guard)
readMap = readFile "data/Day6.txt" <&> \string -> do
  let lines' = lines string
  let cells  :: [(Position, Char)] = concat
        [ [ ((i, j), char) | (j, char) <- zip [0 .. ] line ]
        | (i, line) <- zip [0 .. ] lines'
        ]
  let guardCell = fromMaybe (error "Can't find guard") $
        find (\(_, c) -> c /= '.' && c /= '#') cells
  let guardOrientation =
        case snd guardCell of
          '^' -> Up
          '>' -> Right
          'v' -> Down
          '<' -> Left
          c   -> error $ "Player orientation not recognised: '" <> [c] <> "'"
  (   length lines' - 1
    , length (head lines') - 1
    , Set.fromList [ fst cell | cell <- cells, snd cell == '#' ]
    , (guardOrientation, fst guardCell)
    )
