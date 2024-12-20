module Advent.Day20 where

import Advent.Prelude
import Data.Map (Map)
import Data.Map qualified as Map
import Data.PSQueue (PSQ, Binding((:->)))
import Data.PSQueue qualified as PSQ
import Data.Set (Set)
import Data.Set qualified as Set

type Coord  = (Int, Int)
type Bounds = Coord
type End    = Coord
type Wall   = Coord
type Walls  = Set Wall
type Cost   = Int

day20 :: (Walls, Bounds, End) -> IO ()
day20 (walls, bounds, end) = do
  let cheatsThatSave n gen = jumps gen n bounds walls $ dijkstra
        (findNeighbours bounds walls) Map.empty $ PSQ.singleton end 0
  print $ length $ cheatsThatSave 100 cheatsPartA
  print $ length $ cheatsThatSave 100 $ cheatsPartB 20

cheatsPartA :: Coord -> [(Coord, Int)]
cheatsPartA (i, j) = (,2) <$> [(i-2, j), (i+2, j), (i, j-2), (i, j+2)]

cheatsPartB :: Int -> Coord -> [(Coord, Int)]
cheatsPartB maxCheat (i, j) =
  [ ((i + dI, j + dJ), (abs dI + abs dJ))
  | dI <- [-maxCheat .. maxCheat], let djAbs = maxCheat - abs dI
  , dJ <- [-djAbs .. djAbs]
  ]

jumps
  :: (Coord -> [(Coord, Int)]) -> Int -> Bounds -> Walls -> Map Coord Cost
  -> [(Coord, Coord, Int)]
jumps genCheats expectedSavings bounds@(maxI, maxJ) walls fromE = do
  [ ((i, j), kl, savings)
    | i <- [0..maxI]
    , j <- [0..maxJ]
    , inBounds bounds (i, j)
    , (i, j) `Set.notMember` walls
    , (kl, cheatTime) <- genCheats (i, j)
    , inBounds bounds kl
    , kl `Set.notMember` walls
    , (Just savings) <- [cheatSavings (i, j) kl cheatTime]
    , savings >= expectedSavings
    ]
 where
  cheatSavings ij kl cheatTime = do
    c1 <- Map.lookup ij fromE
    c2 <- Map.lookup kl fromE
    pure $ c1 - c2 - cheatTime

-- * Movement.

findNeighbours :: Bounds -> Walls -> Coord -> [(Coord, Cost)]
findNeighbours bounds walls (i, j) =
  flip mapMaybe [(i-1, j), (i+1, j), (i, j+1), (i, j-1)] \(k, l)-> do
    if   inBounds bounds (k, l) && (k, l) `Set.notMember` walls
    then Just ((k, l), 1)
    else Nothing

inBounds :: Bounds -> Coord -> Bool
inBounds (maxI, maxJ) (i, j) = i >= 0 && j >= 0 && i <= maxI && j <= maxJ

-- * Dijkstra.

dijkstra :: forall a. Ord a =>
  (a -> [(a, Cost)]) -> Map a Cost -> PSQ a Cost -> Map a Cost
dijkstra findNeighbours' visited reachable =
  -- Find the minimum-cost reachable node.
  case PSQ.findMin reachable of
    Nothing                 -> visited -- No more reachable nodes.
    Just (minA :-> minCost) -> do
      -- Move this node from reachable set to visited set.
      let visited'   = Map.insert minA minCost visited
      let reachable' = PSQ.delete minA reachable
      -- Update the reachable set with cost of neighbours through this node.
      let neighbours      = map (second (+ minCost)) $ flip filter
            (findNeighbours' minA) $ (`Map.notMember` visited) . fst
      let insertNeighbour = uncurry $ PSQ.insertWith min
      let reachable''     = foldl' (flip insertNeighbour) reachable' neighbours
      dijkstra findNeighbours' visited' reachable''

-- * IO.

main :: IO ()
main = readInput "data/Day20.txt" >>= day20

readInput :: String -> IO (Walls, Bounds, End)
readInput = fmap (parse . lines) . readFile
 where
  flat rows = [ ((i, j), c) | (i, row) <- enumerate rows, (j, c) <- enumerate row ]
  bounds          = fst . last . flat
  end             = fst . head . filter ((== 'E') . snd) . flat
  parse      rows = (parseWalls rows, bounds rows, end rows)
  parseWalls rows = Set.fromList [ ij | (ij, c) <- flat rows, c == '#' ]
