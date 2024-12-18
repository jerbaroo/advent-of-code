module Advent.Day18 where

import Advent.Prelude
import Data.List.Extra (splitOn, intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.PSQueue (PSQ, Binding((:->)))
import Data.PSQueue qualified as PSQ

type Coord   = (Int, Int)
type Falling = [Coord]
type Fell    = Set Coord
data NSEW    = N | S | E | W deriving Show

-- | Cost of a node, having travelled through edge e.
newtype Cost e = Cost (Int, e) deriving Show

instance Eq (Cost e) where
  (Cost (a, _)) == (Cost (b, _)) = a == b

instance Ord (Cost e) where
  (Cost (a, _)) `compare` (Cost (b, _)) = a `compare` b

infixl 5 +$

(+$) :: Cost e -> Int -> Cost e
(+$) (Cost (c, e)) x = Cost (c + x, e)

cost :: Cost e -> Int
cost (Cost (c, _)) = c

dijkstra :: forall a e. Ord a =>
  (a -> [(a, Cost e)]) -> Map a (Cost e) -> PSQ a (Cost e) -> Map a (Cost e)
dijkstra findNeighbours' visited reachable =
  -- Find the minimum-cost reachable node.
  case PSQ.findMin reachable of
    Nothing                 -> visited -- No more reachable nodes.
    Just (minA :-> minCost) -> do
      -- Move this node from reachable set to visited set.
      let visited'   = Map.insert minA minCost visited
      let reachable' = PSQ.delete minA reachable
      -- Update the reachable set with cost of neighbours through this node.
      let neighbours      = map (second (+$ cost minCost)) $ flip filter
            (findNeighbours' minA) $ (`Map.notMember` visited) . fst
      let insertNeighbour = uncurry $ PSQ.insertWith min
      let reachable''     = foldl' (flip insertNeighbour) reachable' neighbours
      dijkstra findNeighbours' visited' reachable''

isReachable :: (Coord -> [Coord]) -> Coord -> Set Coord -> PSQ Coord Int -> Bool
isReachable findNeighbours' goal visited toVisit = do
  case PSQ.findMin toVisit of
    Nothing                    -> False
    Just (v :-> _) | v == goal -> True
    Just (v :-> _)             -> do
      let visited' = Set.insert v visited
      let toVisit' = PSQ.delete v toVisit
      isReachable findNeighbours' goal visited'
        $ foldl' (\p c -> PSQ.insert c (distance c goal) p) toVisit'
        $ filter (`Set.notMember` visited)
        $ findNeighbours' v

distance :: Num a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

part2 :: Int -> Coord -> Fell -> Falling -> Coord
part2 maxXY goal fell falling = do
  -- Let 1 block fall.
  let (fell', falling') = fallN 1 fell falling
  let byteFell          = head falling
  let start = (0, 0)
  -- Check if
  let reachable = isReachable
          (map fst <$> flip (findNeighbours maxXY) fell')
          goal
          Set.empty
          $ PSQ.singleton start (distance start goal)
  if reachable then part2 maxXY goal fell' falling' else byteFell

fallN :: Int -> Fell -> Falling -> (Fell, Falling)
fallN 0 fell falling = (fell, falling)
fallN _ fell []      = (fell, [])
fallN n fell (f:fs)  = fallN (n-1) (Set.insert f fell) fs

fallAll :: Fell -> Falling -> Fell
fallAll fell = fst . fallN (-1) fell

findNeighbours :: Int -> Coord -> Fell -> [(Coord, Cost NSEW)]
findNeighbours maxXY xy fell = flip mapMaybe [N, S, E, W] \nsew -> do
  let xy'@(x, y) = step xy nsew
  let outOfBounds = x < 0 || y < 0 || x > maxXY || y > maxXY
  if   xy' `Set.member` fell || outOfBounds
  then Nothing
  else Just (xy', Cost (1, nsew))

step :: Coord -> NSEW -> Coord
step (x, y) N = (x    , y - 1)
step (x, y) S = (x    , y + 1)
step (x, y) E = (x - 1, y    )
step (x, y) W = (x + 1, y    )

-- * IO.

main :: IO ()
main = readFalling "data/Day18.txt" >>= \falling -> do
  let maxXY = 70
  let run   = 1024
  let fell = fst $ fallN run Set.empty falling
  do
    let solved = dijkstra
          (flip (findNeighbours maxXY) fell)
          Map.empty
          (PSQ.singleton (0, 0) (Cost (0, N)))
    print "\nRemoved (6, 1):"
    print $ Map.lookup (maxXY, maxXY) solved
    print $ part2 maxXY (maxXY, maxXY) Set.empty falling

readFalling :: String -> IO Falling
readFalling = fmap parse . readFile
 where
  parse = map (toTuple . map read . splitOn ",") . lines
  toTuple [a, b] = (a, b)
  toTuple _      = error "That aint a tuple"

showMemory :: Int -> Fell -> String
showMemory n fell =
  let f ij = if ij `Set.member` fell then '#' else '.'
  in  intercalate "\n" [ [ f (x, y) | x <- [0..n] ] | y <- [0..n] ]
