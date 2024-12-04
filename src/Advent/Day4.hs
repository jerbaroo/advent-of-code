module Advent.Day4 where

import Advent.Prelude
import Debug.Trace (trace)
import Control.Concurrent.Async (forConcurrently)
import Data.Vector qualified as V
import Data.Vector (Vector, (!?))

bug :: Show a => a -> a
bug a = trace (show a) a

main :: IO ()
main = do
  grid <- readGrid
  print =<< searchGridConcurrently part1 "XMAS" grid
  -- print =<< searchGridConcurrently part2 "A" grid

readGrid :: IO (Grid Char)
readGrid = readFile "data/Day4.txt" <&> V.fromList . map V.fromList . lines

type Grid a    = Vector (Vector a)
type Index     = (Int, Int)
data Direction = N | NE | E | SE | S | SW | W | NW deriving Show

-- | Search strategy to allow generalizing code to both parts 1 and 2.
data Strat a b = Strat
  { cells :: [a] -> Index -> Maybe Direction -> [([a], (Index, Maybe Direction))]
  , count :: [b] -> b
  , match :: a -> [a] -> Maybe b
  , noMatch :: b
  , stop :: [a] -> Bool
  }

-- | Search strategy for part 1.
part1 :: Eq a => Strat a Int
part1 = Strat
  { cells = \query index dir ->
      let steps (Just dir') = [second Just $ step index dir']
          steps Nothing = concatMap (steps . Just) [N, NE, E, SE, S, SW, W, NW]
      in  (tail query,) <$> steps dir
  , count = sum
  , match = \c query -> if headMay query == Just c then Just 1 else Nothing
  , noMatch = 0
  , stop = (<= 1) . length
  }

-- | Perform an outward search from each cell concurrently.
searchGridConcurrently:: (Eq a, Show a) => Strat a b -> [a] -> Grid a -> IO b
searchGridConcurrently strat query grid = do
  let maxI = V.length grid
  let maxJ = maybe 0 V.length (grid !? 0)
  fmap strat.count $ forConcurrently
    [ (i, j) | i <- [0 .. maxI], j <- [0 .. maxJ] ]
    $ pure . searchFromCell strat grid query . (,Nothing)

-- | Perform an outward search from one cell.
searchFromCell :: (Eq a, Show a) => Strat a b -> Grid a -> [a] -> (Index, Maybe Direction) -> b
searchFromCell start _ [] _ = start.noMatch
searchFromCell strat grid query ((i, j), dir) =
  case trace ("Cell " <> show i <> ", " <> show j <> " dir " <> show dir <> " " <> show query) $ grid !? i >>= (!? j) of
    Nothing -> strat.noMatch -- Index out of bounds.
    Just c  -> case strat.match (trace ("Cell " <> show i <> ", " <> show j <> ": " <> show c) c) query of
      Nothing    -> trace "no match" strat.noMatch -- Character does not match.
      Just match ->
        if   strat.stop query
        then trace ("Stopping at " <> show i <> "," <> show j <> " found " <> show c) match -- Stop search.
        else strat.count $ -- Count subqueries based on strategy.
          uncurry (searchFromCell strat grid) <$> strat.cells query (i, j) dir

-- | Step in a compass direction.
step :: Index -> Direction -> (Index, Direction)
step (i, j) N  = ((i-1, j  ), N )
step (i, j) NE = ((i-1, j+1), NE)
step (i, j) E  = ((i  , j+1), E )
step (i, j) SE = ((i+1, j+1), SE)
step (i, j) S  = ((i+1, j  ), S )
step (i, j) SW = ((i+1, j-1), SW)
step (i, j) W  = ((i  , j-1), W )
step (i, j) NW = ((i-1, j-1), NW)
