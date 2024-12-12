module Advent.Day12 where

import Advent.Prelude
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

data Direction = U | D | L | R
type Index     = (Int, Int)
type Plots     = Map Index Char
type Plot      = Set Index

main :: IO ()
main = readPlots "data/Day12.txt" >>= (print . part1)

part1 :: Plots -> Int
part1 plots | Map.null plots = 0
part1 plots = do
  let (i, c) = Map.findMin plots
  let ((area, perim), plot) = areaAndPerimeter plots Set.empty i c
  -- (debugPre ("Area " <> [c] <> " " <> show i <> ": ") area * debugPre ("Perim " <> [c] <> " " <> show i <> ": ") perim)
  (area * perim) + part1 (foldl' (flip Map.delete) plots plot)

-- | Area + perimeter for plot at given index (not in given 'Plots').
areaAndPerimeter :: Plots -> Plot -> Index -> Char -> ((Int, Int), Plot)
areaAndPerimeter plots seen i c = first (first (+1)) $ -- Add 1 to area.
  foldl'
    (\(areaPerim, seen') d -> first (add areaPerim) $ f d seen')
    ((0, 0), Set.insert i seen) -- Update sites already counted in plot.
    [U, L, D, R]
 where
  f :: Direction -> Plot -> ((Int, Int), Plot)
  f d seen' = do
    let next     = move i d
    let nextSeen = next `elem` seen'
    case Map.lookup next plots of
      Just c' | c == c' && not nextSeen -> areaAndPerimeter plots seen' next c
      _ -> ((0, if nextSeen then 0 else 1), seen')

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a, b) (c, d) = (a + c, b + d)

move :: (Int, Int) -> Direction -> (Int, Int)
move (i, j) U = (i - 1, j    )
move (i, j) D = (i + 1, j    )
move (i, j) L = (i    , j - 1)
move (i, j) R = (i    , j + 1)

readPlots :: String -> IO Plots
readPlots = fmap (parsePlots . lines) . readFile
 where
  parsePlots rows = Map.fromList
    [ ((i, j), c) | (i, row) <- zip [0..] rows, (j, c) <- zip [0..] row ]
