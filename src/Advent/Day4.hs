module Advent.Day4 where

import Advent.Prelude
import Control.Concurrent.Async (forConcurrently)
import Data.Vector qualified as V
import Data.Vector (Vector, (!?))

type Grid a = Vector (Vector a)

type Index = (Int, Int)

type Search a = [(a, Index)]

data Compass = N | NE | E | SE | S | SW | W | NW deriving Show

main :: IO ()
main = print =<< (readGrid >>= forM [searchGrid part1, searchGrid part2] . (&))

readGrid :: IO (Grid Char)
readGrid = readFile "data/Day4.txt" <&> V.fromList . map V.fromList . lines

part1 :: Index -> [Search Char]
part1 index = [N, NE, E, SE, S, SW, W, NW] <&> \dir ->
  scanl (\(_, index') char -> (char, step index' dir)) ('X', index) "MAS"

part2 :: Index -> [Search Char]
part2 index =
  [ [ (a, step index NE), (c, step index NW)
    ,              ('A', index)
    , (d, step index SE), (b, step index SW)
    ]
  | [a, b] <- ["MS", "SM"], [c, d] <- ["MS", "SM"]
  ]

-- | Step in a compass direction.
step :: Index -> Compass -> Index
step (i, j) N  = (i-1, j  )
step (i, j) NE = (i-1, j+1)
step (i, j) E  = (i  , j+1)
step (i, j) SE = (i+1, j+1)
step (i, j) S  = (i+1, j  )
step (i, j) SW = (i+1, j-1)
step (i, j) W  = (i  , j-1)
step (i, j) NW = (i-1, j-1)

-- | Run searches starting at each cell concurrently.
searchGrid :: Eq a => (Index -> [Search a]) -> Grid a -> IO Int
searchGrid genSearches grid = do
  let maxI = V.length grid
  let maxJ = maybe 0 V.length (grid !? 0)
  fmap sum $ forConcurrently
    [ (i, j) | i <- [0 .. maxI], j <- [0 .. maxJ] ]
    $ pure . countSearches grid . genSearches

-- | Count of given searches which are succesful.
countSearches :: Eq a => Grid a -> [Search a] -> Int
countSearches grid = length . filter id . map runSearch
 where
  runSearch = all \(expected, (i, j)) ->
    (grid !? i >>= (!? j)) == Just expected
