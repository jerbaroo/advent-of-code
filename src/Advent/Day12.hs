module Advent.Day12 where

import Advent.Prelude
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

data Direction = U | D | L | R deriving (Eq, Ord, Show)
type Garden    = Map Plot Plant
type Plot      = (Int, Int)
type Plant     = Char
type Region    = Set Plot

main :: IO ()
main = readGarden "data/Day12.txt" >>= \garden ->
  forM_ [(area, perimeter), (area, walls)] \(f, g) ->
    print $ sum $ uncurry (*) . (f &&& g) <$> allRegions garden

-- * Determine regions in garden.

allRegions :: Garden -> [Region]
allRegions garden | Map.null garden = []
allRegions garden = do
  let region = uncurry (findRegion garden Set.empty) $ Map.findMin garden
  region : allRegions (foldl' (flip Map.delete) garden $ Set.toList region)

findRegion :: Garden -> Region -> Plot -> Char -> Region
findRegion garden region plot plant =
  foldl' f (Set.insert plot region) [U, D, L, R]
 where
  f :: Region -> Direction -> Region
  f region' direction = do
    let nextPlot = step direction plot
    if   nextPlot `elem` region'
    then region'
    else case Map.lookup nextPlot garden of
      Just plant' | plant' == plant -> findRegion garden region' nextPlot plant
      _                             -> region'

-- * Functions on regions.

area :: Region -> Int
area = Set.size

perimeter :: Region -> Int
perimeter region = sum $ Set.toList region <&> \plot ->
  length $ filter id $ [U, D, L, R] <&> \dir ->
    step dir plot `Set.notMember` region

walls :: Region -> Int
walls region = sum $ Set.toList region <&> \plot ->
  length $ filter id $ [(U, R), (R, D), (D, L), (L, U)] <&> \(a, b) ->
    case [step a plot, step a $ step b plot, step b plot] <&> (`Set.member` region) of
      [False, _    , False] -> True
      [True , False, True ] -> True
      _                     -> False

-- * Parsers and helpers.

readGarden :: String -> IO Garden
readGarden = fmap (parseGarden . lines) . readFile
 where
  parseGarden rows = Map.fromList
    [ ((i, j), c) | (i, row) <- zip [0..] rows, (j, c) <- zip [0..] row ]

step :: Direction -> Plot -> Plot
step U (i, j) = (i - 1, j    )
step D (i, j) = (i + 1, j    )
step L (i, j) = (i    , j - 1)
step R (i, j) = (i    , j + 1)
