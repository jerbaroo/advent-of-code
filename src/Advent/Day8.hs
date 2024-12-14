module Advent.Day8 where

import Advent.Prelude
import Data.List (nub)
import Data.List.Extra (groupOn)

type Index = (Int, Int)

main :: IO ()
main = readAntennas >>= \antennas -> mapM_
  (print . ($ antennas) . uncurry . countAntinodes) [take 1 . drop 1, id]

countAntinodes :: ([Index] -> [Index]) -> Index -> [[Index]] -> Int
countAntinodes f maxIndex = length . nub . concat . concatMap \antennas ->
  [ antiNodesForFrequency maxIndex f a b | a <- antennas, b <- antennas, a /= b ]

antiNodesForFrequency :: Index -> ([Index] -> [Index]) -> Index -> Index -> [Index]
antiNodesForFrequency maxIndex f (i1, j1) (i2, j2) = do
  let (dI, dJ) = (i1 - i2, j1  - j2)
  let go g h = f . takeWhile (onMap maxIndex) . iterate (bimap g h)
  go (+dI) (+dJ) (i1, j1) <> go (subtract dI) (subtract dJ) (i2, j2)

onMap :: Index -> Index -> Bool
onMap (maxI, maxJ) (i, j) = i >= 0 && j >= 0 && i <= maxI && j <= maxJ

readAntennas :: IO (Index, [[Index]])
readAntennas = (readFile "data/Day8.txt" <&> lines) <&> \rows ->
  ( ( length rows - 1, length (head rows) - 1 )
  , (<&&> fst) . groupOn snd . sortOn snd $
    [ ((i, j), c) | (i, row) <- zip [0..] rows, (j, c) <- zip [0..] row, c /= '.' ]
  )
