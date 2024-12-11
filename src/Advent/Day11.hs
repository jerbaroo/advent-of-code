module Advent.Day11 where

import Advent.Prelude
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap

type Stone   = Int
type Counter = IntMap Int

main :: IO ()
main = readStones "data/Day11.txt" >>= (print . count 75)

count :: Int -> [Stone] -> Int
count n = sum . map snd . IntMap.toList . times n blink .
  IntMap.fromListWith (+) . fmap (,1)

blink :: Counter -> Counter
blink c = IntMap.fromListWith (+)
  [ (s', x) | (s, x) <- IntMap.assocs c, s' <- transform s ]

readStones :: FilePath -> IO [Stone]
readStones = fmap (map read . words) . readFile

transform :: Stone -> [Stone]
transform 0 = [1]
transform n
  | (halfLen, 0) <- length (show n) `quotRem` 2
  = let (l, r) = n `quotRem` (10 ^ halfLen) in [l, r]
transform n = [n * 2024]
