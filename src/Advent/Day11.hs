module Advent.Day11 where

import Advent.Prelude
import Data.Map (Map)
import Data.Map qualified as Map

type Blinks = Int
type Stone  = Int
type Memory = Map (Stone, Blinks) Int

main :: IO ()
main = readStones "data/Day11.txt" >>= (print . snd . blinkList Map.empty 75)

blink :: Memory -> Blinks -> Stone -> (Memory, Int)
blink mem 0 _ = (mem, 1)
blink mem b s = case Map.lookup (s, b) mem of
  Nothing ->
    let (mem', v) = blinkList mem (b - 1) $ transform s
    in  (Map.insert (s, b) v mem', v)
  Just  v -> (mem, v)

blinkList :: Memory -> Blinks -> [Stone] -> (Memory, Int)
blinkList mem b = foldl' (\(mem', v) s -> second (+v) $ blink mem' b s) (mem, 0)

readStones :: FilePath -> IO [Stone]
readStones = fmap (map read . words) . readFile

transform :: Stone -> [Stone]
transform 0 = [1]
transform n = do
  let str = show n
  let len = length str
  if   even len
  then let (a, b) = splitAt (len `div` 2) str in read <$> [a, b]
  else [n * 2024]
