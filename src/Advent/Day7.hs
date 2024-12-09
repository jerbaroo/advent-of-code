{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Advent.Day7 where

import Prelude hiding (Left, Right, (||))

import Advent.Prelude
import Data.List.Extra (splitOn)

type Equation = (Int, [Int])
type Operator = Int -> Int -> Int

main :: IO ()
main = readEquations >>= \equations ->
  mapM_ (print . flip calibrate equations) [[(+), (*)], [(+), (*), (||)]]

(||) :: Int -> Int -> Int
(||) a b = read $ show a <> show b

calibrate :: [Operator] -> [Equation] -> Int
calibrate ops = sum . map \(lhs, (x:xs)) -> if solveable ops lhs x xs then lhs else 0

solveable :: [Operator] -> Int -> Int -> [Int] -> Bool
solveable _   lhs x1 []      = lhs == x1
solveable ops lhs x1 (x2:xs) = any (\op -> solveable ops lhs (x1 `op` x2) xs) ops

readEquations :: IO [Equation]
readEquations = (readFile "data/Day7.txt" <&> lines) <&> map \l ->
  let [lhs, operands] = splitOn ": " l
  in  (read lhs, read <$> splitOn " " operands)
