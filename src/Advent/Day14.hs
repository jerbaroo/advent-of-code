module Advent.Day14 where

import Advent.Prelude
import Numeric.LinearAlgebra qualified as Matrix
import Numeric.LinearAlgebra.Data (Matrix)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M
import Data.Char (isDigit)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap

type Robot = ((Int, Int), (Int, Int)) -- Position (x, y) and velocity (vx, vy).

main :: IO ()
main = readRobots "data/Day14.txt" >>=
  let lenXY = (101, 103) in print . safetyFactor lenXY . map (move lenXY 100)

countPerQuadrant :: (Int, Int) -> [Robot] -> IntMap Int
countPerQuadrant lenXY =
  IntMap.fromListWith (+) . map ((,1) . quadrant lenXY . fst)

quadrant :: (Int, Int) -> (Int, Int) -> Int
quadrant (lenX, lenY) (x, y) = do
  let (midX, midY) = (lenX `div` 2, lenY `div` 2)
  if   x == midX || y == midY then -1
  else case (x < lenX `div` 2, y < lenY `div` 2) of
    (True , True ) -> 0
    (False, True ) -> 1
    (True , False) -> 2
    (False, False) -> 3

move :: (Int, Int) -> Int -> Robot -> Robot
move (lenX, lenY) n ((x, y), (vx, vy)) = do
  let (x', y') = ((x + vx * n) `rem` lenX, (y + vy * n) `rem` lenY)
  let f remA len = if remA < 0 then len - abs remA else remA
  ((f x' lenX, f y' lenY), (vx, vy))

safetyFactor :: (Int, Int) -> [Robot] -> Int
safetyFactor lenXY = foldl' (*) 1 . IntMap.elems .
  IntMap.filterWithKey (\k _ -> k >= 0) . countPerQuadrant lenXY

-- * Input & parsing.

parseRobots :: String -> Either String [Robot]
parseRobots = left show . M.runParser (M.many $ M.try parseRobot) ""
 where
  parseRobot = (,) <$> parseTuple   <*> parseTuple
  parseTuple = (,) <$> parseNextInt <*> parseNextInt

parseNextInt :: Parsec Void String Int
parseNextInt = do
  void $ M.takeWhile1P Nothing $ \c -> not (isDigit c) && c /= '-'
  read <$> M.takeWhile1P Nothing \c -> isDigit c || c == '-'

readRobots :: String -> IO [Robot]
readRobots = fmap (fromEither error . parseRobots) . readFile
