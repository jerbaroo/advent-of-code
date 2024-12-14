module Advent.Day14 where

import Advent.Prelude
import Data.Char (isDigit)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M

type Robot = ((Int, Int), (Int, Int)) -- Position (x, y) and velocity (vx, vy).

main :: IO ()
main = readRobots "data/Day14.txt" >>= \robots -> do
  let lenXY@(_, lenY) = (101, 103)
  let treeTime = fst $ last $ sortOn snd $ zip [0..] $ take 10000 $
        longestVLine lenY <$> iterate (<&> move lenXY 1) robots
  writeRobots lenXY $ move lenXY treeTime <$> robots
  putStrLn $ "Part 1: " <> show (safetyFactor lenXY $ move lenXY 100 <$> robots)
  putStrLn $ "Part 2: " <> show treeTime

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

longestVLine :: Int -> [Robot] -> Int
longestVLine lenY robots =
  let positions = Set.fromList $ fst <$> robots in maximum
  [ longestVLineAtX (x, 0) 0 0 positions | x <- fst <$> Set.toList positions ]
 where
  longestVLineAtX :: (Int, Int) -> Int -> Int -> Set (Int, Int) -> Int
  longestVLineAtX (_, y) bestLen _        _        | y == lenY - 1 = bestLen
  longestVLineAtX (x, y) bestLen currLen positions = do
    let currLen' = if (x, y) `Set.member` positions then currLen + 1 else 0
    longestVLineAtX (x, y + 1) (max bestLen currLen') currLen' positions

move :: (Int, Int) -> Int -> Robot -> Robot
move (lenX, lenY) n ((x, y), (vx, vy)) = do
  let (x', y') = ((x + vx * n) `rem` lenX, (y + vy * n) `rem` lenY)
  let f remA len = if remA < 0 then len - abs remA else remA
  ((f x' lenX, f y' lenY), (vx, vy))

safetyFactor :: (Int, Int) -> [Robot] -> Int
safetyFactor lenXY = foldl' (*) 1 . IntMap.elems .
  IntMap.filterWithKey (\k _ -> k >= 0) . countPerQuadrant lenXY

-- * Reading & writing.

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

writeRobots :: (Int, Int) -> [Robot] -> IO ()
writeRobots (lenX, lenY) = mapM_ putStrLn . fmap toList . toList . foldl'
  (\s ((x, y), _) -> Seq.adjust (Seq.adjust (const 'X') x) y s)
  (Seq.fromList [ Seq.fromList ['.' | _ <- [1..lenX] ] | _ <- [1..lenY] ])
