module Advent.Day13 where

import Advent.Prelude
import Numeric.LinearAlgebra qualified as Matrix
import Numeric.LinearAlgebra.Data (Matrix)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M
import Data.Char (isDigit)

type Equation = ((Int, Int, Int), (Int, Int, Int))

main :: IO ()
main = readMatrices True "data/Day13.txt" >>= print . solve

solve :: [Equation] -> Int
solve = sum . map (\(a, b) -> 3 * a + b) . mapMaybe solveEquation

solveEquation :: Equation -> Maybe (Int, Int)
solveEquation ((a1, b1, x1), (a2, b2, x2)) = do
  [m, n] <- map round . concat . Matrix.toLists <$>
    Matrix.linearSolve (matrix [[a1, b1], [a2, b2]]) (matrix [[x1], [x2]])
  let solved m' a n' b x = m' * a + n' * b == x
  if solved m a1 n b1 x1 && solved m a2 n b2 x2 then Just (m, n) else Nothing

matrix :: [[Int]] -> Matrix Double
matrix = Matrix.fromLists . map (map fromIntegral)

-- * Input & parsing.

readMatrices :: Bool -> String -> IO [Equation]
readMatrices part2 = fmap (fromEither error . parseEquations part2) . readFile

parseEquations :: Bool -> String -> Either String [Equation]
parseEquations part2 = left show . M.runParser (M.many $ M.try parseEquation) ""
 where
  f x = if part2 then 10000000000000 + x else x
  parseEquation = M.count 6 parseNextInt <&>
    \[a1, a2, b1, b2, x1, x2] -> ((a1, b1, f x1), (a2, b2, f x2))

parseNextInt :: Parsec Void String Int
parseNextInt = do
  void $ M.takeWhile1P Nothing (not . isDigit)
  read <$> M.takeWhile1P Nothing isDigit
