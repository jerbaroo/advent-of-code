module Advent.Day2 where

import Advent.Prelude
import Data.Char (isDigit)
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC

type Report = [Int]

main :: IO ()
main = readReports >>= \case
  Left err -> putStrLn $ "Error: " <> err
  Right reports -> print $ (countSafe False &&& countSafe True) reports

readReports :: IO (Either String [Report])
readReports = readFile "data/Day2.txt" <&> left show . parseReports

parseReports :: String -> Either (ParseErrorBundle String Void) [Report]
parseReports = M.runParser (M.many parseReport) ""

parseReport :: Parsec Void String Report
parseReport = M.sepBy1 (read <$> M.some (M.satisfy isDigit)) (M.single ' ') <* MC.newline

countSafe :: Bool -> [Report] -> Int
countSafe runDampener = length . filter id . map 
  if runDampener then any (isSafe Nothing) . dropN 1 [] else isSafe Nothing

dropN :: Int -> [a] -> [a] -> [[a]]
dropN 0 seen xs = [seen <> xs]
dropN _ seen [] = [seen]
dropN n seen xs = [seen <> drop n xs] <> dropN n (seen <> take n xs) (drop n xs)

isSafe :: Maybe Bool -> Report -> Bool
isSafe seenInc (x1:x2:xs) = do
  let inc = x2 > x1 -- Is x1 increasing to x2?
  let incCheck = maybe True (== inc) seenInc -- Still dec/increasing?
  let diff = abs $ x1 - x2 -- Absolute difference between x1 and x2.
  let diffCheck = diff `elem` [1 .. 3] -- Difference in ok range.
  incCheck && diffCheck && isSafe (Just inc) (x2:xs)
isSafe _ _ = True
