module Advent.Day1 where

import Advent.Prelude
import Data.Map qualified as Map
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC

main :: IO ()
main = readLists >>= \case
  Left err -> putStrLn $ "Error: " <> err
  Right lists -> do
    print $ part1 lists
    print $ part2 lists

readLists :: IO (Either String ([Int], [Int]))
readLists = readFile "data/Day2.txt" <&> bimap show unzip . parseFile

parseFile :: String -> Either (ParseErrorBundle String Void) [(Int, Int)]
parseFile = M.runParser (M.many parseLine) ""

parseLine :: Parsec Void String (Int, Int)
parseLine = do
  let parseInt = (read <$> M.takeP Nothing 5) <* MC.space
  (,) <$> parseInt <*> parseInt

part1 :: ([Int], [Int]) -> Int
part1 = sum . map abs . uncurry (zipWith (-)) . bimap sort sort

part2 :: ([Int], [Int]) -> Int
part2 (l, r) = do
  let rCounts = foldl' (\m a -> Map.insertWith (\_ old -> old + 1) a 1 m) Map.empty r
  sum $ l <&> \a -> maybe 0 (* a) $ Map.lookup a rCounts
