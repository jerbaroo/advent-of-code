module Advent.Day5 where

import Advent.Prelude
import Data.Char (isDigit)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Set qualified as Set
import Data.Set (Set)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC

-- | Rule (a, b) means FAIL if b comes after a.
type Rule = (Int, Int)

-- | For key k and value v, FAIL if any Int in v comes after k.
type Rules = Map Int (Set Int)

type Update = [Int]

main :: IO ()
main = do
  readInput >>= \case
    Left err -> putStrLn $ "Error: " <> err
    Right input -> print $ uncurry part1 input

part1 :: Rules -> [Update] -> Int
part1 rules = sum . mapMaybe (isCorrect rules)

isCorrect :: Rules -> Update -> Maybe Int
isCorrect rules update = foldM f Set.empty update $> middle update
 where
  middle xs = xs !! (length xs `div` 2)
  f failIfSeen x = if x `Set.member` failIfSeen then Nothing else
    Just $ maybe failIfSeen (Set.union failIfSeen) $ Map.lookup x rules

-- * Input: reading & parsing.

parseRule :: Parsec Void String Rule
parseRule = do
  x <- read <$> M.many (M.satisfy isDigit)
  _ <- M.single '|'
  y <- read <$> M.many (M.satisfy isDigit)
  pure (x, y)

parseRules :: Parsec Void String Rules
parseRules = M.many (M.try $ parseRule <* MC.space)
  <&> Map.fromListWith Set.union . (<&> second Set.singleton . swap)

parseUpdate :: Parsec Void String Update
parseUpdate = M.sepBy1 (read <$> M.some (M.satisfy isDigit)) $ M.single ','

parseUpdates :: Parsec Void String [Update]
parseUpdates = M.many (parseUpdate <* MC.newline)

readInput :: IO (Either String (Rules, [Update]))
readInput = readFile "data/Day5.txt"
  <&> left show . M.runParser ((,) <$> parseRules <*> parseUpdates) ""
