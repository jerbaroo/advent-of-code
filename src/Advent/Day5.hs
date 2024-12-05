module Advent.Day5 where

import Advent.Prelude
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Set qualified as Set
import Data.Set (Set)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC

-- | Rule (x, y) means x must be before y.
--
-- An update is INCORRECT if y is seen before x.
type Rule = (Int, Int)

-- | Key y and value xs.
--
-- An update is INCORRECT if any x in xs is seen after y.
type FailRules = Map Int (Set Int)

type Update = [Int]

main :: IO ()
main = do
  readRulesAndUpdates >>= \case
    Left err -> putStrLn $ "Error: " <> err
    Right (rules, updates) -> do
      let failRules'   = failRules rules -- Convert X|Y to Map Y (Set X)
      -- Convert correct updates to Just Int and incorrect to Nothing.
      let middleMaybes = middleIfCorrect failRules' <$> updates
      -- Sum the middles for part 1 answer.
      print $ sum $ catMaybes middleMaybes
      -- Determine the incorrect updates.
      let incorrect = catMaybes $ zipWith
            (\u m -> if isNothing m then Just u else Nothing)
            updates middleMaybes
      -- Correct the incorrect updates.
      let corrected = mkCorrect failRules' <$> incorrect
      -- Sum the middles of for part 2 answer.
      print $ sum $ mapMaybe (middleIfCorrect failRules') corrected

failRules :: [Rule] -> FailRules
failRules = Map.fromListWith Set.union . (<&> second Set.singleton . swap)

mkCorrect :: FailRules -> [Int] -> [Int]
mkCorrect failRules' = sortBy \a b ->
  if a `Set.member` fromMaybe Set.empty (Map.lookup b failRules') then LT else GT

sumOfMiddles :: FailRules -> [Update] -> Int
sumOfMiddles failRules' = sum . mapMaybe (middleIfCorrect failRules')

middleIfCorrect :: FailRules -> Update -> Maybe Int
middleIfCorrect failRules' update = foldM f Set.empty update $> middle update
 where
  middle xs = xs !! (length xs `div` 2)
  f failIfSeen x = if x `Set.member` failIfSeen then Nothing else
    Just $ maybe failIfSeen (Set.union failIfSeen) $ Map.lookup x failRules'

-- * Input: reading & parsing.

parseRule :: Parsec Void String Rule
parseRule = do
  x <- read <$> M.many (M.satisfy isDigit)
  _ <- M.single '|'
  y <- read <$> M.many (M.satisfy isDigit)
  pure (x, y)

parseRules :: Parsec Void String [Rule]
parseRules = M.many (M.try $ parseRule <* MC.space)

parseUpdate :: Parsec Void String Update
parseUpdate = M.sepBy1 (read <$> M.some (M.satisfy isDigit)) $ M.single ','

parseUpdates :: Parsec Void String [Update]
parseUpdates = M.many (parseUpdate <* MC.newline)

readRulesAndUpdates :: IO (Either String ([Rule], [Update]))
readRulesAndUpdates = readFile "data/Day5.txt"
  <&> left show . M.runParser ((,) <$> parseRules <*> parseUpdates) ""
