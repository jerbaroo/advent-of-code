module Advent.Day19 where

import Advent.Prelude
import Data.List.Extra (splitOn)
import Data.Map qualified as Map
import Data.Trie.Map qualified as Trie
import Data.Trie.Map (TMap)
import Data.Trie.Map.Internal (TMap(..), Node(..))

type Design = [Char]
type Towel  = [Char]
type Trie a = TMap a ()
type Towels = Trie Char

part1 :: [Towel] -> [Design] -> Int
part1 ts = length . filter id . fmap (`canBuildFrom` removeCombos ts)

part2 :: [Towel] -> [Design] -> Int
part2 ts = sum . map fst . tail . scanl (\(_, t) d -> waysToBuild t d $ trie ts) (0, Trie.empty)

-- | Can we build the given sequence out of combinations from the trie.
canBuildFrom :: Ord a => [a] -> Trie a -> Bool
canBuildFrom [] _  = True
canBuildFrom as t =
  any (\pre -> canBuildFrom (drop (length pre) as) t) $ prefixes as t

waysToBuild :: Ord a => TMap a Int -> [a] -> Trie a -> (Int, TMap a Int)
waysToBuild t x _ | Just n <- Trie.lookup x t = (n, t)
waysToBuild t x patterns = do
  let go t' prefix | prefix == x = (1, t')
      go t' prefix = waysToBuild t' (drop (length prefix) x) patterns
  -- Sum the ways to build for each matched prefix.
  (fst &&& uncurry (Trie.insert x)) $
    foldl' (\(n, t') -> first (+n) . go t') (0, t) $ prefixes x patterns

-- | Prefixes of given word that appear in the trie, smallest first.
prefixes :: Ord a => [a] -> TMap a b -> [[a]]
prefixes []     _                 = []
prefixes (a:as) (TMap (Node _ e)) =
  case Map.lookup a e of
    Nothing -> []
    Just t' ->
      let x = (a:) <$> prefixes as t'
      in  if [] `Trie.member` t' then [a]:x else x

-- | Trie WITHOUT sequences that can be built from smaller sequences.
removeCombos :: Ord a => [[a]] -> Trie a
removeCombos xs = go (sortOn ((* (-1)) . length) xs) $ trie xs
 where
  go []     t = t
  go (a:as) t = do
    let t' = Trie.delete a t
    if canBuildFrom a t' then go as t' else go as t

-- | Build a trie from lists of 'a'.
trie :: (Foldable f, Ord a) => f [a] -> Trie a
trie = foldl' (flip (`Trie.insert` ())) Trie.empty

-- * IO.

main :: IO ()
main = readInput "data/Day19.txt" >>= \(towels, designs) -> do
  print $ part1 towels designs
  print $ part2 towels designs

readInput :: String -> IO ([Towel], [Design])
readInput =
  fmap ((parseTowels *** parseDesigns) . break null . lines) . readFile
 where
  parseDesigns      = filter (not . null)
  parseTowels [p] = splitOn ", " p
  parseTowels _   = error "Expect 1 line of towels"
