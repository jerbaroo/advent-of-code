module Advent.Day9 where

import Advent.Prelude
import Data.Char (digitToInt)
import Data.String.Utils (strip)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List (sortOn)

type Block    = (Int, Maybe Int)
type DiskMap  = Map Int [Block]
type BlockMap = Map Int Block

main :: IO ()
main = readDiskMap >>=
  print . bimap checksum checksum . (defragPart1 &&& defragPart2)

checksum :: DiskMap -> Int
checksum = snd . foldl' f (0, 0) . concatMap snd . sortOn fst . Map.toList
 where
  f (i, total) (size, fileIdMay) = let j = i + size in
    (j, total + maybe 0 (sum . (<$> [i..(j-1)]) . (*)) fileIdMay)

defragPart1 :: BlockMap -> DiskMap
defragPart1 blockMap =
  defragPart1' 0 (fst $ Map.findMax blockMap) $ Map.map (:[]) blockMap
 where
  -- | Move file blocks at the right index to the left index.
  defragPart1' :: Int -> Int -> DiskMap -> DiskMap
  defragPart1' l r d | l >= r = d -- File index moved left past empty space index.
  defragPart1' l r d          = do
    let (lBlocks , rBlocks ) = let f x = fromJust $ Map.lookup x d in (f l, f r)
    let (lBlocks', rBlocks') = defragBlocks l lBlocks r rBlocks
    let (l', r') = if any (isNothing . snd) lBlocks' then (l, r-1) else (l+1, r)
    defragPart1' l' r' $ Map.insert l lBlocks' $ Map.insert r rBlocks' d

defragPart2 :: BlockMap -> DiskMap
defragPart2 blockMap = do
  let emptyIndices = Map.keys $ Map.filter (isNothing . snd) blockMap
  let fileIndices = reverse $ Map.keys $ Map.filter (isJust . snd) blockMap
  defragFiles' (Map.map (:[]) blockMap) emptyIndices fileIndices
 where
  -- | Find 'blocks' with 'fileSize' space that occur before 'fileIdx'.
  findEmptySpace :: DiskMap -> Int -> Int -> [Int] -> [(Int, [Block])]
  findEmptySpace diskMap fileSize fileIdx is = flip mapMaybe is \i ->
    case Map.lookup i diskMap of
      Nothing     -> Nothing
      Just blocks -> flip boolToMaybe (i, blocks) $ flip any blocks \case
        (size, Nothing) -> i < fileIdx && size >= fileSize
        _               -> False
  -- | Move first file (from right) to first space (from left), repeat.
  defragFiles' :: DiskMap -> [Int] -> [Int] -> DiskMap
  defragFiles' diskMap _ [] = diskMap -- No more files to check.
  defragFiles' diskMap [] _ = diskMap -- No more empty spaces.
  defragFiles' diskMap emptyIndices (fileIdx:fileIndices) =
    case head . snd <$> Map.lookupLE fileIdx diskMap of
      Just fileBlock@(fileSize, Just _) -> do
        case headMay $ findEmptySpace diskMap fileSize fileIdx emptyIndices of
          Nothing              -> defragFiles' diskMap emptyIndices fileIndices
          Just (i, blocks) -> do
            let (blocks', fileBlock') = defragBlocks i blocks fileIdx [fileBlock]
            defragFiles'
              (Map.insert i blocks' $ Map.insert fileIdx fileBlock' diskMap)
              emptyIndices
              fileIndices
      _ -> diskMap

defragBlocks :: Int -> [Block] -> Int -> [Block] -> ([Block], [Block])
defragBlocks _ []                       _ rs                           = ([], rs)
defragBlocks _ ls                       _ []                           = (ls, [])
defragBlocks l (lBlock@(_, Just _):ls)  r rs                           = first  (lBlock:) $ defragBlocks l ls r rs
defragBlocks l ls                       r (rBlock@(_, Nothing):rs)     =
  if   r == l + 1
  then defragBlocks l (ls <> [rBlock]) r rs
  else second (rBlock:) $ defragBlocks l ls r rs
defragBlocks l ((freeSize, Nothing):ls) r ((fileSize, Just fileId):rs) = do
  let moved = min freeSize fileSize
  let (lFile, lFree) = ((moved, Just fileId), (freeSize - moved, Nothing))
  let (rFile, rFree) = ((fileSize - moved, Just fileId), (moved, Nothing))
  let consIfNotEmpty (size, x) xs = if size == 0 then xs else (size, x) : xs
  bimap (consIfNotEmpty lFile) (consIfNotEmpty rFree) $
    defragBlocks l (consIfNotEmpty lFree ls) r (consIfNotEmpty rFile rs)

readDiskMap :: IO BlockMap
readDiskMap = readFile "data/Day9.txt" <&>
  Map.fromList . zip [0..] . f (Left 0) . map digitToInt . strip
 where
  f _ [] = []
  f (Left fileId ) (x:xs) = (x, Just fileId) : f (Right $ fileId + 1) xs
  f (Right fileId) (x:xs) = (x, Nothing    ) : f (Left fileId       ) xs
