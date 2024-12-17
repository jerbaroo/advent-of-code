module Advent.Day15 where

import Advent.Prelude
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set

type Coords     = (Int, Int)
data Item       = Box | BoxLHS | BoxRHS | Wall deriving (Eq, Show)
data Move       = U | D | L | R deriving Eq
type Robot      = Coords
type Update     = (Coords, Coords)
type Warehouse  = Map Coords Item

instance Show Move where
  show U = "^"; show D = "v"; show L = "<"; show R = ">"

main :: IO ()
main = readInput True "data/Day15.txt" >>= print . part1

part1 :: ((Robot, Warehouse), [Move]) -> Int
part1 = sum . map distance . Map.keys . Map.filter (`elem` [Box, BoxLHS]) . snd . uncurry applyMoves
 where
  distance :: Coords -> Int
  distance (i, j) = i * 100 + j

applyMoves :: (Robot, Warehouse) -> [Move] -> (Robot, Warehouse)
applyMoves x [] = x
applyMoves (robot, warehouse) (move:moves) = do
  case tryMove robot move warehouse [] of
    []      -> applyMoves (robot, warehouse) moves -- No updates to apply.
    updates' -> do
      let updates = filter ((/= robot) . fst) updates'
      let applyUpdate m (from, to) =
            Map.delete from $ Map.insert to (fromJust $ Map.lookup from m) m
      let new = (move1 robot move, foldl' applyUpdate warehouse updates)
      applyMoves new moves

tryMove :: Coords -> Move -> Warehouse -> [Update] -> [Update]
tryMove from move warehouse updates = do
  let to        = move1 from move
  let updates'  = (from, to) : updates
  let keepGoing = tryMove to move warehouse updates'
  case Map.lookup to warehouse of
    Nothing                     -> updates' -- Nothing blocking.
    Just Wall                   -> [] -- Wall blocking move.
    Just Box                    -> keepGoing
    Just _ | move `elem` [L, R] -> keepGoing -- Try move box.
    Just x                      -> do -- Either BoxLHS or BoxRHS
      let (fromL, fromR) = case x of
            BoxLHS -> (to, move1 to R)
            BoxRHS -> (to, move1 to L)
      let updatesL    = tryMove fromL move warehouse updates'
      let updatesLSet = Set.fromList updatesL
      let updatesR    = tryMove fromR move warehouse updatesL
      let updatesR'   = filter (`Set.notMember` updatesLSet) updatesR
      if any null [updatesL, updatesR] then [] else updatesL <> updatesR'

move1 :: Coords -> Move -> Coords
move1 (i, j) U = (i - 1, j    )
move1 (i, j) D = (i + 1, j    )
move1 (i, j) L = (i    , j - 1)
move1 (i, j) R = (i    , j + 1)

-- * Reading & writing.

readInput :: Bool -> String -> IO ((Robot, Warehouse), [Move])
readInput double =
  fmap (parse . bimap (map widen) concat . break (== "") . lines) . readFile
 where
  parse :: ([String], String) -> ((Robot, Warehouse), [Move])
  parse = parseWarehouse *** mapMaybe parseMove

  parseItem :: Char -> Maybe Item
  parseItem = \case
    '#' -> Just Wall; 'O' -> Just Box; '[' -> Just BoxLHS; ']' -> Just BoxRHS;
    _   -> Nothing

  parseMove :: Char -> Maybe Move
  parseMove = \case
    '<' -> Just L; '^' -> Just U; 'v' -> Just D; '>' -> Just R; _ -> Nothing

  parseWarehouse :: [String] -> (Robot, Warehouse)
  parseWarehouse rows = bimap (head . Map.keys) (Map.mapMaybe parseItem) $
    Map.partition (== '@') $ Map.fromList $
      [ ((i, j), c) | (i, row) <- zip [0..] rows , (j, c)   <- zip [0..] row ]

  widen :: String -> String
  widen = if not double then id else concatMap \case
    '#' -> "##"; 'O' -> "[]"; '.' -> ".."; '@' -> "@."; x -> [x]

showWarehouse :: (Robot, Warehouse) -> String
showWarehouse (robot, warehouse) = do
  let findDim :: (Coords -> Int) -> [Coords] -> Int
      findDim f = last . sort . map f
  let (maxI, maxJ) = (findDim fst &&& findDim snd) $ Map.keys warehouse
  let f coords = case Map.lookup coords warehouse of
        Nothing     -> if coords == robot then '@' else '.'
        Just Box    -> 'O'
        Just BoxLHS -> '['
        Just BoxRHS -> ']'
        Just Wall   -> '#'
  intercalate "\n" [ [ f (i, j) | j <- [0..maxJ] ] | i <- [0..maxI] ]
