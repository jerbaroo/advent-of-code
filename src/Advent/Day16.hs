module Advent.Day16 where

import Advent.Prelude
import Data.List (intercalate, nub)
import Data.List.Extra (groupOn)
import Data.Map (Map)
import Data.Map qualified as Map

type Coords = (Int, Int)
type Cost   = Int
type Node   = (Coords, NSEW)
data NSEW   = N | S | E | W deriving (Eq, Ord)

instance Show NSEW where
  show N = "^"
  show S = "v"
  show E = "<"
  show W = ">"

main :: IO ()
main = do
  (cells, start, end) <- readMaze "data/Day16-example.txt"
  let solved = dijkstra start cells Map.empty (Map.singleton (start, E) 0)
  let shortestPath = walk True start end solved
  putStrLn $ showMap cells start end $ fst <$> shortestPath
  print $ shortestPath
  print $ goodSeats $ fst <$> shortestPath
  mapM_ print $ sort $ Map.toList solved

deleteFindMin :: Map Node Cost -> ((Node, Cost), Map Node Cost)
deleteFindMin m = do
  let (node, cost) = head $ sortOn snd $ Map.toList m
  ((node, cost), Map.delete node m)

dijkstra :: Coords -> Map Coords Bool -> Map Node Cost -> Map Node Cost -> Map Node Cost
dijkstra goal cells visited unvisited = do
  -- Select node to add to 'visited': the minimum cost node in 'unvisited'.
  let ((currNode, currCost), unvisited') = deleteFindMin unvisited
  let visited' = Map.insert currNode currCost visited
  -- Add each neighbouring node to 'unvisited' IFF:
  -- - cost is a new minimum (includes check that node is not in visited set).
  -- - the cell is not a wall!
  let isValidNeighbour neighbour@(coords, direction) = do
        let cost        = addCost (snd currNode) direction currCost
        let isWall      = Map.lookup coords cells == Just True
        let isLowerCost = maybe True (< cost) $ Map.lookup neighbour unvisited
        if   not isWall && isLowerCost && neighbour `Map.notMember` visited
        then Just cost else Nothing
  let validNeighbours = mapMaybe
        (\x -> (x,) <$> isValidNeighbour x) $ neighbours cells currNode
  let unvisited'' = foldl' (flip $ uncurry Map.insert) unvisited' validNeighbours
  -- Stop if no more unvisited nodes, else recurse!
  if Map.null unvisited'' then visited else dijkstra goal cells visited' unvisited''

-- | All NSEW neighbours of the given coordinates, no walls.
neighbours :: Map Coords Bool -> (Coords, NSEW) -> [Node]
neighbours cells (coords, facing) = flip mapMaybe (facing : ninety facing)
  \direction -> do
    let next = step True coords direction
    if Map.lookup next cells == Just True then Nothing else Just (next, direction)

walk :: Bool -> Coords -> Coords -> Map Node Cost -> [(Node, Cost)]
walk branch start coords shortestPaths = do
  -- Select the lowest cost to the given 'Coords'.
  let lowestCost = (if branch then id else (:[]) . head) $ sortOn fst $ flip mapMaybe [N, S, E, W]
        \d -> (,d) <$> Map.lookup (coords, d) shortestPaths
  flip concatMap (trace (show $ length lowestCost) lowestCost) \(cost, direction) -> do
    let prevCoords = step False coords direction
    ((coords, direction), cost) :
      if coords == start then [] else walk False start prevCoords shortestPaths

goodSeats :: [Node] -> Int
goodSeats = length . nub . map fst

-- | Step forward or backward.
step :: Bool -> Coords -> NSEW -> Coords
step f (i, j) N = (if f then i - 1 else i + 1,                      j    )
step f (i, j) S = (if f then i + 1 else i - 1,                      j    )
step f (i, j) E = (i                         , if f then j - 1 else j + 1)
step f (i, j) W = (i                         , if f then j + 1 else j - 1)

ninety :: NSEW -> [NSEW]
ninety N = [E, W]; ninety S = [E, W]; ninety E = [N, S]; ninety W = [N, S]

addCost :: NSEW -> NSEW -> Cost -> Cost
addCost a b | a == b            = (+1)
addCost a b | a `elem` ninety b = (+1001)
addCost _ _                     = (+2001)

readMaze :: String -> IO (Map Coords Bool, Coords, Coords)
readMaze = fmap (parse . lines) . readFile
 where
  start      rows = (length rows - 2, 1)
  stop       rows = (1, length (head rows) - 2)
  parse      rows = (parseWalls rows, start rows, stop rows)
  parseWalls rows = Map.fromList
    [ ((i, j), c == '#')
    | (i, row) <- zip [0..] rows
    , (j, c)   <- zip [0..] row
    ]

showMap :: Map Coords Bool -> Coords -> Coords -> [Node] -> String
showMap cells start end pathNodes = do
  let path = Map.fromList $ (fst &&& snd) <$> pathNodes
  intercalate "\n" $ map (map snd) $ groupOn (fst . fst) $ sortOn fst $
    Map.toList $ flip Map.mapWithKey cells \coords isWall ->
           if isWall          then '#'
      else if coords == start then 'S'
      else if coords == end   then 'E'
      else maybe '.' (head . show) $ Map.lookup coords path
