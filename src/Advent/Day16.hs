module Advent.Day16 where

import Advent.Prelude
import Data.List (intercalate, nub)
import Data.List.Extra (groupOn, minimumOn)
import Data.Map (Map)
import Data.Map qualified as Map

type Coords = (Int, Int)
type Cost   = Int
type Node   = (Coords, NSEW)
data NSEW   = N | S | E | W deriving (Eq, Ord)

instance Show NSEW where
  show N = "^"; show S = "v"; show E = ">"; show W = "<"

-- * Unvisited data type for fast lookup. --------------------------------------

newtype Unvisited = Unvisited (Map Node Cost, Map Cost [Node])

mkUnvisited :: Node -> Cost -> Unvisited
mkUnvisited n c = Unvisited (Map.singleton n c, Map.singleton c [n])

-- | Delete and find min by cost.
unvisitedDeleteFindMin :: Unvisited -> ((Node, Cost), Unvisited)
unvisitedDeleteFindMin (Unvisited (byNode, byCost)) = do
  let (c, (n:ns)) = Map.findMin byCost
  let byCost' = ($ byCost) if null ns then Map.delete c else Map.insert c ns
  ((n, c), Unvisited (Map.delete n byNode, byCost'))

unvisitedInsert :: Unvisited -> (Node, Cost) -> Unvisited
unvisitedInsert (Unvisited (byNode, byCost)) (n, c) =
  Unvisited (Map.insert n c byNode, Map.insertWith (<>) c [n] byCost)

unvisitedNull :: Unvisited -> Bool
unvisitedNull (Unvisited (byNode, _)) = Map.null byNode

unvisitedCost :: Unvisited -> Node -> Maybe Cost
unvisitedCost (Unvisited (byNode, _)) node = Map.lookup node byNode

--------------------------------------------------------------------------------

main :: IO ()
main = do
  (cells, start, end) <- readMaze "data/Day16.txt"
  let solved = dijkstra start cells Map.empty $ mkUnvisited (start, E) 0
  print $ minimumOn snd $ allPathsAt end solved -- Part 1
  let goodPaths = walk True start Nothing end solved
  print $ length . nub . map fst $ fst <$> goodPaths -- Part 2

addCost :: NSEW -> NSEW -> Cost -> Cost
addCost a b | a == b            = (+1)
addCost a b | a `elem` ninety b = (+1001)
addCost _ _                     = (+2001)

allPathsAt :: Coords -> Map Node Cost -> [(Node, Cost)]
allPathsAt coords shortestPaths = flip mapMaybe [N, S, E, W] \d ->
  ((coords,d),) <$> Map.lookup (coords, d) shortestPaths

dijkstra :: Coords -> Map Coords Bool -> Map Node Cost -> Unvisited -> Map Node Cost
dijkstra goal cells visited unvisited = do
  -- Select node to add to 'visited': the minimum cost node in 'unvisited'.
  let ((currNode, currCost), unvisited') = unvisitedDeleteFindMin unvisited
  let visited' = Map.insert currNode currCost visited
  -- Add each neighbouring node to 'unvisited' IFF:
  -- - cost is a new minimum (includes check that node is not in visited set).
  -- - the cell is not a wall!
  let isValidNeighbour neighbour@(coords, direction) = do
        let cost        = addCost (snd currNode) direction currCost
        let isWall      = Map.lookup coords cells == Just True
        let isLowerCost = maybe True (<= cost) $ unvisitedCost unvisited neighbour
        let notVisited  = neighbour `Map.notMember` visited
        if   coords >= (0, 0) && not isWall && isLowerCost && notVisited
        then Just cost else Nothing
  let validNeighbours = mapMaybe
        (\x -> (x,) <$> isValidNeighbour x) $ neighbours cells currNode
  let unvisited'' = foldl' unvisitedInsert unvisited' validNeighbours
  -- Stop if no more unvisited nodes, else recurse!
  if unvisitedNull unvisited'' then visited else dijkstra goal cells visited' unvisited''

-- | All NSEW neighbours of the given coordinates, no walls.
neighbours :: Map Coords Bool -> (Coords, NSEW) -> [Node]
neighbours cells (coords, facing) = flip mapMaybe (facing : ninety facing)
  \direction -> do
    let next = step True coords direction
    if Map.lookup next cells == Just True then Nothing else Just (next, direction)

ninety :: NSEW -> [NSEW]
ninety N = [E, W]; ninety S = [E, W]; ninety E = [N, S]; ninety W = [N, S]

-- | Step forward or backward.
step :: Bool -> Coords -> NSEW -> Coords
step f (i, j) N = (if f then i - 1 else i + 1,                      j    )
step f (i, j) S = (if f then i + 1 else i - 1,                      j    )
step f (i, j) E = (i                         , if f then j + 1 else j - 1)
step f (i, j) W = (i                         , if f then j - 1 else j + 1)

readMaze :: String -> IO (Map Coords Bool, Coords, Coords)
readMaze = fmap (parse . lines) . readFile
 where
  flat rows =
    [ ((i, j), c) | (i, row) <- zip [0..] rows, (j, c) <- zip [0..] row ]
  start           = fst . head . filter ((== 'S') . snd) . flat
  stop            = fst . head . filter ((== 'E') . snd) . flat
  parse      rows = (parseWalls rows, start rows, stop rows)
  parseWalls rows = Map.fromList [ (ij, c == '#') | (ij, c) <- flat rows ]

showMap :: Map Coords Bool -> Coords -> Coords -> [Node] -> String
showMap cells start end pathNodes = do
  let path = Map.fromList $ (fst &&& snd) <$> pathNodes
  intercalate "\n" $ map (map snd) $ groupOn (fst . fst) $ sortOn fst $
    Map.toList $ flip Map.mapWithKey cells \coords isWall ->
           if isWall          then '#'
      else if coords == start then 'S'
      else if coords == end   then 'E'
      else maybe '.' (head . show) $ Map.lookup coords path

-- | Walk the path back to the start from given 'Coords'. At each branch check
-- if the different paths have same cost.
walk :: Bool -> Coords -> Maybe Node -> Coords -> Map Node Cost -> [(Node, Cost)]
walk branch start prevMay currCoords shortestPaths = do
  let allPaths = allPathsAt currCoords shortestPaths
  let bestPath = minimumOn snd allPaths

  let bestPaths = case (branch, prevMay) of
        (True, Nothing) -> filter (\x -> snd x == snd bestPath) allPaths
        (True, Just (_, prevDirection)) -> do
          let trueCost = allPaths <&> \(node@(_, currDirection), currCost) ->
                (node, addCost currDirection prevDirection currCost)
          let trueBestCost = snd $ minimumOn snd trueCost
          filter ((== trueBestCost) . snd) trueCost
        _ -> [bestPath]

  flip concatMap bestPaths \((_, direction'), cost') -> do
    let nextCoords = step False currCoords direction'
    ((currCoords, direction'), cost') :
      if   currCoords == start then []
      else walk branch start (Just (currCoords, direction')) nextCoords shortestPaths
