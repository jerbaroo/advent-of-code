module Advent.Day10 where

import Advent.Prelude
import Data.Char (digitToInt)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Data.Vector (Vector, (!), (!?))
import Data.Proxy (Proxy(..))

main :: IO ()
main = readTopoMap "data/Day10.txt" >>=
  print . (result (Proxy @Peaks) &&& result (Proxy @Trails))

data Direction = U | D | L | R deriving (Eq, Ord, Show)
type Position  = (Int, Int)
type TopoMap   = Vector (Vector Int)
type Reachable = Set (Position, Direction)

gradualNeighbours :: TopoMap -> Position -> [(Position, Direction)]
gradualNeighbours topo (i, j) = do
  let height = topo ! i ! j
  [ ((h, k), dir)
    | (h, k, dir) <- [(i-1, j, U), (i+1, j, D), (i, j-1, L), (i, j+1, R)]
    , (topo !? h >>= (!? k)) == Just (height + 1)
    ]

hike :: Result a => TopoMap -> Position -> a
hike topo = hike' Set.empty topo . (, Nothing)

hike' :: Result a => Reachable -> TopoMap -> (Position, Maybe Direction) -> a
hike' seen _    (pos, Just dir) | (pos, dir) `Set.member` seen = onLoop
hike' _    topo ((i, j), _)     | topo ! i ! j == 9            = onPeak (i, j)
hike' seen topo (pos, dirMay)   = combine do
  let nextSeen = maybe id (Set.insert . (pos,)) dirMay seen
  hike' nextSeen topo . second Just <$> gradualNeighbours topo pos

readTopoMap :: String -> IO TopoMap
readTopoMap = fmap (parseLines . lines) . readFile
 where
  parseLines = V.fromList . map V.fromList . (<&&> digitToInt)

result :: forall a. Result a => Proxy a -> TopoMap -> Int
result _ topo = finalize $ trailheads topo <&> hike @a topo

trailheads :: TopoMap -> [Position]
trailheads topo =
  [ (i, j)
  | (i, row)    <- V.toList $ V.indexed topo
  , (j, height) <- V.toList $ V.indexed row
  , height == 0
  ]

class Result a where
  combine  :: [a] -> a
  finalize :: [a] -> Int
  onLoop   :: a
  onPeak   :: Position -> a

newtype Trails = Trails Int deriving Num

instance Result Trails where
  combine  = sum
  finalize = (\(Trails i) -> i) . sum
  onLoop   = 0
  onPeak   = const 1

newtype Peaks = Peaks (Set Position)

instance Result Peaks where
  combine  = Peaks . foldl' Set.union Set.empty . map \(Peaks x) -> x
  finalize = sum . map \(Peaks s) -> Set.size s
  onLoop   = Peaks Set.empty
  onPeak   = Peaks . Set.singleton
