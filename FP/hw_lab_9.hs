module HW where

import Data.List (sortOn)
import Data.Foldable (foldMap)
import Data.Monoid (All(..))
import Data.Char (isUpper)
import Data.List (nub, delete)
import qualified Data.Map as M

--------------------------------------------
-- sortRev Implementation
--------------------------------------------
newtype Rev a = Rev a
  deriving (Eq, Show)

instance Ord a => Ord (Rev a) where
  compare (Rev x) (Rev y) = compare y x

sortRev :: Ord a => [a] -> [a]
sortRev = sortOn Rev

--------------------------------------------
-- allUpper Implementation
--------------------------------------------
allUpper :: String -> Bool
allUpper = getAll . foldMap (All . isUpper)

--------------------------------------------
-- Graph Type Class Definition
--------------------------------------------
class Graph g where
  neighbors :: Ord a => g a -> a -> [a] 

--------------------------------------------
-- TupleGraph Implementation
--------------------------------------------
newtype TupleGraph a = TupleGraph [(a, a)]

instance Graph TupleGraph where
  neighbors (TupleGraph edges) node = [y | (x, y) <- edges, x == node]

--------------------------------------------
-- NeighborListGraph Implementation
--------------------------------------------
newtype NeighborListGraph a = NeighborListGraph (M.Map a [a])

instance Graph NeighborListGraph where
  neighbors (NeighborListGraph adjList) node = M.findWithDefault [] node adjList

--------------------------------------------
-- Helper Function
--------------------------------------------
helper :: (Graph g, Ord a) => ([a] -> [a] -> [a]) -> [a] -> g a -> [a] -> [a]
helper strategy [] _ visited = visited
helper strategy (current:queue) graph visited
  | current `elem` visited = helper strategy queue graph visited
  | otherwise = helper strategy (strategy queue (neighbors graph current)) graph (visited ++ [current])

--------------------------------------------
-- Breadth-First Search (BFS)
--------------------------------------------
bf :: (Graph g, Ord a) => g a -> a -> [a]
bf graph start = helper (++) [start] graph []

--------------------------------------------
-- Depth-First Search (DFS)
--------------------------------------------
df :: (Graph g, Ord a) => g a -> a -> [a]
df graph start = helper (flip (++)) [start] graph []
