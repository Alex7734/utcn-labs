module HW where

import Data.List (sortOn)
import Data.Foldable (foldMap)
import Data.Monoid (All(..))
import Data.Char (isUpper)
import Data.List (nub, delete)
import qualified Data.Map as M

--- 9.9.5

newtype Reverse a = Reverse a
  deriving (Eq, Show)

instance Ord a => Ord (Reverse a) where
  compare (Reverse x) (Reverse y) = compare y x

sortDescending :: Ord a => [a] -> [a]
sortDescending = sortOn Reverse

--- 9.9.6

allCharactersUpper :: String -> Bool
allCharactersUpper = getAll . foldMap (All . isUpper)

--- 9.9.7

class GraphStructure g where
  getNeighbors :: Ord a => g a -> a -> [a] 

newtype EdgeListGraph a = EdgeListGraph [(a, a)]

instance GraphStructure EdgeListGraph where
  getNeighbors (EdgeListGraph edges) node = [y | (x, y) <- edges, x == node]

newtype AdjacencyListGraph a = AdjacencyListGraph (M.Map a [a])

instance GraphStructure AdjacencyListGraph where
  getNeighbors (AdjacencyListGraph adjList) node = M.findWithDefault [] node adjList

graphTraversal :: (GraphStructure g, Ord a) => ([a] -> [a] -> [a]) -> [a] -> g a -> [a] -> [a]
graphTraversal strategy [] _ visited = visited
graphTraversal strategy (current:queue) graph visited
  | current `elem` visited = graphTraversal strategy queue graph visited
  | otherwise = graphTraversal strategy (strategy queue (getNeighbors graph current)) graph (visited ++ [current])

breadthFirstSearch :: (GraphStructure g, Ord a) => g a -> a -> [a]
breadthFirstSearch graph start = graphTraversal (++) [start] graph []

depthFirstSearch :: (GraphStructure g, Ord a) => g a -> a -> [a]
depthFirstSearch graph start = graphTraversal (flip (++)) [start] graph []
