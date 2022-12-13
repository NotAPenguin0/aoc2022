module Dijkstra where

import qualified Data.Map as M
import Data.List
import Data.Maybe

type DistanceMap n = M.Map n Int
type Path n = M.Map n n

_initDistances :: (Ord n) => [n] -> DistanceMap n
_initDistances = foldr ((flip M.insert) maxBound) M.empty

_setDistance :: (Ord n) => Int -> n -> DistanceMap n -> DistanceMap n
_setDistance dist = M.update (const $ Just dist)

_getDistance :: (Ord n) => n -> DistanceMap n -> Int
_getDistance node = maybe (maxBound) id . M.lookup node

_setPrev :: (Ord n) => n -> n -> Path n -> Path n
_setPrev prev = M.alter (const $ Just prev)

_forceMaybe :: Maybe a -> a
_forceMaybe (Just x) = x
_forceMaybe Nothing = error "Maybe was Nothing"

_getPrev :: (Ord n) => n -> Path n -> n 
_getPrev node = _forceMaybe . M.lookup node

_hasPrev :: (Ord n) => n -> Path n -> Bool
_hasPrev node = isJust . M.lookup node

_getMinNode :: (Ord n) => DistanceMap n -> [n] -> n
_getMinNode dist = minimumBy (\n1 n2 -> compare (_getDistance n1 dist) (_getDistance n2 dist)) 

_updateDistMap :: (Ord n) => Int -> Int -> n -> DistanceMap n -> DistanceMap n
_updateDistMap old new node map 
    | old < new = map
    | otherwise = _setDistance new node map

_updatePath :: (Ord n) => Int -> Int -> n -> n -> Path n -> Path n
_updatePath old new node prev path
    | old < new = path
    | otherwise = _setPrev prev node path 

_updateNodes :: (Ord n) => Int -> n -> [n] -> DistanceMap n -> Path n -> (DistanceMap n, Path n)
_updateNodes alt u [] dist path = (dist, path)
_updateNodes alt u (n:ns) dist path = _updateNodes alt u ns newDist newPath
    where
        oldDist = _getDistance n dist
        newDist = _updateDistMap oldDist alt n dist
        newPath = _updatePath oldDist alt n u path


dijkstra' :: (Eq n, Ord n) => (n -> [n]) -> n -> [n] -> DistanceMap n -> Path n -> Path n
-- base case, node queue is empty.
dijkstra' _ _ [] _ p = p
dijkstra' getNeighbors target q dist path 
    -- | target == u = path -- terminate search, we found the shortest path. No early termination!!
    | otherwise = dijkstra' (getNeighbors) target newQ newDist newPath
    where 
        -- u <- vertex with minimum distance to target
        u = _getMinNode dist q
        -- remove u from node list
        newQ = filter (/=u) q
        -- for each neighbour of u that is still a member of the queue
        neighbors = filter ((flip elem) newQ) $ getNeighbors u
        -- assume weight of 1 for each edge, we can configure this later if needed
        alternativeDist = 1 + _getDistance u dist 
        -- for each neighbour, if this alternative distance is shorter than the original distance,
        -- update it and set the 'prev' pointer.
        -- Then, repeat the algorithm. We can simply call dijkstra' again.
        (newDist, newPath) = _updateNodes alternativeDist u neighbors dist path


-- dijkstra algorithm for computing the shortest path.
-- n is the node type
-- given a function n -> [n] for generating the neighbors of a given node
-- a start and target node,
-- and a list of nodes [n] in the graph, this algorithm will
-- return the shortest path from the start node to the target node.
-- the path is a map with the previous node from each node, so you can follow it backwards to 
-- construct a path (or do whatever, I don't care.)
dijkstra :: (Ord n) => (n -> [n]) -> n -> n -> [n] -> Path n 
dijkstra getNeighbors source target nodes = dijkstra' getNeighbors target nodes dist prev
    where 
        dist = _setDistance 0 source $ _initDistances nodes
        prev = M.empty -- god bless type inferrence?
        queue = nodes

hasPath :: (Ord n) => n -> n -> Path n -> Bool
hasPath source target path 
    | source == target = True
    | otherwise = (_hasPrev target path) && (hasPath source (_getPrev target path) path)

reconstructPath :: (Ord n) => n -> n -> Path n -> [n]
reconstructPath source target path 
    | source == target = [target]
    | otherwise = (reconstructPath source (_getPrev target path) path) ++ [target]