{-# LANGUAGE BangPatterns #-}
import Aoc
import Control.Lens -- cabal install lens
import System.IO
import Data.List
import Data.List.Split 
import Data.Maybe
import Dijkstra
import Debug.Trace
import Text.Printf
import qualified Data.Map as M

data Tile = Air | Rock | Sand
    deriving (Eq)

data ScanElement = ScanElement Int Int
    deriving (Eq, Show)

instance Show Tile where
    show Air = "."
    show Rock = "#"
    show Sand = "O"

setTile :: Int -> Int -> Tile -> [[Tile]] -> [[Tile]]
setTile x y tile tiles = (element y .~ ((element x .~ tile) (tiles !! y))) tiles

getTile :: Int -> Int -> [[Tile]] -> Tile
getTile x y tiles = (tiles !! y) !! x

onLeftEdge :: (Int, Int) -> [[Tile]] -> Bool
onLeftEdge (0, _) _ = True
onLeftEdge (_, _) _ = False

onRightEdge :: (Int, Int) -> [[Tile]] -> Bool
onRightEdge (x, _) tiles = (x == (length $ head tiles) - 1)

belowBlocked :: (Int, Int) -> [[Tile]] -> Bool
belowBlocked (x, y) tiles = Air /= getTile x (y+1) tiles

rightBelowBlocked :: (Int, Int) -> [[Tile]] -> Bool
rightBelowBlocked (x, y) tiles = Air /= getTile (x+1) (y+1) tiles

leftBelowBlocked :: (Int, Int) -> [[Tile]] -> Bool
leftBelowBlocked (x, y) tiles = Air /= getTile (x-1) (y+1) tiles

intoAbyss :: (Int, Int) -> [[Tile]] -> Bool
intoAbyss pos@(x, y) tiles = (y == maxY - 1) || ((belowBlocked pos tiles) && (((onLeftEdge pos tiles) && (rightBelowBlocked pos tiles)) || (onRightEdge pos tiles) && (leftBelowBlocked pos tiles)))
    where
        maxY = length tiles

isBlocked :: (Int, Int) -> [[Tile]] -> Bool
isBlocked pos tiles 
    | onLeftEdge pos tiles = belowBlocked pos tiles && rightBelowBlocked pos tiles 
    | onRightEdge pos tiles = belowBlocked pos tiles && leftBelowBlocked pos tiles 
    | otherwise = belowBlocked pos tiles && leftBelowBlocked pos tiles && rightBelowBlocked pos tiles

moveSand :: (Int, Int) -> [[Tile]] -> (Int, Int)
moveSand pos@(x, y) tiles 
    | not $ belowBlocked pos tiles = (x, y+1)
    | onLeftEdge pos tiles = (x+1, y+1)
    | onRightEdge pos tiles = (x-1, y+1)
    | not $ leftBelowBlocked pos tiles = (x-1, y+1)
    | not $ rightBelowBlocked pos tiles = (x+1, y+1) 
    | otherwise = error "!"

dropSand' :: (Int, Int) -> [[Tile]] -> Maybe (Int, Int)
dropSand' pos@(x, y) tiles 
    | intoAbyss pos tiles = Nothing
    | isBlocked pos tiles = Just pos
    | otherwise = dropSand' (moveSand pos tiles) tiles

dropSand :: Int -> [[Tile]] -> Maybe (Int, Int)
dropSand sandX tiles = dropSand' (sandX, 0) tiles

setDroppedSand :: Maybe (Int, Int) -> [[Tile]] -> [[Tile]] 
setDroppedSand Nothing _ = error ""
setDroppedSand (Just (x, y)) tiles = setTile x y Sand tiles

dropSandWhile :: Int -> [[Tile]] -> Int
dropSandWhile spawnX tiles 
    | isNothing drop = 0
    | isJust drop = 1 + (dropSandWhile spawnX $ setDroppedSand drop tiles)
    where
        drop = dropSand spawnX tiles

part1 :: [[Tile]] -> Int -> Int
part1 tiles sandX = dropSandWhile sandX tiles

evens :: [Int] 
evens = map (*2) [0..]

odds :: [Int]
odds = map (+1) evens

pyramidBase :: Int -> Int
pyramidBase levels = (odds !! (levels-1))

-- our pyramid is centered around the spawn X coordinate

pyramid :: Int -> Int
pyramid 1 = 1
pyramid base = base + (pyramid (base - 2))

cappedPyramid :: Int -> Int -> Int
cappedPyramid base cap = (pyramid base) - (pyramid cap)

countRock :: [[Tile]] -> Int
countRock = length . filter (==Rock) . concat

getTileNeighbours :: [[Tile]] -> (Int, Int) -> [(Int, Int)]
getTileNeighbours tiles pos@(x, y)
    | y == 0 = []
    | x == 0 = (x-1, y-1):(filterAir [(x, y-1), (x+1, y-1)])
    | x == -1 = (x, y-1):(filterAir [(x+1, y-1)])
    | x == (maxX) = (x, y-1):(filterAir [(x-1, y-1)])
    | x == (maxX-1) = (x+1, y-1):(filterAir [(x, y-1), (x-1, y-1)])
    | otherwise = filterAir [(x, y-1), (x+1, y-1), (x-1, y-1)]
    where
        filterAir = filter ((==Air) . (flip $ uncurry getTile) tiles)
        maxX = length $ head tiles

airHasPath :: [(Int, Int)] -> [[Tile]] -> (Int, Int) -> (Int, Int) -> Bool
airHasPath nodes tilesExp spawn air = Dijkstra.hasPath air spawn pathMap
    where
        pathMap = Dijkstra.dijkstra (getTileNeighbours tilesExp) air spawn nodes

{-part2 :: [[Tile]] -> Int -> Int
part2 tiles sandX = numAirLeft
    -- dropping sand, part 2.
    -- in this case, we drop N full layers of sand in a pyramid shape, minus the contained blocks, minus the air gaps.
    -- we drop maxY+1 pyramid layers.
    where
        maxY = length tiles
        maxX = length $ head tiles
        sandPyramid = pyramid $ pyramidBase (maxY+1)
        -- 1. count rock inside the pyramid and remove it from the count
        rockTiles = countRock tiles

        -- 2. for each horizontal layer of rock at level y with width W
        --      there will be an upside down pyramid of air with base length (W-2)
        --      note that this pyramid is capped at the floor level (maxY+1)
        -- NOTE: this will remove the rock inside the upside down pyramids too!
        -- Solution: DELETE all rock inside pyramids, work top down.
        -- This means we have to count the rock tiles AFTER counting air pyramids.

        -- New idea:
        -- 1. Initial solution = full pyramid
        -- 2. Remove all rocks
        -- 3. From each air tile, do a BFS/dijkstra to the start, to see if it is reachable.
        --      we might be able to reuse our old dijkstra for this!
        -- 4. Solution minus one for each air tile with no path
        
        -- For our search to work properly, we need to expand the grid by one
        -- tile, to allow for pathing outside the initial grid.
        tiles' = tiles ++ [replicate (maxX) Air]
        dijkstraNodes = [(x, y) | x <- [0..(maxX+1)], y <- [0..(maxY+1)]]
        -- Compute path from end point (the sand spawn locaation) to a "good" initial start position
        -- One such good location is the bottom left corner, as it always has a path
        end = (sandX+1, 0) -- +1 because we shifted all coordinates by adding the extra nodes on the edges
        start = (0, maxY+1)
        pathMap = Dijkstra.dijkstra (getTileNeighbours tiles') start end dijkstraNodes
        -- take all air blocks, check if they have a path, and filter
        allAirCoords = filter ((==Air) . (flip $ uncurry getTile) tiles) [(x, y) | x <- [0..(maxX-1)], y <- [0..(maxY-1)]]
        -- numAirLeft = length $ filter (airHasPath dijkstraNodes expandedTiles end) allAirCoords
        numAirLeft = length $ filter (\s@(x, y) -> Dijkstra.hasPath (x+1,y) end pathMap) allAirCoords
-}

type Cave = M.Map (Int, Int) Tile

setTile2 :: Int -> Int -> Tile -> Cave -> Cave
setTile2 x y tile tiles = M.alter (const $ Just tile) (x, y) tiles

getTile2 :: Int -> Int -> Cave -> Tile
getTile2 x y tiles = M.findWithDefault Air (x, y) tiles

belowBlocked2 :: (Int, Int) -> Cave -> Bool
belowBlocked2 (x, y) tiles = Air /= getTile2 x (y+1) tiles

rightBelowBlocked2 :: (Int, Int) -> Cave-> Bool
rightBelowBlocked2 (x, y) tiles = Air /= getTile2 (x+1) (y+1) tiles

leftBelowBlocked2 :: (Int, Int) -> Cave -> Bool
leftBelowBlocked2 (x, y) tiles = Air /= getTile2 (x-1) (y+1) tiles

isBlocked2 :: (Int, Int) -> Cave -> Bool
isBlocked2 pos tiles = belowBlocked2 pos tiles && leftBelowBlocked2 pos tiles && rightBelowBlocked2 pos tiles

moveSand2 :: (Int, Int) -> Cave -> (Int, Int)
moveSand2 pos@(x, y) tiles 
    | not $ belowBlocked2 pos tiles = (x, y+1)
    | not $ leftBelowBlocked2 pos tiles = (x-1, y+1)
    | not $ rightBelowBlocked2 pos tiles = (x+1, y+1) 
    | otherwise = error "!"

onFloor :: Int -> (Int, Int) -> Bool
onFloor floorLvl pos@(x, y) = y == floorLvl

dropSand2' :: Int -> (Int, Int) -> Cave -> [(Int, Int)]
dropSand2' floorLvl pos@(x, y) tiles 
    | onFloor floorLvl pos = [pos] -- on the floor, we do nothing
    | isBlocked2 pos tiles = [pos] -- blocked, do nothing
    | otherwise = pos:(dropSand2' floorLvl (moveSand2 pos tiles) tiles) -- update, then move again

dropSand2 :: Int -> Int -> Cave -> [(Int, Int)]
dropSand2 sandX floorLvl tiles = dropSand2' floorLvl (sandX, 0) tiles

setDroppedSand2 :: (Int, Int) -> Cave -> Cave
setDroppedSand2 (x, y) tiles = setTile2 x y Sand tiles

-- We keep a list of 'history' of the sand.
-- this means, the full path the previous piece of sand took.

-- if we have the history, then the next drop location defined by:
-- finding the first non-blocked tile in the history (backwards, from end tile)
-- taking the best available step there

-- sand[x, y] = 1 + sand[x, y + 1] + sand [x - 1, y + 1] + sand[x + 1, y + 1]

sand :: Cave -> (Int, Int) -> (Cave, Int)
sand cave pos@(x, y)
    | Rock == getTile2 x y cave = (cave, 0)
    | Sand == getTile2 x y cave = (cave, 0)
    | otherwise = (cave'''', 1 + down + left + right)
        where 
            -- mark this tile as visited already, so we don't ever count it twice.
            cave' = setTile2 x y Sand cave
            (cave'', down) = sand cave' (x, y + 1)
            (cave''', left) = sand cave'' (x - 1, y + 1)
            (cave'''', right) = sand cave''' (x + 1, y + 1)

continueFromHistory :: [(Int, Int)] -> Int -> Int -> Cave -> (Cave, Int)
continueFromHistory [] _ _ tiles = (tiles, 0) -- no more unblocked tiles starting from the previous tile, we are done!
continueFromHistory (h:history) spawnX floorLvl tiles = (newTiles', n + 1)
    where 
        -- 'h' has the previous unblocked tile.
        -- we try to move from there
        newHistory = dropSand2' floorLvl h tiles
        newSand@(x,y) = last newHistory 
        !_ = trace ("Found new sand: " ++ show (x,y)) $ newSand
        newTiles = setDroppedSand2 newSand tiles
        (newTiles', n) = dropSandWhile2' (reverse $ history ++ [newSand] ++ (reverse newHistory)) spawnX floorLvl newTiles


dropSandWhile2' :: [(Int, Int)] -> Int -> Int -> Cave -> (Cave, Int)
dropSandWhile2' history spawnX floorLvl tiles = continueFromHistory dropBlocked spawnX floorLvl tiles
    where 
        revHistory = reverse history
        dropBlocked = dropWhile (\p -> onFloor floorLvl p || isBlocked2 p tiles) revHistory


dropSandWhile2 :: Int -> Int -> Cave -> (Cave, Int)
dropSandWhile2 spawnX floorLvl tiles = (newTiles', n) -- we did one drop here, so increment one
    where
        dropHistory = dropSand2 floorLvl spawnX tiles
        drop = trace (show dropHistory) $ last dropHistory
        newTiles = setDroppedSand2 drop tiles
        (newTiles', n) = dropSandWhile2' dropHistory spawnX floorLvl newTiles

part2 :: [[Tile]] -> Int -> Int
part2 tiles sandX = snd $ sand tileMap' (sandX, 0)
    where 
        -- expand with a bottom layer of air, right above the ground.
        maxY = length tiles
        maxX = length $ head tiles
        tilesExpanded = tiles ++ [replicate maxX Air]
        -- [replicate (2*maxY-1) Rock]
        floorLvl = maxY
        -- convert tiles to map
        allCoords = [[(x, y) | x <- [0..(maxX-1)]] | y <- [0..maxY]]
        tilesWithCoords = concat $ Aoc.zip2D allCoords tilesExpanded
        tileMap = M.fromList tilesWithCoords
        floorXs = [(sandX-maxY-5)..(sandX+maxY+5)]
        floorTiles = map (\x -> ((x, floorLvl + 1), Rock)) floorXs
        tileMap' = M.union tileMap (M.fromList floorTiles) 

scanX :: ScanElement -> Int
scanX (ScanElement x _) = x

scanY :: ScanElement -> Int
scanY (ScanElement _ y) = y

setRock :: Int -> Int -> [[Tile]] -> [[Tile]]
setRock x y tiles = setTile x y Rock tiles

diffTowards :: (Int, Int) -> (Int, Int) -> (Int, Int)
diffTowards (x1, y1) (x2, y2) = (signum $ x2 - x1, signum $ y2 - y1)

makeLine :: ScanElement -> ScanElement -> [[Tile]] -> [[Tile]]
makeLine (ScanElement fromX fromY) (ScanElement toX toY) tiles
    | fromX == toX && fromY == toY = setRock fromX fromY tiles
    | otherwise = makeLine (ScanElement (fromX + dx) (fromY + dy)) (ScanElement toX toY) $ setRock fromX fromY tiles 
        where
            (dx, dy) = diffTowards (fromX, fromY) (toX, toY)

applyScan :: [ScanElement] -> [[Tile]] -> [[Tile]]
applyScan [x] tiles = tiles
applyScan (from:to:scans) tiles = applyScan (to:scans) $ makeLine from to tiles  

initTiles :: Int -> Int -> [[Tile]]
initTiles width height = replicate height (replicate width Air)

printMap :: [[Tile]] -> IO ()
printMap = putStr . concat . intercalate ["\n"] . (map . map) (show)

parse :: String -> ([[Tile]], Int) -- int is the sand start position
parse input = (foldr (applyScan) initialTiles remap, 500-smallestX)
    where 
        scan = (map (
                map (
                    uncurry ScanElement 
                    . first2 
                    . map (read) 
                    . splitOn ","
                    ) 
                . splitOn "->") 
               . lines) input
        -- find the smallest x coordinate so we can remap to a better system
        smallestX = minimum $ concat $ (map . map) (scanX) scan
        largestX = maximum $ concat $ (map . map) (scanX) scan
        largestY = maximum $ concat $ (map . map) (scanY) scan
        width = largestX - smallestX + 1
        initialTiles = initTiles width (largestY+1)
        remap = (map . map) (\(ScanElement x y) -> ScanElement (x-smallestX) y) scan

main :: IO ()
main = openFile "input/day14.txt" ReadMode
       >>= hGetContents
       >>= print . (uncurry part2) . parse

