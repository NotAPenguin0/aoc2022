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

type Cave = M.Map (Int, Int) Tile

setTile2 :: Int -> Int -> Tile -> Cave -> Cave
setTile2 x y tile tiles = M.alter (const $ Just tile) (x, y) tiles

getTile2 :: Int -> Int -> Cave -> Tile
getTile2 x y tiles = M.findWithDefault Air (x, y) tiles

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

