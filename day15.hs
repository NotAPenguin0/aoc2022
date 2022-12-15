import Aoc
import Data.List
import Data.List.Split
import Data.Char
import System.IO
import Data.Char
import Data.Maybe
import Debug.Trace 

data Pos = Pos Int Int
    deriving (Eq, Show)
data Sensor = Sensor Pos Pos -- sensor position/beacon position
    deriving (Show)

manhattan :: Pos -> Pos -> Int
manhattan (Pos x1 y1) (Pos x2 y2) = (abs (x1 - x2)) + abs (y1 - y2)

sensorRange :: Sensor -> Int
sensorRange (Sensor loc beacon) = manhattan loc beacon

-- Strategy:
-- Each sensor covers a specific range on the asked row.
-- If we can compute this range for each sensor (this is fairly simple),
-- we can then concatenate the overlapping areas of these ranges,
-- and simply count how large they are.

-- 2. Merge ranges
mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges = foldl (flip merge) [] . sort
    where
        merge :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
        merge elem [] = [elem]
        merge elem@(start, end) (top@(topstart, topend):is) 
             | start == topend + 1 = merge (topstart, end) $ sort is
            | start <= topend && end <= topend = top:is
            | start <= topend && end >= topend = merge (topstart, end) $ sort is
            | otherwise = elem:(top:is)

-- 1. Compute the coverage of each sensor (inclusive!). Returns Nothing if there is no coverage at all
sensorCoverage :: Int -> Sensor -> Maybe (Int, Int)
sensorCoverage row sensor@(Sensor loc@(Pos x y) beacon@(Pos bx by)) = coverage
    where
        range = sensorRange sensor
        diff = abs $ y - row
        horizontalRange = range - diff
        coverage = if (horizontalRange < 0) then Nothing else Just (x - horizontalRange, x + horizontalRange)

insideRange :: Int -> (Int, Int) -> Bool
insideRange x (start, end) = x >= start && x <= end

insideAnyRange :: Int -> [(Int, Int)] -> Bool
insideAnyRange x = any (insideRange x) 

part1 :: [Sensor] -> Int
part1 sensors = numCovered - numObjects
    where
        yLevel = 2000000
        ranges = map (maybe (0,0) id) $ filter (isJust) $ map (sensorCoverage yLevel) sensors
        merged = mergeRanges ranges
        numCovered = foldl (\acc (start, end) -> acc + (end - start) + 1) 0 merged
        -- remove all beacon and sensor spots here
        beaconLocs = nub $ map (\(Sensor _ b) -> b) sensors
        sensorLocs = nub $ map (\(Sensor s _) -> s) sensors
        objectLocs = nub $ beaconLocs ++ sensorLocs
        objects = filter (\(Pos x y) -> (y == yLevel && insideAnyRange x merged)) objectLocs
        numObjects = length objects

limitRanges :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
limitRanges minVal maxVal = mergeRanges . map (\(start, end) -> (max minVal start, min maxVal end)) . filter ((>=minVal) . snd) . filter ((<=maxVal) . fst)

limitedRangesAt :: [Sensor] -> Int -> Int -> [(Int, Int)]
limitedRangesAt sensors y maxY = limitRanges 0 maxY $ map (maybe (0,0) id) $ filter (isJust) $ map (sensorCoverage y) sensors

part2 :: [Sensor] -> Int
part2 sensors = x * 4000000 + y
    where
        maxCoord = 4000000
        correctRow@(xranges, y) = head $ filter ((/=1) . length . fst) $ zip [limitedRangesAt sensors y maxCoord | y <- [1..maxCoord]] [1..maxCoord]
        -- we found the correct row, now we need to compute the coordinate
        xranges' = sort xranges
        x = 1 + snd (head xranges') 
        

readSensor :: String -> Sensor
readSensor s = Sensor (Pos (nums !! 0) (nums !! 1)) (Pos (nums !! 2) (nums !! 3))
    where 
        data' = filter (\c -> c=='-' || c=='=' || isDigit c) s
        nums = map (read) $ filter (/="") (splitOn "=" data')


parse :: String -> [Sensor]
parse = map (readSensor) . lines

main :: IO () 
main = openFile "input/day15.txt" ReadMode
       >>= hGetContents
       >>= print . solve part1 part2 . parse