import Aoc
import qualified Dijkstra 
import System.IO
import Data.List
import Data.List.Split
import Data.Char


data Point = Point Int Int
    deriving (Eq, Ord, Show)

isStart :: ((Bool, Bool, Int), Point) -> Bool
isStart ((True, _, _), _) = True
isStart _ = False

isEnd :: ((Bool, Bool, Int), Point) -> Bool
isEnd ((_, True, _), _) = True
isEnd _ = False

isStart2 :: ((Bool, Bool, Int), Point) -> Bool
isStart2 ((_, _, 0), _) = True
isStart2 _ = False

getCoord :: ((Bool, Bool, Int), Point) -> Point 
getCoord = snd

getHeight :: (Bool, Bool, Int) -> Int
getHeight (_, _, h) = h

getPointHeight :: [[(Bool, Bool, Int)]] -> Point -> Int
getPointHeight tiles (Point x y) = getHeight $ (tiles !! y) !! x

inBounds :: Int -> Int -> Point -> Bool
inBounds maxX maxY (Point x y) = (x >= 0) && (y >= 0) && (x < maxX) && (y < maxY)

getValidNeighbours :: [[(Bool, Bool, Int)]] -> Point -> [Point]
getValidNeighbours tiles (Point x y) = candidates''
    where
        maxHeight = 1 + (getHeight $ (tiles !! y) !! x)
        candidates = [Point (x+1) y, Point (x-1) y, Point x (y+1), Point x (y-1)]
        maxX = length (head tiles)
        maxY = length tiles
        candidates' = filter (inBounds maxX maxY) candidates
        candidates'' = filter ((<=maxHeight) . getPointHeight tiles) candidates'

getValidNeighboursReverse :: [[(Bool, Bool, Int)]] -> Point -> [Point]
getValidNeighboursReverse tiles (Point x y) = candidates''
    where 
        -- reverse: the *previous* node can be as high as we want, but can only be *one* lower than us
        minHeight = (getHeight $ (tiles !! y) !! x) - 1
        candidates = [Point (x+1) y, Point (x-1) y, Point x (y+1), Point x (y-1)]
        maxX = length (head tiles)
        maxY = length tiles
        candidates' = filter (inBounds maxX maxY) candidates
        candidates'' = filter ((>=minHeight) . getPointHeight tiles) candidates'

part1 :: [[(Bool, Bool, Int)]] -> Int
part1 tiles = (length realPath) - 1 -- start node is counted
    where 
        maxX = length (head tiles)
        maxY = length tiles
        allCoords = [[Point x y | x <- [0..(maxX-1)]] | y <- [0..(maxY-1)]]
        withCoords = (zipWith zip) tiles allCoords
        startCoord = getCoord $ head $ filter (isStart) $ concat withCoords
        endCoord = getCoord $ head $ filter (isEnd) $ concat withCoords
        pathMap = Dijkstra.dijkstra (getValidNeighbours tiles) startCoord endCoord $ concat allCoords 
        realPath = Dijkstra.reconstructPath startCoord endCoord pathMap

part2 :: [[(Bool, Bool, Int)]] -> Int
part2 tiles = (length shortest) - 1
    where
        maxX = length (head tiles)
        maxY = length tiles
        allCoords = [[Point x y | x <- [0..(maxX-1)]] | y <- [0..(maxY-1)]]
        withCoords = (zipWith zip) tiles allCoords
        startCoords = map (getCoord) $ filter (isStart2) $ concat withCoords
        initialStart = getCoord $ head $ filter (isStart) $ concat withCoords
        endCoord = getCoord $ head $ filter (isEnd) $ concat withCoords
        -- run Dijkstra from finish to our initial start coordinate (because there is guaranteed to be a path).
        -- With this we obtain a pathmap from all nodes to other nodes (because no early termination)
        -- We can use this to reconstruct paths if they exist, and find the shortest one.
        -- note that not all combinations will have a valid path. we'll need to add a hasPath function
        pathMap = Dijkstra.dijkstra (getValidNeighboursReverse tiles) endCoord (initialStart) $ concat allCoords
        validStarts = filter (\s -> Dijkstra.hasPath endCoord s pathMap) startCoords
        paths = map (\s -> Dijkstra.reconstructPath endCoord s pathMap) validStarts
        shortest = minimumBy (\p1 p2 -> compare (length p1) (length p2)) paths


readHeight :: Char -> Int
readHeight c = (ord c - ord 'a')

readTile :: Char -> (Bool, Bool, Int)
readTile 'S' = (True, False, 0)
readTile 'E' = (False, True, 25)
readTile c = (False, False, readHeight c)

parse :: String -> [[(Bool, Bool, Int)]] -- isStart, isEnd, heights
parse = (map . map) (readTile) . lines

main :: IO () 
main = openFile "input/day12.txt" ReadMode
       >>= hGetContents 
       >>= print . solve part1 part2 . parse