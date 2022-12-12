import Aoc
import System.IO
import Data.List
import Data.Char

visible :: Int -> Int -> [[Int]] -> Bool
-- edges of the forest
visible 0 _ _ = True
visible _ 0 _ = True
visible x y forest 
    | x == length (head forest) = True
    | y == length forest = True
    | otherwise = 
        -- x direction
        all (<tree) (take x row) || all (<tree) (drop (x+1) row)
        -- y direction
        || all (<tree) (take y col) || all (<tree) (drop (y+1) col)
    where
        tree = (forest !! y) !! x
        row = (forest !! y)
        col = (transpose forest) !! x

visibleInDirection :: Int -> [Int] -> Int
visibleInDirection tree sight = min (length sight) $ ((+1) . length . takeWhile (<tree)) sight

scenicScore :: Int -> Int -> [[Int]] -> Int
scenicScore x y forest = product $ map (\sight -> visibleInDirection tree sight) [up, down, left, right]
    where
        tree = (forest !! y) !! x
        row = (forest !! y)
        col = (transpose forest) !! x
        up = reverse $ take y col
        down = drop (y+1) col
        left = reverse $ take x row
        right = drop (x+1) row


part1 :: [[Int]] -> Int
part1 forest = (length . filter (==True) . concat . (map . map) (uncurry $ \x y -> visible x y forest)) [[(x, y) | x <- [0..(length $ head forest)-1]] | y <- [0..(length forest)-1]]

part2 :: [[Int]] -> Int
part2 forest = (maximum . concat . (map . map) (uncurry $ \x y -> scenicScore x y forest)) [[(x, y) | x <- [0..(length $ head forest)-1]] | y <- [0..(length forest)-1]]

parse :: String -> [[Int]]
parse = (map . map) (digitToInt) . lines

main :: IO ()
main = openFile "input/day8.txt" ReadMode >>= hGetContents >>= print . solve part1 part2 . parse