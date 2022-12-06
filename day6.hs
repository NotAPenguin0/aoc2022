import System.IO
import Aoc
import Data.List

unique :: (Eq a) => [a] -> Bool
unique l = (length $ nub l) == (length l)

isMarker :: Int -> (String, Int) -> (Int, Bool)
isMarker len (s, i)
    | length s /= len = (i, False)
    | unique s = (i, True)
    | otherwise = (i, False)

findMarker :: Int -> String -> Int
findMarker len input = (fst $ head $ filter (snd) $ [isMarker len $ (drop x $ take (x+len) input, x) | x <- [0..max]]) + len
    where
        max = length input

part1 :: String -> Int 
part1 = findMarker 4

part2 :: String -> Int
part2 = findMarker 14

parse :: String -> String
parse = id

main :: IO ()
main = openFile "input/day6.txt" ReadMode 
       >>= hGetContents
       >>= print . solve part1 part2 . parse 