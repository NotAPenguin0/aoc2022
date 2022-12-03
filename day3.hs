import System.IO
import Data.List
import Data.Char
import Data.List.Split
import Aoc

halves :: String -> (String, String)
halves l = first2 $ [take d, drop d] <*> [l]
    where 
        d = (length l) `div` 2

value :: Char -> Int
value x 
    | isLower x = (ord x) - (ord 'a') + 1
    | isUpper x = (ord x) - (ord 'A') + 27

part1 :: [String] -> Int
part1 = sum . map (value . head . (uncurry intersect) . halves)

part2 :: [String] -> Int
part2 = sum . map (value . head . (foldr1 intersect)) . chunksOf 3

parse :: String -> [String]
parse = lines

main :: IO ()
main = 
    openFile "input/day3.txt" ReadMode 
    >>= hGetContents 
    >>= print . solve part1 part2 . parse