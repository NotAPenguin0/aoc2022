import System.IO
import Data.List.Split -- cabal install split
import Data.List
import Data.Ord

part1 :: [[Int]] -> Int
part1 = maximum . map (sum)

part2 :: [[Int]] -> Int
part2 = sum . take 3 . sortBy (comparing Down) . map sum

solve :: [[Int]] -> (Int, Int)
solve input = (part1 input, part2 input)

parse :: String -> [[Int]]
parse = (map . map) (read) . split (dropDelims $ whenElt (=="\n")) . split (keepDelimsR $ oneOf ['\n'])

main = openFile "input/day1.txt" ReadMode >>= hGetContents >>= print . solve . parse