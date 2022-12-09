import Aoc
import System.IO
import Data.List
import Data.List.Split

data Interval = Interval Int Int
    deriving (Eq, Show)

contains :: Interval -> Interval -> Bool
contains (Interval s1 e1) (Interval s2 e2) = s1 <= s2 && e1 >= e2

partialContains :: Interval -> Interval -> Bool
partialContains (Interval s1 e1) (Interval s2 e2) = (s1 <= s2 && s2 <= e1)

oneContains :: (Interval -> Interval -> Bool) -> Interval -> Interval -> Bool
oneContains f x y = (uncurry (||)) $ (applyTuple2 (f) (flip f) x y)

part1 :: [(Interval, Interval)] -> Int
part1 = length . filter (uncurry (oneContains contains))

part2 :: [(Interval, Interval)] -> Int
part2 = length . filter (uncurry (oneContains partialContains))

parse :: String -> [(Interval, Interval)]
parse = (map . mapTuple) 
            ((uncurry Interval) . mapTuple (read) . first2 . splitOn "-") 
        . map (first2 . splitOn ",")
         . lines

main :: IO ()
main = 
    openFile "input/day4.txt" ReadMode
    >>= hGetContents
    >>= print . solve part1 part2 . parse