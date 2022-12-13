{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
import NestedList

import System.IO
import Data.List
import Data.List.Split
import Data.List.Utils (replace) -- cabal install MissingH
import Data.Char
import Aoc

data Decision = Yes | Undecided | No
    deriving (Eq, Show)

(!) :: Decision -> Decision -> Decision
Undecided ! x = x
x ! _ = x

toOrdering :: Decision -> Ordering
toOrdering Yes = LT
toOrdering Undecided = EQ
toOrdering No = GT

order :: NestedList Int -> NestedList Int -> Decision
order (Item x) (Item y) 
    | x < y = Yes 
    | x == y = Undecided
    | x > y = No
order (List []) (List []) = Undecided
order (List []) (List _) = Yes -- left side may run out first
order (List (x:xs)) (List []) = No -- right side may not run out first
order (List (x:xs)) (List (y:ys)) = (order x y) ! order (List xs) (List ys)
order (Item x) (List ys) = order (List [Item x]) (List ys)
order (List xs) (Item y) = order (List xs) (List [Item y])

part1 :: [(NestedList Int, NestedList Int)] -> Int 
part1 = sum . map (fst) . filter ((==Yes) . snd) . map (\(i, s) -> (i, uncurry order s)) . zip [1..]

dividers :: [NestedList Int]
dividers = [List [List [Item 2]], List [List [Item 6]]]

part2 :: [NestedList Int] -> Int
part2 = product 
        . map (fst) 
        . filter ((flip elem) dividers . snd) 
        . zip [1..] 
        . sortBy (\l1 l2 -> toOrdering $ order l1 l2) 
        . (++dividers)

addItemConstructor :: String -> String
addItemConstructor [] = ""
addItemConstructor (',':(d:s))
    | isDigit d = ",Item " ++ [d] ++ (addItemConstructor s)
    | otherwise = "," ++ [d] ++ (addItemConstructor s)
addItemConstructor ('[':(d:s)) 
    | isDigit d = "[Item " ++ [d] ++ (addItemConstructor s)
    | otherwise = "[" ++ [d] ++ (addItemConstructor s)
addItemConstructor (' ':xs) = addItemConstructor xs -- skip spaces
addItemConstructor (x:xs) = x:(addItemConstructor xs)

readNestedList :: String -> NestedList Int
readNestedList = read . addItemConstructor . Data.List.Utils.replace "[" "List ["

parse :: String -> [(NestedList Int, NestedList Int)]
parse = map (first2 . map (readNestedList) . splitOn "\n")  
        . splitOn "\n\n"


parse2 :: String -> [NestedList Int]
parse2 = concat . map (map (readNestedList) . splitOn "\n")  
        . splitOn "\n\n"


main :: IO ()
main = openFile "input/day13.txt" ReadMode 
       >>= hGetContents
       >>= print . part1 . parse

main2 :: IO ()
main2 = openFile "input/day13.txt" ReadMode
        >>= hGetContents
        >>= print . part2 . parse2