module Aoc where


first2 :: [a] -> (a, a)
first2 (x:(y:ys)) = (x, y)
first2 _ = error "Invalid element count"

solve :: (a -> Int) -> (a -> Int) -> a -> (Int, Int)
solve p1 p2 input = first2 $ [p1, p2] <*> [input]