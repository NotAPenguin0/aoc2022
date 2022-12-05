module Aoc where


first2 :: [a] -> (a, a)
first2 (x:(y:ys)) = (x, y)
first2 _ = error "Invalid element count"

solve :: (a -> b) -> (a -> c) -> a -> (b, c)
solve p1 p2 input = (p1 input, p2 input)

-- applies a function to a tuple of 2 variables
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

-- applies a tuple of 2 functions to 2 variables
applyTuple :: (a -> b) -> (c -> d) -> a -> c -> (b, d)
applyTuple f1 f2 x y = (f1 x, f2 y)

applyTuple2 :: (a -> b -> c) -> (a -> b -> d) -> a -> b -> (c, d)
applyTuple2 f1 f2 x y = (f1 x y, f2 x y)

-- transpose a matrix
--transpose :: [[a]] -> [[a]]
--transpose ([]:_) = []
--transpose x = (map head x) : transpose (map tail x)

replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x:xs)
    | x == a = (b:replace a b xs)
    | otherwise = (x:replace a b xs)