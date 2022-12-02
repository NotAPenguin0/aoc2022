import System.IO
import Data.List.Split
import Data.List

data Action = Rock | Paper | Scissors
    deriving (Show, Eq)

data Outcome = Win | Draw | Lose

readAction :: String -> Action
readAction "X" = Rock
readAction "Y" = Paper
readAction "Z" = Scissors
readAction "A" = Rock
readAction "B" = Paper
readAction "C" = Scissors

value :: Action -> Int
value Rock = 1
value Paper = 2
value Scissors = 3

outcomeValue :: Outcome -> Int
outcomeValue Win = 6
outcomeValue Lose = 0
outcomeValue Draw = 3

-- opponent, you
outcome :: (Action, Action) -> Int
outcome (Rock, Paper) = 6
outcome (Paper, Scissors) = 6
outcome (Scissors, Rock) = 6
outcome (x, y) 
    | x == y = 3
    | otherwise = 0

score :: (Action, Action) -> Int
score (opp, you) = value you + outcome (opp, you)

part1 :: [(Action, Action)] -> Int
part1 = foldr (+) 0 . map (score)

-- first map second Action to outcome because puzzle authors are doing the funny
mapOutcome :: [(Action, Action)] -> [(Action, Outcome)]
mapOutcome = map (\(x, y) -> (x, toOutcome y))
    where 
        toOutcome :: Action -> Outcome
        toOutcome Rock = Lose
        toOutcome Paper = Draw
        toOutcome Scissors = Win

toPlay :: Action -> Outcome -> Action
toPlay x Draw = x
toPlay Rock Win = Paper
toPlay Paper Win = Scissors
toPlay Scissors Win = Rock
toPlay Rock Lose = Scissors
toPlay Paper Lose = Rock
toPlay Scissors Lose = Paper

score' :: (Action, Outcome) -> Int
score' (a, o) = (outcomeValue o) + (value $ toPlay a o)

part2 :: [(Action, Action)] -> Int
part2 = foldr (+) 0 . map (score') . mapOutcome

solve :: [(Action, Action)] -> (Int, Int)
solve input = (part1 input, part2 input)

toTuple :: [a] -> (a, a)
toTuple (x:(y:[])) = (x, y)
toTuple _ = error "Invalid number of elements"

apply :: (a -> b) -> (a, a) -> (b, b)
apply f (x, y) = (f x, f y)

parse :: String -> [(Action, Action)]
parse = map (apply readAction . toTuple . splitOn " ") . splitOn "\n"

main = openFile "input/day2.txt" ReadMode >>= hGetContents >>= print . solve . parse
