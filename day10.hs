import Aoc
import System.IO
import Data.List
import Data.List.Split

data Instruction = Noop | Addx Int
    deriving (Eq, Show)

interestingCycles :: [Int]
interestingCycles = [20, 60, 100, 140, 180, 220]

-- simulates all instructions, returning a list of 
-- the register value at each cycle.
simulate :: Int -> [Instruction] -> [Int]
simulate x [] = []
simulate x (Noop:is) = x:(simulate x is)
simulate x ((Addx n):is) = x:(x:(simulate (x+n) is))

part1 :: [Instruction] -> Int
part1 = 
    sum 
    . map (\(c, v) -> c * v) 
    . filter ((flip elem) interestingCycles . fst) 
    . (zip [1..220]) 
    . simulate 1

isCovered :: Int -> Int -> Bool
isCovered cycle reg = abs (reg - (mod (cycle - 1) 40)) <= 1

renderChar :: Bool -> Char
renderChar True = '#'
renderChar False = ' '

-- one string for each row on the CRT, we can render manually from there
part2 :: [Instruction] -> [String]
part2 = map (++"\n") . chunksOf 40 . map (renderChar . uncurry isCovered) . zip [1..240] . simulate 1

readInstr :: [String] -> Instruction
readInstr ("noop":[]) = Noop
readInstr ("addx":n:[]) = Addx $ read n

parse :: String -> [Instruction]
parse = map (readInstr . words) . lines

main :: IO () 
main = openFile "input/day10.txt" ReadMode
       >>= hGetContents
       >>= print . solve part1 part2 . parse

prettyPart2 :: [String] -> IO ()
prettyPart2 [] = return ()
prettyPart2 (l:ls) = putStr (l) >> prettyPart2 ls

prettyPrintPart2 :: IO ()
prettyPrintPart2 = openFile "input/day10.txt" ReadMode
                   >>= hGetContents
                   >>= prettyPart2 . part2 . parse