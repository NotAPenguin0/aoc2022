import Aoc
import System.IO
import Data.List 
import Data.List.Split
import Data.Char

type Box = Char
type Stack = [Box]
data Instruction = Instruction Int Int Int
    deriving (Show)

getTop :: [Stack] -> Int -> Box
getTop (stack:stacks) 1 = head stack
getTop (stack:stacks) i = getTop stacks (i-1)

popTop :: [Stack] -> Int -> [Stack]
popTop (stack:stacks) 1 = ((drop 1 stack):stacks)
popTop (stack:stacks) i = stack:(popTop stacks (i-1))

moveOne' :: [Stack] -> Int -> Box -> [Stack]
moveOne' (stack:stacks) 1 box = (box:stack):stacks
moveOne' (stack:stacks) i box = stack:(moveOne' stacks (i-1) box)
moveOne' stacks i box = error (show stacks ++ show i ++ show box)

moveOne :: [Stack] -> Int -> Int -> [Stack]
moveOne stacks from to = moveOne' stacks to (getTop stacks from)

move :: [Stack] -> Instruction -> [Stack]
move stacks (Instruction 0 _ _) = stacks
move stacks (Instruction amt from to) = move (popTop (moveOne stacks from to) from) (Instruction (amt-1) from to)

part1 :: ([Stack], [Instruction]) -> String
part1 (stacks, []) = map (head) stacks
part1 (stacks, (i:is)) = part1 (move stacks i, is)
 
-- part 2: can move multiple stacks at once

getTop2 :: [Stack] -> Int -> Int -> [Box]
getTop2 (stack:stacks) 1 n = take n stack
getTop2 (stack:stacks) i n = getTop2 stacks (i-1) n 

popTop2 :: [Stack] -> Int -> Int -> [Stack]
popTop2 (stack:stacks) 1 n = ((drop n stack):stacks)
popTop2 (stack:stacks) i n = stack:(popTop2 stacks (i-1) n)

moveN' :: [Stack] -> Int -> [Box] -> [Stack]
moveN' (stack:stacks) 1 boxes = (boxes++stack):stacks
moveN' (stack:stacks) i boxes = stack:(moveN' stacks (i-1) boxes)

moveN :: [Stack] -> Int -> Int -> Int -> [Stack]
moveN stacks amt from to = moveN' stacks to (getTop2 stacks from amt)

move2 :: [Stack] -> Instruction -> [Stack]
move2 stacks (Instruction amt from to) = popTop2 (moveN stacks amt from to) from amt

part2 :: ([Stack], [Instruction]) -> [Box]
part2 (stacks, []) = map (head) stacks
part2 (stacks, (i:is)) = part2 (move2 stacks i, is)

mkInstruction :: [Int] -> Instruction 
mkInstruction (x:y:z:[]) = Instruction x y z
mkInstruction _ = error ""

parseStacks :: String -> [Stack]
parseStacks = map (init . filter (not . (==' ')))
              . filter (not . all (==' ')) 
              . transpose 
              . map (replace ']' ' ' . replace '[' ' ') 
              . lines 

parseInstructions :: String -> [Instruction]
parseInstructions = map mkInstruction
                    . (map . map) (read)
                    . map (filter (all isDigit))
                    . map (splitOn " ")
                    . lines

parse :: String -> ([Stack], [Instruction])
parse input = (parseStacks (fst parts), parseInstructions (snd parts))
    where 
        parts = first2 $ splitOn "\n\n" input
    
main :: IO ()
main = openFile "input/day5.txt" ReadMode
       >>= hGetContents
       >>= print . solve part1 part2 . parse