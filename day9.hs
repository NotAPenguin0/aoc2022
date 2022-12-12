import Aoc
import Data.List
import System.IO
import Data.List.Split
import qualified Data.Set as S

data Direction = U | D | L | R 
    deriving (Eq, Show)
data Instruction = Instruction Direction Int
    deriving (Eq, Show)

data Pos = Pos Int Int
    deriving (Eq, Show, Ord)

moveHead :: Pos -> Direction -> Pos
moveHead (Pos x y) R = Pos (x+1) y
moveHead (Pos x y) L = Pos (x-1) y
moveHead (Pos x y) U = Pos x (y+1)
moveHead (Pos x y) D = Pos x (y-1)

adjacent :: Pos -> Pos -> Bool
adjacent (Pos x1 y1) (Pos x2 y2) = (abs $ x1 - x2) <= 1 && (abs $ y1 - y2) <= 1

diffToHead :: Pos -> Pos -> (Int, Int)
diffToHead (Pos x1 y1) (Pos x2 y2) = (signum $ x1 - x2, signum $ y1 - y2)

moveTail :: Pos -> Pos -> Pos
moveTail h@(Pos hx hy) t@(Pos tx ty) 
    -- overlapping, don't move
    | h == t = t
    -- adjacent, don't move
    | adjacent h t = t
    -- only different in one direction, move in that direction
    | hx == tx = Pos tx $ ty + (snd $ diffToHead h t)
    | hy == ty = Pos (tx + (fst $ diffToHead h t)) ty
    -- different in both directions, move in x/y plane
    | otherwise = let (dx, dy) = diffToHead h t in Pos (tx + dx) (ty + dy)

simulateStep :: S.Set Pos -> Pos -> Pos -> Instruction -> (Pos, Pos, S.Set Pos)
simulateStep visited h t (Instruction _ 0) = (h, t, visited)
simulateStep visited h@(Pos hx hy) t@(Pos tx ty) (Instruction dir n)
    = simulateStep newVisited newHead newTail (Instruction dir (n-1))
    where
        newHead = moveHead h dir
        newTail = moveTail newHead t
        newVisited = S.insert newTail visited

-- simulate the rope
simulate :: S.Set Pos -> Pos -> Pos -> [Instruction] -> S.Set Pos
simulate visited _ _ [] = visited
simulate visited h t (i:is) = simulate newVisited newHead newTail is
    where 
        (newHead, newTail, newVisited) = simulateStep visited h t i

part1 :: [Instruction] -> Int
part1 = S.size . simulate (S.fromList [Pos 0 0]) (Pos 0 0) (Pos 0 0)

-- for each movement step:
-- 1. move tail(0) towards head
-- 2. move tail(1) towards tail(0)

moveTails :: [Pos] -> [Pos]
moveTails [t] = []
-- move t towards h, then move ts towards t
moveTails (h:t:ts) = movedTail:(moveTails (movedTail:ts))
    where 
        movedTail = moveTail h t

simulateStep' :: S.Set Pos -> Pos -> [Pos] -> Instruction -> (Pos, [Pos], S.Set Pos)
simulateStep' visited h t (Instruction _ 0) = (h, t, visited)
simulateStep' visited h ts (Instruction dir n) 
    = simulateStep' newVisited newHead newTails (Instruction dir (n-1))
    where 
        newHead = moveHead h dir
        newTails = moveTails (newHead:ts)
        newVisited = S.insert (last newTails) visited

simulate' :: S.Set Pos -> Pos -> [Pos] -> [Instruction] -> S.Set Pos
simulate' visited _ _ [] = visited
simulate' visited h t (i:is) = simulate' newVisited newHead newTail is
    where 
        (newHead, newTail, newVisited) = simulateStep' visited h t i

part2 :: [Instruction] -> Int
part2 = S.size . simulate' (S.fromList [Pos 0 0]) (Pos 0 0) (replicate 9 (Pos 0 0))

readDir :: Char -> Direction 
readDir 'U' = U
readDir 'D' = D
readDir 'L' = L 
readDir 'R' = R
 
readInstr :: String -> Instruction
readInstr a = 
    let (p1:p2:[]) = splitOn " " a in 
    Instruction (readDir $ head p1) $ read p2

parse :: String -> [Instruction]
parse = map (readInstr) . lines

main :: IO ()
main = 
    openFile "input/day9.txt" ReadMode
    >>= hGetContents 
    >>= print . solve part1 part2 . parse