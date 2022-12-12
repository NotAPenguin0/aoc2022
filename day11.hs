import Aoc
import System.IO
import Data.List
import Data.List.Split
import Control.Monad
import Text.Printf
import Control.Lens
import Data.Ord


-- Monkey:
--  - monkey number
--  - items
--  - operation
--  - test
--  - true target
--  - false target
--  - total inspect actions
data Monkey = Monkey Int [Integer] (Integer -> Integer) Integer Int Int Integer

instance Show Monkey where
    show (Monkey idx items f test true false n) = 
        "Items: " ++ show items ++
        printf "\nOperation (old = 1) %d\nTest: Div by %d\nTrue:%d\nFalse:%d\nn:%d\n"
        (f 1) test true false n

readStartingItems :: String -> [Integer] 
readStartingItems = map (read) 
                    . splitOn "," 
                    . (flip (!!)) 1 
                    . splitOn ":"

readOperation :: String -> (Integer -> Integer)
readOperation "Operation:new=old*old" = join (*)
readOperation s 
    | isPrefixOf "Operation:new=old+" s = (+) (read ((splitOn "+" s) !! 1))
    | isPrefixOf "Operation:new=old*" s = (*) (read ((splitOn "*" s) !! 1))
    | otherwise = error s

readTest :: String -> Integer
readTest =
    read
    . (flip (!!)) 1 
    . splitOn "by"

readThrow :: String -> Int
readThrow = 
    read
    . (flip (!!)) 1
    . splitOn "monkey"


readMonkey :: (Int, String) -> Monkey
readMonkey (idx, s) = Monkey idx items (operation) test throwTrue throwFalse 0
    where
        parts = map (filter (/=' ')) $ lines s
        items = readStartingItems $ parts !! 1
        operation = readOperation $ parts !! 2
        test = readTest $ parts !! 3
        throwTrue = readThrow $ parts !! 4
        throwFalse = readThrow $ parts !! 5

parse :: String -> [Monkey]
parse = map (readMonkey) . (zip [0..]) . splitOn "\n\n"

doTurn :: Monkey -> [Monkey] -> [Monkey]
doTurn monke@(Monkey idx [] _ _ _ _ _) monkeys = newMonkeys
    where
        -- update monkey list
        newMonkeys = monkeys & element (idx) .~ monke
doTurn monke@(Monkey idx (i:is) op test tru fls numinsp) monkeys = 
    doTurn (Monkey idx is op test tru fls numinsp') newMonkeys
    where
        -- throw current item, then do turn on rest of the items
        -- monke inspects item
        newWorryLvl = op i
        numinsp' = numinsp + 1
        -- monke gets bored of item
        newWorryLvl' = newWorryLvl `div` 3
        -- monke decides where to throw item
        testVal = (mod newWorryLvl' test) == 0
        throwTarget = if testVal then tru else fls
        -- monke throws item
        (Monkey idx' items' op' test' tru' fls' numinsp'') = monkeys !! (throwTarget)
        targetMonke = Monkey idx' (items'++[newWorryLvl']) op' test' tru' fls' numinsp''
        newMonkeys = monkeys & element (idx') .~ targetMonke


doRound' :: Int -> [Monkey] -> [Monkey]
doRound' 0 monkeys = monkeys
doRound' cur monkeys = doRound' (cur-1) newMonkeys
    where
        idx = (length monkeys) - cur
        monkey = monkeys !! idx
        newMonkeys = doTurn monkey monkeys

doRound :: [Monkey] -> [Monkey]
doRound monkeys = doRound' (length monkeys) monkeys

getNumInspections :: Monkey -> Integer
getNumInspections (Monkey _ _ _ _ _ _ n) = n

monkeyBusiness :: [Integer] -> Integer
monkeyBusiness (x:(y:[])) = x * y

part1 :: [Monkey] -> Integer
part1 = monkeyBusiness 
        . take 2 
        . sortOn (Down) 
        . map (getNumInspections) 
        . (flip (!!)) 20 
        . iterate doRound

doTurn2 :: Integer -> Monkey -> [Monkey] -> [Monkey]
doTurn2 _ monke@(Monkey idx [] _ _ _ _ _) monkeys = newMonkeys
    where
        -- update monkey list
        newMonkeys = monkeys & element (idx) .~ monke
doTurn2 maxTest monke@(Monkey idx (i:is) op test tru fls numinsp) monkeys = 
    doTurn2 maxTest (Monkey idx is op test tru fls numinsp') newMonkeys
    where
        -- throw current item, then do turn on rest of the items
        -- monke inspects item
        newWorryLvl = op i
        numinsp' = numinsp + 1
        -- part 2: monke gets bored but your worry level doesnt decrease!
        newWorryLvl' = mod newWorryLvl maxTest
        -- monke decides where to throw item
        testVal = (mod newWorryLvl' test) == 0
        throwTarget = if testVal then tru else fls
        -- monke throws item
        (Monkey idx' items' op' test' tru' fls' numinsp'') = monkeys !! (throwTarget)
        targetMonke = Monkey idx' (items'++[newWorryLvl']) op' test' tru' fls' numinsp''
        newMonkeys = monkeys & element (idx') .~ targetMonke


doRound2' :: Integer -> Int -> [Monkey] -> [Monkey]
doRound2' maxTest 0 monkeys = monkeys
doRound2' maxTest cur monkeys = doRound2' maxTest (cur-1) newMonkeys
    where
        idx = (length monkeys) - cur
        monkey = monkeys !! idx
        newMonkeys = doTurn2 maxTest monkey monkeys

getTestVal :: Monkey -> Integer 
getTestVal (Monkey _ _ _ t _ _ _) = t

doRound2 :: [Monkey] -> [Monkey]
doRound2 monkeys = doRound2' maxTest (length monkeys) monkeys
    where 
        maxTest = foldl (lcm) 1 $ map (getTestVal) monkeys

part2 :: [Monkey] -> Integer
part2 = monkeyBusiness
    . take 2
    . sortOn (Down)
    . map (getNumInspections)
    . (flip (!!)) 10000
    . iterate doRound2

main :: IO ()
main = openFile "input/day11.txt" ReadMode 
       >>= hGetContents 
       >>= print . solve part1 part2 . parse