import Aoc
import System.IO
import qualified Data.Map as Map
import Data.List
import Control.Monad
import Data.Maybe

data Command = 
      CD String
    | LS
    | DirCmd String
    | FileCmd Int
    deriving (Eq, Show)
          
data DirContents = Dir String | File Int
    deriving (Eq, Show)

type DirTree = Map.Map String [DirContents]

insertDir :: DirTree -> String -> String -> DirTree
insertDir tree key name = Map.alter (Just . (Dir name :) . concat . maybeToList) key tree

insertFile :: DirTree -> String -> Int -> DirTree
insertFile tree key size = Map.alter (Just . (File size :) . concat . maybeToList) key tree

moveUp :: String -> String
moveUp s = take ((snd $ last $ filter ((=='/') . fst) $ zip s [0..])) s

runCommand :: DirTree -> String -> Command -> (DirTree, String)
runCommand tree current (CD "..") = (tree, moveUp current)
runCommand tree current (CD "/") = (tree, "/")
runCommand tree "/" (CD newDir) = (tree, "/" ++ newDir)
runCommand tree current (CD newDir) = (tree, current ++ "/" ++ newDir)
runCommand tree current (LS) = (tree, current) -- irrelevant
runCommand tree "/" (DirCmd name) = (insertDir tree "/" ("/"++name), "/")
runCommand tree current (DirCmd name) = (insertDir tree current (current++"/"++name), current)
runCommand tree current (FileCmd size) = (insertFile tree current size, current)

evaluate :: DirTree -> String -> [Command] -> DirTree
evaluate tree _ [] = tree
evaluate tree current (c:cs) = evaluate newTree newDir cs
    where 
        (newTree, newDir) = runCommand tree current c

getDirItem :: DirTree -> String -> [DirContents]
getDirItem tree name = maybe [] id $ Map.lookup name tree

dirSize :: DirTree -> DirContents -> Int
dirSize _ (File size) = size
dirSize tree (Dir name) = sum [dirSize tree item | item <- getDirItem tree name]

isDir :: Command -> Bool
isDir (DirCmd _) = True
isDir _ = False

getDirName :: Command -> String
getDirName (DirCmd n) = n
getDirName _ = ""

getAllDirs :: DirTree -> DirContents -> [String]
getAllDirs _ (File _) = []
getAllDirs tree (Dir name) = name:(concat [getAllDirs tree $ item | item <- getDirItem tree name])

part1 :: [Command] -> Int
part1 input = sum $ filter (<=100000) [dirSize tree (Dir d) | d <- allDirs]
    where 
        tree = evaluate Map.empty "" input
        allDirs = getAllDirs tree (Dir "/")

part2 :: [Command] -> Int
part2 input = head $ filter (>=spaceNeeded) $ sort [dirSize tree (Dir d) | d <- allDirs]
    where 
        tree = evaluate Map.empty "" input
        totalSpace = 70000000
        spaceRequired = 30000000
        spaceUsed = dirSize tree (Dir "/")
        spaceLeft = totalSpace - spaceUsed
        spaceNeeded = spaceRequired - spaceLeft
        allDirs = getAllDirs tree (Dir "/")


parseCommand :: String -> Command
parseCommand s 
    | isPrefixOf "$ cd" s = CD $ drop 5 s
    | isPrefixOf "$ ls" s = LS
    | isPrefixOf "dir" s = DirCmd $ drop 4 s
    | otherwise = FileCmd (read $ head parts)
        where 
            parts = words s

parse :: String -> [Command]
parse = map (parseCommand) . lines

main :: IO ()
main = openFile "input/day7.txt" ReadMode 
       >>= hGetContents
       >>= print . solve part1 part2 . parse 