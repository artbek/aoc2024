module Main where

import Data.Array
import Data.List (nub)


main = do
    answer_1_test <- part1 12 (6,6) <$> readFile "test_input.txt"
    putStrLn $ "(test) Part 1 (22): " ++ (show answer_1_test)

    answer_1_live <- part1 1024 (70,70) <$> readFile "input.txt"
    putStrLn $ "(live) Part 1 (???): " ++ (show answer_1_live)


-- Data --

data Sq = Sq { val :: Char, isVisited :: Bool } deriving Show

type Pos = (Int, Int)
type Maze = Array Pos Sq

strToMaze :: Int -> (Int,Int) -> String -> Maze
strToMaze qty dim ss = foldl (\b (c,r) -> b // [((r,c), Sq '#' False)]) m corrupted
    where
        m = emptyMaze dim
        coords = map (toTuple . words . replaceChars "," ' ') (lines ss)
        corrupted = take qty coords

emptyMaze :: (Int,Int) -> Maze
emptyMaze (maxRow, maxCol) = listArray ((0,0), (maxRow,maxCol)) mazeList
    where
        mazeList = [ Sq '.' False | r <- [0..maxRow], c <- [0..maxCol] ]


-- Part 1 --

part1 :: Int -> (Int,Int) -> String -> Int
part1 qty dim ss = bfs maze [(0,0)] 0
    where
        maze         = strToMaze qty dim ss

bfs :: Maze -> [Pos] -> Int -> Int
bfs maze        [] cost = -999
bfs maze positions cost
    | foundExit = cost
    | otherwise = bfs maze' (nub neighbours) (cost + 1)
    where
        maze'      = maze // [ (pos, Sq 'O' True) | pos <- positions ]
        neighbours = foldl (\b a -> b ++ findNbrs maze' a) [] positions
        exitSq     = snd $ bounds maze
        foundExit  = elem exitSq positions

findNbrs :: Maze -> Pos -> [Pos]
findNbrs maze (r,c) = legalTiles
    where
        pN = (r-1, c)
        pS = (r+1, c)
        pE = (r, c+1)
        pW = (r, c-1)
        legalTiles = filter (isLegal maze) [pN, pS, pE, pW]

isLegal :: Maze -> Pos -> Bool
isLegal maze pos@(r,c)
    | r < 0 || c < 0 = False
    | r > maxRowIdx  = False
    | c > maxColIdx  = False
    | val sq == '#'  = False
    | isVisited sq   = False
    | otherwise      = True
    where
        sq = maze!pos
        (maxRowIdx, maxColIdx) = snd $ bounds maze


-- Helpers --

toTuple :: [String] -> (Int,Int)
toTuple [a,b] = (read a, read b)

replaceChars :: [Char] -> Char -> String -> String
replaceChars aa b cs = [ if elem c aa then b else c | c <- cs ]

mazeToStr :: Maze -> String
mazeToStr maze = unlines $ splitEvery mazeWidth $ map val $ elems maze
    where
        mazeWidth = (snd $ snd $ bounds maze) + 1

printMaze :: Maze -> IO ()
printMaze = putStrLn . mazeToStr

splitEvery :: Int -> String -> [String]
splitEvery _ [] = []
splitEvery n ss = left : splitEvery n right
    where
        (left, right) = splitAt n ss
