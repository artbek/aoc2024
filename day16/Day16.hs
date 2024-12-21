module Main where

import Data.Array
import Debug.Trace
import Data.List (groupBy, sortBy)


main = do
    answer_1_0_test <- part1 <$> readFile "sample_1.txt"
    putStrLn $ "(test) Part 1.0a (1006): " ++ (show answer_1_0_test)

    answer_1_0_test <- part1 <$> readFile "sample_2.txt"
    putStrLn $ "(test) Part 1.0b (1006): " ++ (show answer_1_0_test)

    answer_1_0_test <- part1 <$> readFile "sample_3.txt"
    putStrLn $ "(test) Part 1.0c (3010): " ++ (show answer_1_0_test)

    answer_1_1_test <- part1 <$> readFile "test_input_1.txt"
    putStrLn $ "(test) Part 1.1 (7036): " ++ (show answer_1_1_test)

    answer_1_2_test <- part1 <$> readFile "test_input_2.txt"
    putStrLn $ "(test) Part 1.2 (11048): " ++ (show answer_1_2_test)

    answer_1 <- part1 <$> readFile "input.txt"
    putStrLn $ "(live) Part 1 (???): " ++ (show answer_1)


-- Data --

data Sq = Sq { val :: Char
             , n :: Int
             , e :: Int
             , s :: Int
             , w :: Int
             } deriving Show

type Pos = (Int, Int)
type Maze = Array Pos Sq
type Tile = (Pos, Cost, Dir)
type Cost = Int
type Dir = Char


-- Part 1 --

strToMaze :: String -> Maze
strToMaze ss = listArray ((0,0), (maxRow, maxCol)) $ [ Sq v 0 0 0 0 | v <- vs ]
    where
        vs = concat $ lines ss
        maxRow = length (lines ss) - 1
        maxCol = length (head (lines ss)) - 1

part1 :: String -> Int
part1 ss = bfs maze [startingTile] (maxBound::Int)
    where
        maze         = strToMaze ss
        startingPos  = head [ p | p <- range $ bounds maze, val (maze!p) == 'S' ]
        startingTile = (startingPos, 0, '>')

bfs :: Maze -> [Tile] -> Cost -> Cost
bfs maze    [] minCost = minCost
bfs maze tiles minCost = bfs maze' (neighbours) minCost'
    where
        maze'      = maze // [ (p, visitedSq (maze!p) d c) | (p,c,d) <- tiles ]
        neighbours = foldl (\b a -> b ++ fst (findNbrs maze' a)) [] tiles
        minCost'   = foldl (\b a -> min b (snd (findNbrs maze' a))) minCost tiles

visitedSq :: Sq -> Dir -> Cost -> Sq
visitedSq s '^' cost = s { n = cost }
visitedSq s 'v' cost = s { s = cost }
visitedSq s '>' cost = s { e = cost }
visitedSq s '<' cost = s { n = cost }

findNbrs :: Maze -> Tile -> ([Tile], Int)
findNbrs maze t@((r,c), cost, dir) = (legalTiles, minCost)
    where
        tN = ((r-1, c), newCost cost dir '^', '^')
        tS = ((r+1, c), newCost cost dir 'v', 'v')
        tE = ((r, c+1), newCost cost dir '>', '>')
        tW = ((r, c-1), newCost cost dir '<', '<')
        legalTiles = filter (isLegal maze dir) [tN, tS, tE, tW]
        exitTiles  = filter (\(pos,_,_) -> val (maze!pos) == 'E') [tN, tS, tE, tW]
        minCost    = foldl (\b (_,cost,_) -> min b cost) (maxBound::Int) exitTiles

isLegal :: Maze -> Dir -> Tile -> Bool
isLegal maze prevDir (pos, cost, dir)
    | val sq == '#' = False
    | cost > 90000  = False
    | dir == '^' && n sq > 0 && n sq < cost = False
    | dir == 'v' && s sq > 0 && s sq < cost = False
    | dir == '>' && e sq > 0 && e sq < cost = False
    | dir == '<' && w sq > 0 && w sq < cost = False
    | prevDir == '^' && dir == 'v' = False
    | prevDir == 'v' && dir == '^' = False
    | prevDir == '>' && dir == '<' = False
    | prevDir == '<' && dir == '>' = False
    | otherwise = True
    where
        sq = maze!pos

newCost :: Int -> Dir -> Dir -> Int
newCost baseCost curDir newDir
    | curDir == newDir = baseCost + 1
    | otherwise        = baseCost + 1001


-- Helpers --

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
