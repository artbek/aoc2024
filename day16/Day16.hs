module Main where

import Data.Array
import Debug.Trace
import Data.List (groupBy, sortBy)


main = do
    -- answer_1_0_test <- part1 <$> readFile "sample_1.txt"
    -- putStrLn $ "(test) Part 1.0a (1006): " ++ (show answer_1_0_test)

    -- answer_1_0_test <- part1 <$> readFile "sample_2.txt"
    -- putStrLn $ "(test) Part 1.0b (1006): " ++ (show answer_1_0_test)

    answer_1_0_test <- part1 <$> readFile "sample_3.txt"
    putStrLn $ "(test) Part 1.0c (3010): " ++ (show answer_1_0_test)

    answer_1_1_test <- part1 <$> readFile "test_input_1.txt"
    putStrLn $ "(test) Part 1.1 (7036): " ++ (show answer_1_1_test)

    answer_1_2_test <- part1 <$> readFile "test_input_2.txt"
    putStrLn $ "(test) Part 1.2 (11048): " ++ (show answer_1_2_test)

    answer_1 <- part1 <$> readFile "input.txt"
    putStrLn $ "(live) Part 1 (???): " ++ (show answer_1)


-- Data --

type Pos = (Int, Int)
type Maze = Array Pos Char
type Tile = (Pos, Cost, Dir)
type Cost = Int
type Dir = Char


-- Part 1 --

strToMaze :: String -> Maze
strToMaze ss = listArray ((0,0), (maxRow, maxCol)) $ concat $ lines ss
    where
        maxRow = mazeSize - 1
        maxCol = length (head (lines ss)) - 1
        mazeSize = length $ lines ss


-- 89344 is too high

part1 :: String -> Int
part1 ss = bfs maze [startingTile] (maxBound::Int)
    where
        maze         = strToMaze ss
        startingPos  = head [ p | p <- range $ bounds maze, maze!p == 'S' ]
        startingTile = (startingPos, 0, '>')

bfs :: Maze -> [Tile] -> Cost -> Cost
bfs maze    [] minCost = minCost
bfs maze tiles minCost = -- trace (mazeToStr maze' ++ "\n")
                         bfs maze' (getUnique neighbours) minCost'
    where
        maze'      = maze // [ (pos, 'O') | (pos,_,_) <- tiles ]
        neighbours = foldl (\b a -> b ++ fst (findNbrs maze' a)) [] tiles
        minCost'   = foldl (\b a -> min b (snd (findNbrs maze' a))) minCost tiles

getUnique :: [Tile] -> [Tile]
getUnique tiles = deduped
    where
        sorted1 = sortBy (\(p1,_,_) (p2,_,_) -> compare p1 p2 ) tiles
        grouped = groupBy (\(p1,_,_) (p2,_,_) -> p1 == p2) sorted1
        sorted2 = map (sortBy (\(_,c1,_) (_,c2,_) -> compare c1 c2 )) grouped
        deduped = map (head) sorted2

findNbrs :: Maze -> Tile -> ([Tile], Int)
findNbrs maze t@((r,c), cost, dir) = (legalTiles, minCost)
    where
        tN = ((r-1, c), newCost cost dir '^', '^')
        tS = ((r+1, c), newCost cost dir 'v', 'v')
        tE = ((r, c+1), newCost cost dir '>', '>')
        tW = ((r, c-1), newCost cost dir '<', '<')
        legalTiles = filter (\(pos,_,_) -> maze!pos == '.') [tN, tS, tE, tW]
        exitTiles  = filter (\(pos,_,_) -> maze!pos == 'E') [tN, tS, tE, tW]
        minCost    = foldl (\b (_,cost,_) -> min b cost) (maxBound::Int) exitTiles

newCost :: Int -> Dir -> Dir -> Int
newCost baseCost curDir newDir
    | curDir == newDir = baseCost + 1
    | otherwise        = baseCost + 1001


-- Helpers --

mazeToStr :: Maze -> String
mazeToStr maze = unlines $ splitEvery mazeWidth $ elems maze
    where
        mazeWidth = (snd $ snd $ bounds maze) + 1

printMaze :: Maze -> IO ()
printMaze = putStrLn . mazeToStr

splitEvery :: Int -> String -> [String]
splitEvery _ [] = []
splitEvery n ss = left : splitEvery n right
    where
        (left, right) = splitAt n ss
