module Main where

import Data.Array
import Data.List (sortBy)


main = do
    answer_1_1_test <- part1 7 <$> readFile "test_input_1.txt"
    putStrLn $ "(test) Part 1.1 (2028): " ++ (show answer_1_1_test)

    answer_1_2_test <- part1 9 <$> readFile "test_input_2.txt"
    putStrLn $ "(test) Part 1.2 (10092): " ++ (show answer_1_2_test)

    answer_1 <- part1 49 <$> readFile "input.txt"
    putStrLn $ "(live) Part 1 (???): " ++ (show answer_1)


-- Data --

type Maze = Array (Int, Int) Char
type Move = Char
type Pos  = (Int, Int)
type Tile = (Pos, Char)


-- Part 1 --

strToMaze :: String -> Int -> Maze
strToMaze ss size = listArray ((0,0), (size,size)) $ concat $ mazeLines
    where
        mazeLines = takeWhile (/="") $ lines ss

strToMoves :: String -> [Move]
strToMoves = concat . tail . dropWhile (/="") . lines

part1 :: Int -> String -> Int
part1 size ss = calcScore $ stepAll maze moves
    where
        maze = strToMaze ss size
        moves = strToMoves ss

calcScore :: Maze -> Int
calcScore maze = sum [ 100 * (fst p) + (snd p) | p <- coords, maze!p == 'O' ]
    where
        coords = range $ bounds maze

stepAll :: Maze -> [Move] -> Maze
stepAll maze     [] = maze
stepAll maze (m:ms) = stepAll (step maze m) ms

step :: Maze -> Move -> Maze
step maze move
    | maze!newPos == '.' = maze // [(curPos, '.'), (newPos, '@')]
    | maze!newPos == '#' = maze
    | maze!newPos == 'O' = if newMaze!newPos == '.'
                           then step newMaze move
                           else maze
    where
        newMaze = moveBoxes maze newPos move
        curPos = getCurPos maze
        newPos = getNewPos curPos move

moveBoxes :: Maze -> Pos -> Move -> Maze
moveBoxes maze boxPos m
    | lastTileChar == '.' = maze // [(lastTilePos, 'O'), (boxPos, '.')]
    | otherwise = maze
    where
        (lastTilePos, lastTileChar) = findLastTile maze boxPos m

findLastTile :: Maze -> Pos -> Move -> Tile
findLastTile maze p m
    | maze!nextPos /= 'O' = (nextPos, maze!nextPos)
    | otherwise = findLastTile maze nextPos m
    where
        nextPos = getNewPos p m

getNewPos :: Pos -> Move -> Pos
getNewPos p m
    | m == '^' = (row - 1, col + 0)
    | m == '>' = (row + 0, col + 1)
    | m == 'v' = (row + 1, col + 0)
    | m == '<' = (row + 0, col - 1)
    where
        row = fst p
        col = snd p

getCurPos :: Maze -> Pos
getCurPos maze = head [ p | p <- range $ bounds maze, maze!p == '@' ]

printMaze :: Maze -> String
printMaze maze = unlines $ splitEvery mazeSize $ elems maze
    where
        mazeSize = (fst $ snd $ bounds maze) + 1

splitEvery :: Int -> String -> [String]
splitEvery _ [] = []
splitEvery n ss = left : splitEvery n right
    where
        (left, right) = splitAt n ss
