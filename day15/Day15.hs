module Main where

import Data.Array


main = do
    answer_1_1_test <- part1 (7,7) <$> readFile "test_input_1.txt"
    putStrLn $ "(test) Part 1.1 (2028): " ++ (show answer_1_1_test)

    answer_1_2_test <- part1 (9,9) <$> readFile "test_input_2.txt"
    putStrLn $ "(test) Part 1.2 (10092): " ++ (show answer_1_2_test)

    answer_1 <- part1 (49,49) <$> readFile "input.txt"
    putStrLn $ "(live) Part 1 (???): " ++ (show answer_1)


-- Data --

type Maze = Array (Int, Int) Char
type Move = Char
type Pos  = (Int, Int)
type Tile = (Pos, Char)


-- Part 1 --

strToMaze :: String -> (Int, Int) -> Maze
strToMaze ss dim = listArray ((0,0), (snd dim, fst dim)) $ concat $ mazeLines
    where
        mazeLines = takeWhile (/="") $ lines ss

strToMoves :: String -> [Move]
strToMoves = concat . tail . dropWhile (/="") . lines

part1 :: (Int, Int) -> String -> Int
part1 dim ss = calcScore $ stepAll maze moves
    where
        maze = strToMaze ss dim
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
printMaze maze = unlines $ splitEvery mazeWidth $ elems maze
    where
        mazeWidth = (snd $ snd $ bounds maze) + 1

splitEvery :: Int -> String -> [String]
splitEvery _ [] = []
splitEvery n ss = left : splitEvery n right
    where
        (left, right) = splitAt n ss


-- Part 2 --

type LargeBox = (Pos, Pos)

emptyBox :: LargeBox
emptyBox = ((0,0), (0,0))

getVertGroup :: Maze -> Move -> LargeBox -> [LargeBox]
getVertGroup maze move curBox
    | testArea == "[]" = [curBox] ++ f box0
    | testArea == "][" = [curBox] ++ f box1 ++ f box2
    | testArea == "]." = [curBox] ++ f box1
    | testArea == ".[" = [curBox] ++ f box2
    | testArea == ".." = [curBox] -- can be moved?
    | otherwise        = [emptyBox] -- can't be moved?
    where
        testArea = maze ! (rowL+dir, colL) : maze ! (rowR+dir, colR) : ""
        (rowL, colL) = fst curBox
        (rowR, colR) = snd curBox
        f = getVertGroup maze move
        dir
            | move == '^' = -1
            | move == 'v' = 1
        box0 = ((rowL+dir, colL+0), (rowR+dir, colR+0))
        box1 = ((rowL+dir, colL-1), (rowR+dir, colR-1))
        box2 = ((rowL+dir, colL+1), (rowR+dir, colR+1))


canVertGroupMove :: [LargeBox] -> Bool
canVertGroupMove boxes = length [ b | b <- boxes, b == emptyBox ] == 0

moveVertGroup :: Maze -> Move -> [LargeBox] -> Maze
moveVertGroup maze move boxes = maze''
    where
        maze' = removeVertGroup maze boxes
        maze'' = updateVertGroup maze' move boxes

removeVertGroup :: Maze -> [LargeBox] -> Maze
removeVertGroup maze [] = maze;
removeVertGroup maze (b:bb) = removeVertGroup newMaze bb
    where
        p1 = fst b
        p2 = snd b
        newMaze = maze // [ (p1, '.'), (p2, '.') ]

updateVertGroup :: Maze -> Move -> [LargeBox] -> Maze
updateVertGroup maze move [] = maze
updateVertGroup maze move (b:bb) = updateVertGroup newMaze move bb
    where
        p1 = fst b
        p2 = snd b
        p1' = getNewPos p1 move
        p2' = getNewPos p2 move
        newMaze = maze // [ (p1', '['), (p2', ']') ]
