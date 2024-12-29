module Main where

import Data.Array


main :: IO ()
main = do
    answer_1_test <- part1 1 <$> readFile "test_input.txt"
    putStrLn $ "(test) Part 1 (44): " ++ (show answer_1_test)

    answer_1_live <- part1 100 <$> readFile "input.txt"
    putStrLn $ "(live) Part 1 (???): " ++ (show answer_1_live)

    -- answer_2_test <- part2 <$> readFile "test_input.txt"
    -- putStrLn $ "(test) Part 2 (16): " ++ (show answer_2_test)

    -- answer_2_live <- part2 <$> readFile "input.txt"
    -- putStrLn $ "(live) Part 2 (???): " ++ (show answer_2_live)


-- Data --

data Tile = Tile { val :: Char
                 , pos :: Pos
                 , visited :: Bool
                 , dist :: Int
                 } deriving (Eq, Show)

type Pos = (Int, Int)
type Maze = Array Pos Tile
type Cost = Int

strToMaze :: String -> Maze
strToMaze ss = maze
    where
        maze = foldl (\m p -> m // [ (p, (m!p) { pos = p }) ]) mm (coords mm)
        mm = listArray ((0,0), (maxRow, maxCol)) $ [ newTile v | v <- vs ]
        vs = concat $ lines ss
        maxRow = length (lines ss) - 1
        maxCol = length (head (lines ss)) - 1
        newTile v = Tile v (0,0) False (-1)

startTile :: Maze -> Tile
startTile maze = head [ maze!p | p <- coords maze, val (maze!p) == 'S' ]

endTile :: Maze -> Tile
endTile maze = head [ maze!p | p <- coords maze, val (maze!p) == 'E' ]

coords :: Maze -> [Pos]
coords = range . bounds


-- Part 1 --

part1 :: Int -> String -> Int
part1 minCheatValue ss = findCheats minCheatValue maze'
    where
        maze' = fst path
        path  = findPath maze [(startTile maze)] (endTile maze) 0
        maze  = strToMaze ss

findPath :: Maze -> [Tile] -> Tile -> Int -> (Maze, Int)
findPath maze tiles endTile level
    | length foundEnd > 0 = (maze', level)
    | otherwise = findPath maze' tiles' endTile level'
    where
        foundEnd = filter (== endTile) tiles
        maze'  = markVisited maze tiles level
        tiles' = findNbrs maze' tiles
        level' = level + 1

findNbrs :: Maze -> [Tile] -> [Tile]
findNbrs maze     [] = []
findNbrs maze (t:ts) = legalTiles ++ findNbrs maze' ts
    where
        r = fst $ pos t
        c = snd $ pos t
        tN = maze ! (r-1, c)
        tS = maze ! (r+1, c)
        tE = maze ! (r, c+1)
        tW = maze ! (r, c-1)
        maze' = markVisited maze legalTiles 0
        legalTiles = filter (isTileLegal maze) [tN, tS, tE, tW]

isTileLegal :: Maze -> Tile -> Bool
isTileLegal maze t
    | val t == '#' = False
    | visited t    = False
    | otherwise    = True

markVisited :: Maze -> [Tile] -> Int -> Maze
markVisited mm tt d = mm // [ (pos t, t {visited = True, dist = d}) | t <- tt ]

findCheats :: Int -> Maze -> Int
findCheats minVal mm = length [ t | t <- allWalls mm, cheatVal mm t >= minVal ]

allWalls :: Maze -> [Tile]
allWalls maze = [ maze!p | p <- coords maze, val (maze!p) == '#']

cheatVal :: Maze -> Tile -> Int
cheatVal maze tile
    | row == 0 || row == maxRow = -999
    | col == 0 || col == maxCol = -999
    | val tile == '#' && isVertConnector = abs(dist nT - dist sT) - 2
    | val tile == '#' && isHoriConnector = abs(dist eT - dist wT) - 2
    | otherwise = -999
    where
        nT = maze!(row - 1, col)
        sT = maze!(row + 1, col)
        eT = maze!(row, col + 1)
        wT = maze!(row, col - 1)
        row = fst (pos tile)
        col = snd (pos tile)
        maxRow = fst $ snd (bounds maze)
        maxCol = snd $ snd (bounds maze)
        isVertConnector = visited nT && visited sT
        isHoriConnector = visited eT && visited wT


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
