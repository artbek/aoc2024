module Day06 where

import Data.List (findIndex)
import Data.Maybe (fromMaybe)

main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_live) ++ " (live)"

    --answer_2_test <- part2 <$> readFile "test_input.txt"
    --putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"

    --answer_2_live <- part2 <$> readFile "input.txt"
    --putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


-- Data --

type Map = [[Char]]
type Pos = (Int,Int)
data Dir = N | E | S | W deriving (Eq,Show)


-- Part 1 --

part1 :: String -> Int
part1 ss = step (map, startingPos, N)
    where
        map = getMap ss
        startingPos = findGuard map 0

getMap :: String -> Map
getMap = lines

findGuard :: Map -> Int -> Pos
findGuard (r:rs) rowIdx =
    if elem '^' r
        then (rowIdx, fromMaybe 0 colIdx)
        else findGuard rs (rowIdx+1)
    where
        colIdx = findIndex (=='^') r

step :: (Map, Pos, Dir) -> Int
step (map, pos, dir)
    | isOutsideTheMap pos map = length $ filter (=='x') (concat map)
    | isOnObstacle newPos map = step (newMap, pos, newDir)
    | otherwise = step (newMap, newPos, dir)
    where
        newPos = nextPos pos dir
        newDir = turnRight dir
        newMap = makeFootprint map pos

isOutsideTheMap :: Pos -> Map -> Bool
isOutsideTheMap pos map =
    if row < 0 || row > maxRow || col < 0 || col > maxCol
        then True
        else False
    where
        row = fst pos
        col = snd pos
        maxRow = length map - 1
        maxCol = length (map!!0) - 1

isOnObstacle :: Pos -> Map -> Bool
isOnObstacle pos map
    | isOutsideTheMap pos map = False
    | otherwise = map!!row!!col == '#'
    where
        row = fst pos
        col = snd pos

nextPos :: Pos -> Dir -> Pos
nextPos (row,col) N = (row - 1, col + 0)
nextPos (row,col) S = (row + 1, col + 0)
nextPos (row,col) E = (row + 0, col + 1)
nextPos (row,col) W = (row + 0, col - 1)

turnRight :: Dir -> Dir
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

makeFootprint :: Map -> Pos -> Map
makeFootprint map pos = take row map ++ [newRow] ++ drop (row+1) map
    where
        row = fst pos
        col = snd pos
        curRow = map!!row
        newRow = take col curRow ++ ['x'] ++ drop (col+1) curRow


-- Part 2 --

part2 :: String -> Int
part2 = length

