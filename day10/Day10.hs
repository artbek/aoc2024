module Main where

import Data.Char (digitToInt, intToDigit)
import Data.List (groupBy, nub)


main = do
    answer_1_test <- part1 <$> readFile "test_input5.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_live) ++ " (live)"

    answer_2_test <- part2 <$> readFile "test_input5.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"

    answer_2_live <- part2 <$> readFile "input.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


-- Part 1 --

type Map  = ([[Tile]], Int)
data Tile = Tile { height :: Int, row :: Int, col :: Int, visited :: Bool }
            deriving (Eq, Show)

strToMap :: String -> Map
strToMap ss = (mm, length mm)
    where
        tiles = addCoords $ lines ss
        mm = groupBy (\a b -> row a == row b) tiles

addCoords :: [[Char]] -> [Tile]
addCoords grid = [ Tile (toInt x) r c False | (r, rr) <- rows, (c, x) <- zip [0..] rr ]
    where
        rows = zip [0..] grid
        toInt a = if a == '.' then -1 else digitToInt a

findAllZeros :: Map -> [(Int, Int)]
findAllZeros mm = [ (row t, col t) | rr <- fst mm, t <- rr, height t == 0 ]

part1 :: String -> Int
part1 ss = countTrails mm startingPos
    where
        mm = strToMap ss
        startingPos = findAllZeros mm

countTrails :: Map -> [(Int, Int)] -> Int
countTrails mm []     = 0
countTrails mm (p:ps) = count + countTrails mm ps
    where
        tiles = findTrails (fst p) (snd p) (-1) mm
        count = length $ nub tiles

findTrails :: Int -> Int -> Int -> Map -> [Tile]
findTrails r c prevHeight mm
    | isOutside r c mm    = []
    | visited curTile     = []
    | elevation  > 1      = []
    | elevation  < 1      = []
    | height curTile == 9 = [curTile]
    | otherwise = [] ++ trailN ++ trailE ++ trailS ++ trailW
    where
        elevation = height curTile - prevHeight
        curTile = (fst mm)!!r!!c
        curHeight = height curTile
        newMap = markVisited (row curTile) (col curTile) mm
        trailN = findTrails (r - 1) (c + 0) curHeight newMap
        trailE = findTrails (r + 0) (c + 1) curHeight newMap
        trailS = findTrails (r + 1) (c + 0) curHeight newMap
        trailW = findTrails (r + 0) (c - 1) curHeight newMap

markVisited :: Int -> Int -> Map -> Map
markVisited r c mm = (take r m ++ [newRow] ++ drop (r + 1) m, snd mm)
    where
        m = fst mm
        newRow = take c oldRow ++ [newTile] ++ drop (c + 1) oldRow
        oldRow = m!!r
        newTile = oldTile { visited = True }
        oldTile = m!!r!!c

isOutside :: Int -> Int -> Map -> Bool
isOutside row col mm =
    if row < 0 || row > maxRow || col < 0 || col > maxCol
        then True
        else False
    where
        maxRow = (snd mm) - 1
        maxCol = maxRow


-- Part 2 --

part2 :: String -> Int
part2 ss = countTrails' mm startingPos
    where
        mm = strToMap ss
        startingPos = findAllZeros mm

countTrails' :: Map -> [(Int, Int)] -> Int
countTrails' mm []     = 0
countTrails' mm (p:ps) = count + countTrails' mm ps
    where
        tiles = findTrails (fst p) (snd p) (-1) mm
        count = length $ tiles


-- Helpers --

mapToStr :: Map -> String
mapToStr mm = unlines $ map (map tileToChar) (fst mm)
    where
        tileToChar = (\x -> if visited x then '.' else intToDigit (height x))
