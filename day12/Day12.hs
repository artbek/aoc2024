module Main where

import Data.List (groupBy, sortBy)


main = do
    answer_1_1_test <- part1 <$> readFile "test_input_1.txt"
    putStrLn $ "(test) Part 1.1  (140): " ++ (show answer_1_1_test)

    answer_1_2_test <- part1 <$> readFile "test_input_2.txt"
    putStrLn $ "(test) Part 1.2  (772): " ++ (show answer_1_2_test)

    answer_1_3_test <- part1 <$> readFile "test_input_3.txt"
    putStrLn $ "(test) Part 1.2 (1930): " ++ (show answer_1_3_test)

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "(live) Part 1: " ++ (show answer_1_live)

    -- let answer_2_test = part2 "125 17"
    -- putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"

    -- answer_2_live <- part2 <$> readFile "input.txt"
    -- putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


-- Data --

data Tile = Tile { row :: Int
                 , col :: Int
                 , val :: Char
                 , plotId :: Int
                 } deriving (Show)

type Plot = [Tile]
type Farm = ([[Tile]], Int)

strToFarm :: String -> Farm
strToFarm ss = (ff, length ff)
    where
        tiles = addCoords $ lines ss
        ff = groupBy (\a b -> row a == row b) tiles

addCoords :: [String] -> [Tile]
addCoords rows = [ Tile r c v (-1)
                 | (r, rr) <- zip [0..] rows, (c, v) <- zip [0..] rr ]

printFarm :: Farm -> String
printFarm = unlines . map (map (\x -> if plotId x >= 0 then '.' else val x)) . fst


-- Part 1 --

part1 :: String -> Int
part1 ss = sum $ map (getPrice ff) $ groupedPlots
    where
        groupedPlots = getAllPlotsGrouped $ markAllPlots 0 $ ff
        ff = strToFarm ss

markAllPlots :: Int -> Farm -> Farm
markAllPlots pid ff
    | length (unvisitedTiles ff) == 0 = ff
    | otherwise = markAllPlots (pid + 1) newFarm
    where
        newFarm = markPlot ff (row utile, col utile) (val utile) pid
        utile   = findUnvisitedTile ff

markPlot :: Farm -> (Int, Int) -> Char -> Int -> Farm
markPlot ff (r, c) val pid
    | isTileInvalid ff (r, c) val = ff
    | otherwise = ffW
    where
        ff0 = addPlotId ff pid (r, c)
        ffN = markPlot ff0 (r-1, c+0) val pid
        ffE = markPlot ffN (r+0, c+1) val pid
        ffS = markPlot ffE (r+1, c+0) val pid
        ffW = markPlot ffS (r+0, c-1) val pid

isTileInvalid :: Farm -> (Int, Int) -> Char -> Bool
isTileInvalid ff (r, c) v
    | r < 0            = True
    | c < 0            = True
    | r >= snd ff      = True
    | c >= snd ff      = True
    | plotId tile > -1 = True
    | val tile /= v    = True
    | otherwise        = False
    where
        farm = fst ff
        tile = farm!!r!!c

addPlotId :: Farm -> Int -> (Int, Int) -> Farm
addPlotId ff pid (r, c) = (take r farm ++ [newRow] ++ drop (r+1) farm, snd ff)
    where
        farm    = fst ff
        curRow  = farm!!r
        newRow  = take c curRow ++ [newTile] ++ drop (c+1) curRow
        curTile = farm!!r!!c
        newTile = curTile { plotId = pid }

unvisitedTiles :: Farm -> [Tile]
unvisitedTiles = filter (\x -> plotId x == -1) . concat . fst

findUnvisitedTile :: Farm -> Tile
findUnvisitedTile = head . unvisitedTiles

getAllPlotsGrouped :: Farm -> [[Tile]]
getAllPlotsGrouped = customGroup . customSort . concat . fst
    where
        customSort  = sortBy (\a b -> compare (plotId a) (plotId b))
        customGroup = groupBy (\a b -> plotId a == plotId b)

getPrice :: Farm -> [Tile] -> Int
getPrice ff pp = getArea pp * getPerimeter ff pp

getArea :: [Tile] -> Int
getArea = length

getPerimeter :: Farm -> [Tile] -> Int
getPerimeter ff []     = 0
getPerimeter ff (t:tt) = totalScore + getPerimeter ff tt
    where
        r = row t
        c = col t
        scoreN = if isTileInvalid ff (r-1, c+0) (val t) then 1 else 0
        scoreE = if isTileInvalid ff (r+0, c+1) (val t) then 1 else 0
        scoreS = if isTileInvalid ff (r+1, c+0) (val t) then 1 else 0
        scoreW = if isTileInvalid ff (r+0, c-1) (val t) then 1 else 0
        totalScore = scoreN + scoreE + scoreS + scoreW


-- Part 2 --

part2 :: String -> Int
part2 = length
