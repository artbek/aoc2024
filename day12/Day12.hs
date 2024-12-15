module Main where

import Data.List (groupBy, nub, sortBy)


main = do
    answer_1_1_test <- part1 <$> readFile "test_input_1.txt"
    putStrLn $ "(test) Part 1.1  (140): " ++ (show answer_1_1_test)

    answer_1_2_test <- part1 <$> readFile "test_input_2.txt"
    putStrLn $ "(test) Part 1.2  (772): " ++ (show answer_1_2_test)

    answer_1_3_test <- part1 <$> readFile "test_input_3.txt"
    putStrLn $ "(test) Part 1.3 (1930): " ++ (show answer_1_3_test)

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "(live) Part 1: " ++ (show answer_1_live)

    answer_2_1_test <- part2 <$> readFile "test_input_1.txt"
    putStrLn $ "(test) Part 2.1  (80): " ++ (show answer_2_1_test)

    answer_2_2_test <- part2 <$> readFile "test_input_2.txt"
    putStrLn $ "(test) Part 2.2  (436): " ++ (show answer_2_2_test)

    answer_2_3_test <- part2 <$> readFile "test_input_3.txt"
    putStrLn $ "(test) Part 2.3 (1206): " ++ (show answer_2_3_test)

    answer_2_4_test <- part2 <$> readFile "test_input_4.txt"
    putStrLn $ "(test) Part 2.4  (236): " ++ (show answer_2_4_test)

    answer_2_5_test <- part2 <$> readFile "test_input_5.txt"
    putStrLn $ "(test) Part 2.5  (368): " ++ (show answer_2_5_test)

    answer_2_live <- part2 <$> readFile "input.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


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

getAllPlotsGrouped :: Farm -> [Plot]
getAllPlotsGrouped = customGroup . customSort . concat . fst
    where
        customSort  = sortBy (\a b -> compare (plotId a) (plotId b))
        customGroup = groupBy (\a b -> plotId a == plotId b)

getPrice :: Farm -> Plot -> Int
getPrice ff pp = getArea pp * getPerimeter ff pp

getArea :: Plot -> Int
getArea = length

getPerimeter :: Farm -> Plot -> Int
getPerimeter ff plot = length $ getFenceSegments ff plot

getFenceSegments :: Farm -> Plot -> [FenceSegment]
getFenceSegments ff []     = []
getFenceSegments ff (t:tt) = customSort (allSegs ++ getFenceSegments ff tt)
    where
        r = row t
        c = col t
        pid = plotId t
        segN = if isTileInvalid ff (r-1, c+0) (val t) then [((r, r-1, 'H'), c)] else []
        segE = if isTileInvalid ff (r+0, c+1) (val t) then [((c, c+1, 'V'), r)] else []
        segS = if isTileInvalid ff (r+1, c+0) (val t) then [((r, r+1, 'H'), c)] else []
        segW = if isTileInvalid ff (r+0, c-1) (val t) then [((c, c-1, 'V'), r)] else []
        allSegs = segN ++ segE ++ segS ++ segW
        customSort = sortBy (\a b -> compare (fst a) (fst b))


-- Part 2 --

type FenceSegment = ((Int, Int, Char), Int)

part2 :: String -> Int
part2 ss = sum $ map (getPrice' ff) $ groupedPlots
    where
        ff = strToFarm ss
        groupedPlots = getAllPlotsGrouped $ markAllPlots 0 $ ff

getPrice' :: Farm -> Plot -> Int
getPrice' ff plot = getArea plot * getPerimeter' ff plot

getPerimeter' :: Farm -> Plot -> Int
getPerimeter' ff plot = length $ nub $ concat $ groupedBySide
    where
        fenceSegs = getFenceSegments ff plot
        groupedByLine = groupBy (\a b -> fst a == fst b) fenceSegs
        groupedBySide = [ groupAdjacent line 0 [] | line <- groupedByLine ]

groupAdjacent :: [FenceSegment] -> Int -> [FenceSegment] -> [FenceSegment]
groupAdjacent (a:[])   groupId acc = acc ++ [(fst a, groupId)]
groupAdjacent (a:b:xs) groupId acc
    | snd b - snd a <= 1 = groupAdjacent (b:xs) (groupId)     newAcc
    | otherwise          = groupAdjacent (b:xs) (groupId + 1) newAcc
    where
        newAcc = acc ++ [(fst a, groupId)]
