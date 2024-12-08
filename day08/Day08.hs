module Main where

import Data.List (nub, sort, tails)

main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_live) ++ " (live)"

    -- answer_2_test <- part2 <$> readFile "test_input.txt"
    -- putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"

    -- answer_2_live <- part2 <$> readFile "input.txt"
    -- putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


-- Data types --

type Pos = (Int, Int)
type Freq = Char
type Antenna = (Freq, Pos)
type City = ([Antenna], Int)


-- Part 1 --

part1 :: String -> Int
part1 ss = length $ nub $ getAllAntinodes city freqs
    where
        city = getCity ss
        freqs = getFreqs city

getCity :: String -> City
getCity ss = (antennas, gridSize)
    where
        grid = lines ss
        gridSize = length grid
        allSquares = addCoords grid
        antennas = filter (\x -> fst x /= '.') allSquares

addCoords :: [[Char]] -> [(Char, (Int, Int))]
addCoords grid = [ (x, (r,c)) | (r, rr) <- rows, (c, x) <- zip [0..] rr ]
    where
        rows = zip [0..] grid

getFreqs :: City -> [Freq]
getFreqs = nub . sort . map fst . fst

getAllAntinodes :: City -> [Freq] -> [Pos]
getAllAntinodes city []   = []
getAllAntinodes city (f:fs) =
    filter (isValidPos gridSize) antinodes ++ getAllAntinodes city fs
    where
        antennas = getAntennas f city
        antinodes = getGroupAntinodes antennas
        gridSize = snd city

getAntennas :: Freq -> City -> [Pos]
getAntennas freq city = map snd $ filter (\x -> fst x == freq) antennas
    where
        antennas = fst city

getGroupAntinodes :: [Pos] -> [Pos]
getGroupAntinodes = concat . map getPairAntinodes . getPairs

getPairs :: [Pos] -> [(Pos, Pos)]
getPairs pp = [ (fst g, h) | g <- groups, h <- snd g ]
    where
        groups = map (\x -> (head x, tail x)) $ init $ init $ tails pp

getPairAntinodes :: (Pos, Pos) -> [Pos]
getPairAntinodes (p1, p2) = [antinode_1, antinode_2]
    where
        d1 = (fst p1 - fst p2, snd p1 - snd p2)
        antinode_1 = (fst p1 + fst d1, snd p1 + snd d1)
        d2 = (fst p2 - fst p1, snd p2 - snd p1)
        antinode_2 = (fst p2 + fst d2, snd p2 + snd d2)

isValidPos :: Int -> Pos -> Bool
isValidPos gridSize p =
    row >= 0 && row < gridSize && col >= 0 && col < gridSize
    where
        row = fst p
        col = snd p


-- Part 2 --

part2 :: String -> Int
part2 = length
