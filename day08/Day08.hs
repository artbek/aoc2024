module Main where

import Data.List (nub, sort, tails)

main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    putStrLn $ "Solution: " ++ (show answer_1_test) ++ " (test)"

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "Solution: " ++ (show answer_1_live) ++ " (live)"


-- Config --

-- choose getPairAntinodes1 for part 1 solution ...
-- getPairAntinodes = getPairAntinodes1
-- ... or getPairAntinodes2 for part 2 solution
getPairAntinodes = getPairAntinodes2


-- Data types --

type Pos = (Int, Int)
type Freq = Char
type Antenna = (Freq, Pos)
type City = ([Antenna], Int)


-- Part 1 --

getPairAntinodes1 :: City -> (Pos, Pos) -> [Pos]
getPairAntinodes1 city (p1, p2) = filter (isValidPos gridSize) [an1, an2]
    where
        gridSize = snd city
        d1 = (fst p1 - fst p2, snd p1 - snd p2)
        an1 = p1 <+> d1
        d2 = (fst p2 - fst p1, snd p2 - snd p1)
        an2 = p2 <+> d2

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
getAllAntinodes city (f:fs) = antinodes ++ getAllAntinodes city fs
    where
        antennas = getAntennas f city
        antinodes = getGroupAntinodes city antennas

getAntennas :: Freq -> City -> [Pos]
getAntennas freq city = map snd $ filter (\x -> fst x == freq) antennas
    where
        antennas = fst city

getGroupAntinodes :: City -> [Pos] -> [Pos]
getGroupAntinodes city = concat . map (getPairAntinodes city) . getPairs

getPairs :: [Pos] -> [(Pos, Pos)]
getPairs pp = [ (fst g, h) | g <- groups, h <- snd g ]
    where
        groups = map (\x -> (head x, tail x)) $ init $ init $ tails pp


isValidPos :: Int -> Pos -> Bool
isValidPos gridSize p =
    row >= 0 && row < gridSize && col >= 0 && col < gridSize
    where
        row = fst p
        col = snd p

(<+>) :: Pos -> Pos -> Pos
(<+>) v1 v2 = (fst v1 + fst v2, snd v1 + snd v2)


-- Part 2 --

part2 = part1

getPairAntinodes2 :: City -> (Pos, Pos) -> [Pos]
getPairAntinodes2 city (p1, p2) = filter (isValidPos gridSize) (an1 ++ an2)
    where
        gridSize = snd city
        d1 = (fst p1 - fst p2, snd p1 - snd p2)
        an1 = take 130 $ scanl (\b a -> b <+> a) p1 (repeat d1)

        d2 = (fst p2 - fst p1, snd p2 - snd p1)
        an2 = take 130 $ scanl (\b a -> b <+> a) p2 (repeat d2)
