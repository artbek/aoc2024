module Main where

import Data.List (sort)
import Debug.Trace


main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    putStrLn $ "(test) Part 1 (480): " ++ (show answer_1_test)

    answer_1_2_test <- part1 <$> readFile "test_input_2.txt"
    putStrLn $ "(test) Part 1.2 (168): " ++ (show answer_1_2_test)

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "(live) Part 1: " ++ (show answer_1_live)


-- Part 1 --

strToInput :: [String] -> [(Int,Int,Int,Int,Int,Int)]
strToInput [] = []
strToInput (line_a:line_b:line_p:_:ss) = [(x1,y1,p1,x2,y2,p2)] ++ strToInput ss
    where
        buttonAdata = words line_a
        x1 = read $ init $ tail $ dropWhile (/='+') (buttonAdata!!2)
        y1 = read $ tail $ dropWhile (/='+') (buttonAdata!!3)

        buttonBdata = words line_b
        x2 = read $ init $ tail $ dropWhile (/='+') (buttonBdata!!2)
        y2 = read $ tail $ dropWhile (/='+') (buttonBdata!!3)

        prizeData = words line_p
        p1 = read $ init $ tail $ dropWhile (/='=') (prizeData!!1)
        p2 = read $ tail $ dropWhile (/='=') (prizeData!!2)

part1 :: String -> Int
part1 = sum . map findSolution . strToInput . lines

calculateCost :: (Int,Int) -> Int
calculateCost (a,b) = a * 3 + b

findSolution :: (Int,Int,Int,Int,Int,Int) -> Int
findSolution (x1,y1,p1,x2,y2,p2)
    | hasFoundSolution = cheapestSolution
    | otherwise     = 0
    where
        solutions = [ (a,b) | a <- [0..100], b <- [0..100], testX a b && testY a b ]
        testX a b = (a*x1)+(b*x2) == p1
        testY a b = (a*y1)+(b*y2) == p2
        hasFoundSolution = length solutions > 0
        cheapestSolution = head $ sort $ map calculateCost solutions
