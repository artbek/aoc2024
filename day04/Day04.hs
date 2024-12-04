module Day04 where

import Data.List (inits, tails, isPrefixOf, sort, transpose)

main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_live) ++ " (live)"

    answer_2_test <- part2 <$> readFile "test_input.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"

    answer_2_live <- part2 <$> readFile "input.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


-- Part 1

part1 :: String -> Int
part1 = getCountAll . lines

getLineCount :: String -> Int
getLineCount = length . filter (isPrefixOf "XMAS") . tails

getCount :: [String] -> Int
getCount = sum . map getLineCount

getCountAll :: [String] -> Int
getCountAll ss = getCount rot0 + getCount (rot45 rot0) +
                 getCount rot1 + getCount (rot45 $ rot1) +
                 getCount rot2 + getCount (rot45 $ rot2) +
                 getCount rot3 + getCount (rot45 $ rot3)
                 where rot0 = ss
                       rot1 = rot90 rot0
                       rot2 = rot90 rot1
                       rot3 = rot90 rot2

rot90 :: [String] -> [String]
rot90 = transpose . map reverse


-- Input:     Step 1 (rot45part):    Step 2 (rot45):
-- 123        32147                  3
-- 456         658                   26
-- 789          9                    159
--                                   48
--                                   7
rot45part :: [String] -> [String]
rot45part (s:[]) = [s]
rot45part   (ss) = line : rot45part peeledTopAndLeft
    where
        line = (reverse $ head ss) ++ (tail $ head $ transpose ss)
        peeledTopAndLeft = transpose $ tail $ transpose $ tail ss

rot45 :: [String] -> [String]
rot45 ss = transpose $ zipWith (++) padding (rot45part ss)
    where
        padding = inits $ take 140 $ repeat ' '


-- Part 2

part2 :: String -> Int
part2 ss = length $ filter (==True) (scanInput 0 0 (lines ss))

scanInput :: Int -> Int -> [[Char]] -> [Bool]
scanInput r c ss
    | c >= (length ss - 2) = []
    | otherwise = hasX (c1:c3:c5:c7:c9:"") : scanInput nextR nextC ss
    where
        nextR = mod (r+1) (length ss - 2)
        nextC = if (nextR == 0) then (c+1) else c
        c1 = ss!!r!!c
        c3 = ss!!r!!(c+2)
        c5 = ss!!(r+1)!!(c+1)
        c7 = ss!!(r+2)!!c
        c9 = ss!!(r+2)!!(c+2)

hasX :: [Char] -> Bool
hasX (a:b:'A':c:d:_) = elem [a,b,c,d] ["MMSS", "SSMM", "MSMS", "SMSM"]
hasX _ = False
