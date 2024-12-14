module Main where

import Data.Function.Memoize


main = do
    let answer_1_test = part1 "125 17"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_live) ++ " (live)"

    -- let answer_2_test = part2 "125 17"
    -- putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"

    answer_2_live <- part2 <$> readFile "input.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


maxBlinksP1 = 25 :: Int
maxBlinksP2 = 75 :: Int

-- Part 1 --

part1 :: String -> Int
part1 = length . last . take (maxBlinksP1 + 1) . iterate blink . words

{-
    0) 125 17
    1) 253000 1 7
    2) 253 0 2024 14168
    3) 512072 1 20 24 28676032
    4) 512 72 2024 2 0 2 4 2867 6032
    5) 1036288 7 2 20 24 4048 1 4048 8096 28 67 60 32
    6) 2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2
-}
blink :: [String] -> [String]
blink = concat . map processStone

processStone :: String -> [String]
processStone ss
    | ss == "0" = ["1"]
    | (stoneLen) `mod` 2 == 0 = [dz $ take halfStone ss, dz $ drop halfStone ss]
    | otherwise = [ dz $ show $ (read ss) * 2024]
    where
        stoneLen = length ss
        halfStone = stoneLen `div` 2

-- drop leading zeros
dz :: String -> String
dz ss = dropWhile (=='0') (init ss) ++ [last ss]


-- Part 2 --

part2 :: String -> Int
part2 = sum . map (blink2 (maxBlinksP2 - 1)) . map read . words

blink2Memo :: Int -> Int -> Int
blink2Memo = memoize2 blink2

blink2 :: Int -> Int -> Int
blink2 0 stone = length $ processStone' stone
blink2 counter stone = sum $ map (blink2Memo (counter - 1)) newStones
    where
        newStones = processStone' stone

processStone' :: Int -> [Int]
processStone' stone
    | stone == 0 = [1]
    | (stoneLen) `mod` 2 == 0 = map read [take halfStone ss, drop halfStone ss]
    | otherwise = [stone * 2024]
    where
        ss = show stone
        stoneLen = length ss
        halfStone = stoneLen `div` 2
