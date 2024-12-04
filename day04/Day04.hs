module Day04 where

import Data.List (inits, tails, isPrefixOf, transpose)

main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_live) ++ " (live)"

    -- answer_2_test <- part2 <$> readFile "test_input2.txt"
    -- putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"

    -- answer_2_live <- part2 <$> readFile "input.txt"
    -- putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


-- Part 1

part1 :: String -> Int
part1 = getCountAll . lines

getLineCount :: String -> Int
getLineCount = length . filter (isPrefixOf "XMAS") . tails

getCount :: [String] -> Int
getCount = sum . map getLineCount

getCountAll :: [String] -> Int
getCountAll ss = getCount ss +
                 getCount (rot45 ss) +

                 getCount (rot90 ss) +
                 getCount (rot45 $ rot90 ss) +

                 getCount (rot90 $ rot90 ss) +
                 getCount (rot45 $ rot90 $ rot90 ss) +

                 getCount (rot90 $ rot90 $ rot90 ss) +
                 getCount (rot45 $ rot90 $ rot90 $ rot90 ss)

rot90 :: [String] -> [String]
rot90 = transpose . map reverse

-- 123
-- 456
-- 789

-- Step 1 (rot45p):
--
-- 32147
--  658
--   9

-- Step 2 (rot90):
--
-- 3
-- 26
-- 159
-- 48
-- 7

rot45p :: [String] -> [String]
rot45p (s:[]) = [s]
rot45p   (ss) = line : rot45p peeledTopAndLeft
    where
        line = (reverse $ head ss) ++ (tail $ head $ transpose ss)
        peeledTopAndLeft = transpose $ tail $ transpose $ tail ss

rot45 :: [String] -> [String]
rot45 ss = transpose $ zipWith (++) padding (rot45p ss)
    where
        padding = inits $ take 140 $ repeat ' '


-- Part 2

part2 :: String -> Int
part2 ss = 0

