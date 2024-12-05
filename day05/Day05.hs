module Day05 where

import Data.List (group, nub, sort, sortBy)

main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_live) ++ " (live)"

    -- answer_2_test <- part2 <$> readFile "test_input.txt"
    -- putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"

    -- answer_2_live <- part2 <$> readFile "input.txt"
    -- putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


-- Part 1

part1 :: String -> Int
part1 = sum . map getMiddle . getValidUpdates

getMiddle :: [Int] -> Int
getMiddle xs = xs!!(length xs `div` 2)

getValidUpdates :: String -> [[Int]]
getValidUpdates ss = [ u | u <- getAllUpdates ss, u == customSort u rules ]
    where
        rules = getAllRules ss

getAllUpdates :: String -> [[Int]]
getAllUpdates = map stringToList . tail . dropWhile (/="") . lines

getAllRules :: String -> [[Int]]
getAllRules = map stringToList . takeWhile (/="") . lines

customSort :: [Int] -> [[Int]] -> [Int]
customSort xs rules = sortBy (\a b -> myCompare a b rules) xs

myCompare :: Int -> Int -> [[Int]] -> Ordering
myCompare a b rules = if matchingRule == [a,b] then LT else GT
    where
        matchingRule = head [ r | r <- rules, (sort r) == (sort [a,b]) ]


-- Part 2

part2 :: String -> Int
part2 ss = 0


-- Helpers

replaceChars :: [Char] -> Char -> String -> String
replaceChars aa b cs = [ if elem c aa then b else c | c <- cs ]

stringToList :: String -> [Int]
stringToList = map (read::String->Int) . words . replaceChars "|," ' '
