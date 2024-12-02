module Day01 where

import Data.List (sort)

main = do
    answer_1_test <- sumOfDistances <$> readFile "test_input.txt"
    answer_1_live <- sumOfDistances <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"
    putStrLn $ "Part 1: " ++ (show answer_1_live) ++ " (live)"

    answer_2_test <- part2 <$> readFile "test_input.txt"
    answer_2_live <- part2 <$> readFile "input.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"
    putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


-- Part 1

sumOfDistances :: String -> Int
sumOfDistances ss = sum $ map (\ (x,y) -> abs (x - y)) (sortedLists ss)

sortedLists :: String -> [(Int, Int)]
sortedLists ss = zip (leftList ss) (rightList ss)


-- Part 2

countElement :: Int -> [Int] -> Int
countElement el []     = 0
countElement el (x:xs) = (if (el == x) then 1 else 0) + countElement el xs

similarityScore :: [Int] -> [Int] -> Int
similarityScore []     ys = 0
similarityScore (x:xs) ys = x * (countElement x ys) + similarityScore xs ys


part2 :: String -> Int
part2 ss = similarityScore (leftList ss) (rightList ss)


-- Helpers

makeList :: ([String] -> String) -> String -> [Int]
makeList f = sort . map (read . f . words) . lines

leftList :: String -> [Int]
leftList = makeList head

rightList :: String -> [Int]
rightList = makeList last
