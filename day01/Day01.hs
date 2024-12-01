module Day01 where

import Data.List

main = do
    answer_1_test <- sumOfDistances <$> readFile "test_input.txt"
    answer_1 <- sumOfDistances <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"
    putStrLn $ "Part 1: " ++ (show answer_1) ++ " (live)"

sumOfDistances :: String -> Int
sumOfDistances ss = sum $ map (\ (x,y) -> abs (x - y)) (sortedLists ss)

sortedLists :: String -> [(Int, Int)]
sortedLists ss = zip (sort leftList) (sort rightList)
    where
        leftList  = map (read . head . words) . lines $ ss
        rightList = map (read . last . words) . lines $ ss

