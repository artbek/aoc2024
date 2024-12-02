module Day01 where

import Data.List (sort)

main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"
    putStrLn $ "Part 1: " ++ (show answer_1_live) ++ " (live)"

    answer_2_test <- part2 <$> readFile "test_input.txt"
    answer_2_live <- part2 <$> readFile "input.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"
    putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


-- Part 1

part1 :: String -> Int
part1 = sum . (reportSafetyScores 0)


-- Part 2

part2 :: String -> Int
part2 = sum . (reportSafetyScores 1)


-- Helpers

parsedReports :: String -> [[Int]]
parsedReports = map ((map read :: [String] -> [Int]) . words) . lines

reportSafetyScores :: Int -> String -> [Int]
reportSafetyScores remainingStrikes ss = map (isReportStable remainingStrikes) allReports
    where
        allReports  = reportsAtoZ ++ reportsZtoA
        reportsAtoZ = parsedReports ss
        reportsZtoA = (map reverse . parsedReports) ss

isReportStable :: Int -> [Int] -> Int
isReportStable remainingStrikes (a:b:[])     = 1
isReportStable remainingStrikes (a:b:c:rest) =
    if isTrippleStable [a,b,c]
    then isReportStable remainingStrikes (b:c:rest)
    else
        if remainingStrikes > 0
        then
            if length stablePair > 1
            then isReportStable (remainingStrikes-1) (stablePair ++ rest)
            else 0
        else 0
        where
            stablePair = getStablePair a b c

getStablePair :: Int -> Int -> Int -> [Int]
getStablePair a b c
    | isTrippleStable [a,b] = [a,b]
    | isTrippleStable [b,c] = [b,c]
    | isTrippleStable [a,c] = [a,c]
    | otherwise             = []

isTrippleStable :: [Int] -> Bool
isTrippleStable (a:[])     = True
isTrippleStable (a:b:rest) =
    if (b - a) >= 1 && (b - a) <= 3
    then isTrippleStable (b:rest)
    else False
