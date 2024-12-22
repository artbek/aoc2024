module Main where

import Data.List
import Data.Function.Memoize


main :: IO ()
main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    putStrLn $ "(test) Part 1 (6): " ++ (show answer_1_test)

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "(live) Part 1 (???): " ++ (show answer_1_live)

    answer_2_test <- part2 <$> readFile "test_input.txt"
    putStrLn $ "(test) Part 2 (16): " ++ (show answer_2_test)

    answer_2_live <- part2 <$> readFile "input.txt"
    putStrLn $ "(live) Part 2 (???): " ++ (show answer_2_live)


-- Data --

strToTokens :: String -> [String]
strToTokens = words . replaceChars "," ' ' . head . lines

strToDesigns :: String -> [String]
strToDesigns = tail . dropWhile (/="") . lines


-- Part 1 --

part1 :: String -> Int
part1 = length . filter (>0) . getTestResults

getTestResults :: String -> [Int]
getTestResults ss = map (isDesignPos allTokens) allDesigns
    where
        allTokens = strToTokens ss
        allDesigns = strToDesigns ss

isDesignPosMemo :: [String] -> String -> Int
isDesignPosMemo = memoize2 isDesignPos

isDesignPos :: [String] -> String -> Int
isDesignPos _       ""            = 1
isDesignPos tokens  design
    | length candidateTokens == 0 = 0
    | otherwise                   = sum isPossibleMap
    where
        candidateTokens = [ t | t <- tokens, isPrefixOf t design ]
        isPossibleMap   = map (trimToken) candidateTokens
        trimToken t     = isDesignPosMemo tokens (drop (length t) design)


-- Part 2 --

part2 :: String -> Int
part2 = sum . filter (>0) . getTestResults


-- Helpers --

replaceChars :: [Char] -> Char -> String -> String
replaceChars aa b cs = [ if elem c aa then b else c | c <- cs ]
