module Main where

import Data.List
import Data.Function.Memoize


main :: IO ()
main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    putStrLn $ "(test) Part 1 (6): " ++ (show answer_1_test)

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "(live) Part 1 (???): " ++ (show answer_1_live)


-- Data --

strToTokens :: String -> [String]
strToTokens = words . replaceChars "," ' ' . head . lines

strToDesigns :: String -> [String]
strToDesigns = tail . dropWhile (/="") . lines


-- Part 1 --

part1 :: String -> Int
part1 ss = length $ filter id testResults
    where
        allTokens = strToTokens ss
        allDesigns = strToDesigns ss
        testResults = map (isDesignPos allTokens) allDesigns

isDesignPosMemo :: [String] -> String -> Bool
isDesignPosMemo = memoize2 isDesignPos

isDesignPos :: [String] -> String -> Bool
isDesignPos      _ "" = True
isDesignPos tokens design
    | length candidateTokens == 0 = False
    | otherwise = length (filter id isPossibleMap) > 0
    where
        candidateTokens = [ t | t <- tokens, isPrefixOf t design ]
        isPossibleMap = map (trimToken) candidateTokens
        trimToken = (\t -> isDesignPosMemo tokens $ drop (length t) design)

-- Helpers --

replaceChars :: [Char] -> Char -> String -> String
replaceChars aa b cs = [ if elem c aa then b else c | c <- cs ]
