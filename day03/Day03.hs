module Day02 where

import Data.List (tails, isPrefixOf)
import Data.Char (isDigit)

main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_live) ++ " (live)"

    answer_2_test <- part2 <$> readFile "test_input2.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"

    answer_2_live <- part2 <$> readFile "input.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


-- Part 1

part1 :: String -> Int
part1 = sum . map mul . pairsOfNumbers . candidates

candidates :: String -> [String]
candidates = filter (isPrefixOf "mul(") . map (take 12) . tails

getStuffInBrackets :: String -> String
getStuffInBrackets ss = if length tokens > 1 then (tokens!!1) else ""
    where
        tokens = words $ replaceChars "()" ' ' $ replaceChars " " '_' ss

pairsOfNumbers :: [String] -> [[String]]
pairsOfNumbers = map $ words . (replaceChars "," ' ') . getStuffInBrackets

mul :: [String] -> Int
mul ([])    = 0
mul (a:[])  = 0
mul (a:b:_) = if argsAreValid then argOne * argTwo else 0
    where
        argsAreValid = argIsValid a && argIsValid b
        argOne = read a :: Int
        argTwo = read b :: Int

argIsValid :: String -> Bool
argIsValid aa = all isDigit aa && elem (length aa) [1..3]


-- Part 2

part2 :: String -> Int
part2 = sum . map mul . pairsOfNumbers . candidates . parse Enabled

data Mode = Enabled | Disabled deriving (Eq)

parse :: Mode -> String -> String
parse mode [] = ""
parse mode (x:xs)
    | isPrefixOf "don't()" (x:xs) = x : (parse Disabled xs)
    | isPrefixOf "do()" (x:xs)    = x : (parse Enabled xs)
    | mode == Enabled             = x : (parse mode xs)
    | mode == Disabled            = '_' : (parse mode xs)


-- Helpers

replaceChars :: [Char] -> Char -> String -> String
replaceChars aa b cs = [ if elem c aa then b else c | c <- cs ]
