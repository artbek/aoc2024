module Main where

main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_live) ++ " (live)"

    answer_2_test <- part2 <$> readFile "test_input.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"

    answer_2_live <- part2 <$> readFile "input.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


type Equation = (Int, [Int])

-- Part 1 --

part1 :: String -> Int
part1 = sum . map fst . getPossibleEquations

getPossibleEquations :: String -> [Equation]
getPossibleEquations = filter (isEqPossible 0) . getAllEquations . lines

getAllEquations :: [String] -> [Equation]
getAllEquations [] = []
getAllEquations (s:ss) = (total, xs) : (getAllEquations ss)
    where
        ww = words s
        total = read $ init $ head ww
        xs = map (read) $ tail ww

isEqPossible :: Int -> (Int, [Int]) -> Bool
isEqPossible acc (total, (x:[])) = (t1 == total || t2 == total)
    where
        t1 = acc + x
        t2 = acc * x
isEqPossible acc (total, (x:xs)) = (c1 || c2)
    where
        c1 = isEqPossible (acc + x) (total, xs)
        c2 = isEqPossible (acc * x) (total, xs)


-- Part 2 --

part2 :: String -> Int
part2 = sum . map fst . getPossibleEquations'

getPossibleEquations' :: String -> [Equation]
getPossibleEquations' = filter (isEqPossible' 0) . getAllEquations . lines

isEqPossible' :: Int -> (Int, [Int]) -> Bool
isEqPossible' acc (total, (x:[])) = (t1 == total || t2 == total || t3 == total)
    where
        t1 = acc + x
        t2 = acc * x
        t3 = acc // x
isEqPossible' acc (total, (x:xs)) = (c1 || c2 || c3)
    where
        c1 = isEqPossible' (acc + x) (total, xs)
        c2 = isEqPossible' (acc * x) (total, xs)
        c3 = isEqPossible' (acc // x) (total, xs)

(//) :: Int -> Int -> Int
(//) a b = read (show a ++ show b)
