module Main where

import Debug.Trace


main = do
    answer_1_test <- part1 (11, 7) <$> readFile "test_input.txt"
    putStrLn $ "(test) Part 1 (12): " ++ (show answer_1_test)

    answer_1_live <- part1 (101, 103) <$> readFile "input.txt"
    putStrLn $ "(live) Part 1: " ++ (show answer_1_live)

    -- answer_2_test <- part2 (11, 7) <$> readFile "test_input.txt"
    -- putStrLn $ "(test) Part 2 (???): \n" ++ (show answer_2_test)

    answer_2_live <- part2 (101, 103) <$> readFile "input.txt"
    putStrLn $ "(live) Part 2: " ++ (show answer_2_live)


-- Part 1 --

type Robot = (Pos, Vec)
type Pos   = (Int, Int)
type Vec   = (Int, Int)

strToRobot :: String -> Robot
strToRobot ss = ((c, r), (vc, vr))
    where
        [c, r, vc, vr] = map read $ words $ replaceChars ",=pv" ' ' ss

part1 :: (Int, Int) -> String -> Int
part1 (cols, rows) ss = q1 * q2 * q3 * q4
    where
        rr = map (moveRobot 100 (cols, rows)) $ map strToRobot $ lines ss
        q1 = countRobots      (0)      (0) (midC-1) (midR-1) rr
        q2 = countRobots (midC+1)      (0) (cols-1) (midR-1) rr
        q3 = countRobots      (0) (midR+1) (midC-1) (rows-1) rr
        q4 = countRobots (midC+1) (midR+1) (cols-1) (rows-1) rr
        midC = div (cols - 1) 2
        midR = div (rows - 1) 2

moveRobot :: Int -> (Int, Int) -> Robot -> Robot
moveRobot steps (cols, rows) ((c,r), (vc,vr)) = ((c',r'), (vc,vr))
    where
        c' = (c + (vc * steps)) `mod` cols
        r' = (r + (vr * steps)) `mod` rows

countRobots :: Int -> Int -> Int -> Int -> [Robot] -> Int
countRobots minC minR maxC maxR = length . filter isInQuadrant . map (fst)
    where
        isInQuadrant = (\x -> fst x >= minC && fst x <= maxC &&
                              snd x >= minR && snd x <= maxR)


-- Part 2 --

{-
  OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
  O                             O
  O                             O
  O                             O
  O                             O
  O              O              O
  O             OOO             O
  O            OOOOO            O
  O           OOOOOOO           O
  O          OOOOOOOOO          O
  O            OOOOO            O
  O           OOOOOOO           O
  O          OOOOOOOOO          O
  O         OOOOOOOOOOO         O
  O        OOOOOOOOOOOOO        O
  O          OOOOOOOOO          O
  O         OOOOOOOOOOO         O
  O        OOOOOOOOOOOOO        O
  O       OOOOOOOOOOOOOOO       O
  O      OOOOOOOOOOOOOOOOO      O
  O        OOOOOOOOOOOOO        O
  O       OOOOOOOOOOOOOOO       O
  O      OOOOOOOOOOOOOOOOO      O
  O     OOOOOOOOOOOOOOOOOOO     O
  O    OOOOOOOOOOOOOOOOOOOOO    O
  O             OOO             O
  O             OOO             O
  O             OOO             O
  O                             O
  O                             O
  O                             O
  O                             O
  OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
-}

part2 :: (Int, Int) -> String -> Int
part2 (cols, rows) ss = stepRobots (cols, rows) newRobots 0
    where
        robots = map strToRobot $ lines ss
        newRobots = map (moveRobot (11 + (76 * 101)) (cols, rows)) robots

stepRobots :: (Int, Int) -> [Robot] -> Int -> Int
stepRobots (cols, rows) robots  (-1) = -999
stepRobots (cols, rows) robots steps =
    trace ("Step " ++ show steps)
    trace (printRobots (cols, rows) robots)
    stepRobots (cols, rows) newRobots stepsLeft
        where
            newRobots = map (moveRobot 101 (cols, rows)) robots
            stepsLeft = steps - 1

printRobots :: (Int, Int) -> [Robot] -> String
printRobots (cols, rows) rr = unlines $ splitEvery cols tempStr
    where
        tempStr = [ getChar (c,r) rr | r <- rowIds, c <- colIds ]
        rowIds = [0..(rows-1)]
        colIds = [0..(cols-1)]
        getChar (x,y) vals
            | lookup (x,y) vals == Nothing = ' '
            | otherwise = 'O'

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as,bs) = splitAt n xs


-- Helpers

replaceChars :: [Char] -> Char -> String -> String
replaceChars aa b cs = [ if elem c aa then b else c | c <- cs ]
