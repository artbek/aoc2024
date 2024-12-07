module Main where

import Data.List (findIndex)
import Data.Maybe (fromMaybe)

main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_live) ++ " (live)"

    answer_2_test <- part2 <$> readFile "test_input.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"

    answer_2_live <- part2 <$> readFile "input.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


-- Data --

type Map = [[Char]]
type Pos = (Int, Int)
data Dir = N | E | S | W deriving (Eq, Show)


-- Part 1 --

part1 :: String -> Int
part1 = (+1) . countFootprints . getAllSteps

getAllSteps :: String -> Map
getAllSteps ss = placeChar '^' steps guardPos
    where
        mm       = getMap ss
        steps    = getSteps (mm, guardPos, N) (0, 6000)
        guardPos = findGuard mm 0

getMap :: String -> Map
getMap = lines

findGuard :: Map -> Int -> Pos
findGuard (r:rs) rowIdx =
    if elem '^' r
        then (rowIdx, fromMaybe 0 colIdx)
        else findGuard rs (rowIdx + 1)
    where
        colIdx = findIndex (== '^') r

countFootprints :: Map -> Int
countFootprints = length . filter (== 'x') . concat

getSteps :: (Map, Pos, Dir) -> (Int, Int) -> Map
getSteps (mm, pos, dir) (stepNum, maxSteps)
    | stepNum >= maxSteps  = [[]]
    | isOutsideTheMap pos mm = mm
    | isOnObstacle newPos mm = getSteps (newMap, pos, newDir) (stepNum, maxSteps)
    | otherwise              = getSteps (newMap, newPos, dir) (stepNum + 1, maxSteps)
    where
        newPos = nextPos pos dir
        newDir = turnRight dir
        newMap = placeChar 'x' mm pos

isOutsideTheMap :: Pos -> Map -> Bool
isOutsideTheMap pos mm =
    if row < 0 || row > maxRow || col < 0 || col > maxCol
        then True
        else False
    where
        row = fst pos
        col = snd pos
        maxRow = length mm - 1
        maxCol = length (mm!!0) - 1

isOnObstacle :: Pos -> Map -> Bool
isOnObstacle pos mm
    | isOutsideTheMap pos mm = False
    | otherwise = mm!!row!!col == '#'
    where
        row = fst pos
        col = snd pos

nextPos :: Pos -> Dir -> Pos
nextPos (row, col) N = (row - 1, col + 0)
nextPos (row, col) S = (row + 1, col + 0)
nextPos (row, col) E = (row + 0, col + 1)
nextPos (row, col) W = (row + 0, col - 1)

turnRight :: Dir -> Dir
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N


-- Part 2 --

part2 :: String -> Int
part2 = testPositions . getAllCandidates . getAllSteps

getAllCandidates :: Map -> ([Pos], Map)
getAllCandidates mm = (candidates, mm)
    where
        candidates      = map fst $ filter candidateTest stepsWithCoords
        candidateTest   = (\((r,c), x) -> x == 'x')
        stepsWithCoords = getStepsWithCoords mm

getStepsWithCoords :: Map -> [(Pos, Char)]
getStepsWithCoords mm = [ ((r,c), cc)
                        | (r,rr) <- zip [0..] mm, (c,cc) <- zip [0..] rr
                        ]

testPositions :: ([Pos], Map) -> Int
testPositions ([], _)      = 0
testPositions ((p:ps), mm) = loopScore candidateMap + testPositions (ps, mm)
    where
        candidateMap = placeChar '#' mm p

placeChar :: Char -> Map -> Pos -> Map
placeChar c mm pos = take row mm ++ [newRow] ++ drop (row + 1) mm
    where
        row = fst pos
        col = snd pos
        curRow = mm!!row
        newRow = take col curRow ++ [c] ++ drop (col + 1) curRow

loopScore :: Map -> Int
loopScore mm = if (steps == [[]]) then 1 else 0
    where
        guardPos = findGuard mm 0
        steps    = getSteps' (mm, guardPos, N) (0, 6000)

-- Faster version of getSteps
getSteps' :: (Map, Pos, Dir) -> (Int, Int) -> Map
getSteps' (mm, pos, dir) (stepNum, maxSteps)
    | stepNum >= maxSteps  = [[]]
    | isOutsideTheMap pos mm = mm
    | isOnObstacle newPos mm = getSteps' (newMap, pos, newDir) (stepNum, maxSteps)
    | otherwise              = getSteps' (newMap, newPos, dir) (stepNum + 1, maxSteps)
    where
        newPos = nextPos pos dir
        newDir = turnRight dir
        newMap = mm -- placeChar 'x' mm pos
