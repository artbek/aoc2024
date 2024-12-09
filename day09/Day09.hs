module Main where

import Data.Array
import Data.Char
import Data.List

main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"

    -- answer_1_live <- part1 <$> readFile "input.txt"
    -- putStrLn $ "Part 1: " ++ (show answer_1_live) ++ " (live)"

    answer_2_test <- part2 <$> readFile "test_input.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"

    answer_2_live <- part2 <$> readFile "input.txt"
    putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"



type Disk = [Int]


-- Part 1 --

part1 :: String -> Int
part1 ss = getChecksum compressedDisk
    where
        compressedDisk = compressDisk (length disk) disk
        disk = strToDisk 0 $ init ss

strToDisk :: Int -> String -> Disk
strToDisk fileId []       = []
strToDisk fileId (a:[])   = take (read [a]) $ repeat fileId
strToDisk fileId (a:b:ss) = blocks ++ spaces ++ strToDisk (fileId+1) ss
    where
        blocks = take (read [a]) $ repeat fileId
        spaces = take (read [b]) $ repeat (-1)

{-
    2333133121414131402

    00...111...2...333.44.5555.6666.777.888899
    009..111...2...333.44.5555.6666.777.88889.
    0099.111...2...333.44.5555.6666.777.8888..
    00998111...2...333.44.5555.6666.777.888...
    009981118..2...333.44.5555.6666.777.88....
    0099811188.2...333.44.5555.6666.777.8.....
    009981118882...333.44.5555.6666.777.......
    0099811188827..333.44.5555.6666.77........
    00998111888277.333.44.5555.6666.7.........
    009981118882777333.44.5555.6666...........
    009981118882777333644.5555.666............
    00998111888277733364465555.66.............
    0099811188827773336446555566..............

    [0,0,9,9,8,1,1,1,8,8,8,2,7,7,7,3,3,3,6,4,4,6,5,5,5,5,6,6] ==> 1928
-}

compressDisk :: Int -> Disk -> Disk
compressDisk diskLen disk
    | isContinuous disk = disk
    | otherwise =
    if isContinuous newDisk
        then newDisk
        else compressDisk diskLen newDisk
    where
        blockIdx = diskLen - getIndexOfFirstBlock 0 (reverse disk) - 1
        spaceIdx = getIndexOfFirstSpace 0 disk
        newDisk  = swap spaceIdx blockIdx disk

swap :: Int -> Int -> Disk -> Disk
swap s1 b2 disk = step2
    where
        step1 = take s1 disk ++ [disk!!b2] ++ drop (s1+1) disk
        step2 = take b2 step1 ++ [-1] ++ drop (b2+1) step1

getIndexOfFirstBlock :: Int -> Disk -> Int
getIndexOfFirstBlock index (s:ss) =
    if isBlock
        then index
        else getIndexOfFirstBlock nextIndex ss
    where
        isBlock = s >= 0
        nextIndex = index + 1

getIndexOfFirstSpace :: Int -> Disk -> Int
getIndexOfFirstSpace index (s:ss) =
    if isSpace
        then index
        else getIndexOfFirstSpace nextIndex ss
    where
        isSpace = s == -1
        nextIndex = index + 1

isContinuous :: Disk -> Bool
isContinuous (a:[]) = True
isContinuous (a:b:ss) =
    if a == -1 && b > -1
        then False
        else isContinuous (b:ss)

getChecksum :: Disk -> Int
getChecksum = sum . zipWith (*) [0..] . map (max 0)


-- Part 2 --

data File = File { len :: Int, val :: Int, idx :: Int } deriving (Eq, Show)

part2 :: String -> Int
part2 ss = getChecksum $ diskToInts compressedDisk
    where
        compressedDisk = compressDisk' maxVal disk
        maxVal = last $ sort $ diskToInts disk
        disk = strToDisk' 0 0 $ init ss

diskToInts :: [File] -> [Int]
diskToInts ff = concat [ take (len f) $ repeat (val f) | f <- ff ]

strToDisk' :: Int -> Int -> String -> [File]
strToDisk' fileId i (a:[])   = [File { len = read [a], val = fileId, idx = i }]
strToDisk' fileId i (a:b:ss) = diskFrag ++ strToDisk' (fileId + 1) nextIdx2 ss
    where
        file = File { len = read [a], val = fileId, idx = i }
        nextIdx = idx file + len file
        space = File { len = read [b], val = -1, idx = nextIdx }
        nextIdx2 = idx space + len space
        diskFrag = [file, space]

{-
    2333133121414131402

    00...111...2...333.44.5555.6666.777.888899
    0099.111...2...333.44.5555.6666.777.8888..
    0099.1117772...333.44.5555.6666.....8888..
    0099.111777244.333....5555.6666.....8888..
    00992111777.44.333....5555.6666.....8888..

    [0,0,9,9,2,1,1,1,7,7,7,-1,4,4,-1,3,3,3,-1,-1,-1,-1,
                          5,5,5,5,-1,6,6,6,6,-1,-1,-1,-1,-1,8,8,8,8] ==> 2858
-}

compressDisk' :: Int -> [File] -> [File]
compressDisk' (-1)    ff = ff
compressDisk' fileVal ff
    | foundSpaceMaybe == Nothing = compressDisk' (fileVal - 1) ff
    | otherwise = compressDisk' (fileVal - 1) newDisk
    where
        foundFile = findFile fileVal ff
        foundSpaceMaybe = findSpaceForFile foundFile ff
        (Just foundSpace) = foundSpaceMaybe
        diskUnsorted = filter (\f -> f /= foundFile && f /= foundSpace) ff
        newFile = foundFile { idx = idx foundSpace }
        erasedFile = foundFile { val = -1 }
        newSpaceLen = len foundSpace - len foundFile
        newSpace = foundSpace { idx = idx newFile + 1 , len = newSpaceLen }
        newDisk = customSort $ diskUnsorted ++ [newFile, newSpace, erasedFile]

findFile :: Int -> [File] -> File
findFile value (f:fs)
    | val f == value = f
    | otherwise = findFile value fs

findSpaceForFile :: File -> [File] -> Maybe File
findSpaceForFile foundFile [] = Nothing
findSpaceForFile foundFile (s:ss)
    | idx s > idx foundFile = Nothing
    | val s == -1 && len s >= len foundFile = Just s
    | otherwise = findSpaceForFile foundFile ss

customSort :: [File] -> [File]
customSort = sortBy (\f1 f2 -> customCompare f1 f2)
    where
        customCompare = (\f1 f2 -> if idx f1 < idx f2 then LT else GT)


-- Helpers --

diskToString :: [File] -> String
diskToString = map (\x -> if x < 0 then '.' else intToDigit x) . diskToInts
