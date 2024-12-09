module Main where

main = do
    answer_1_test <- part1 <$> readFile "test_input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_test) ++ " (test)"

    answer_1_live <- part1 <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show answer_1_live) ++ " (live)"

    -- answer_2_test <- part2 <$> readFile "test_input.txt"
    -- putStrLn $ "Part 2: " ++ (show answer_2_test) ++ " (test)"

    -- answer_2_live <- part2 <$> readFile "input.txt"
    -- putStrLn $ "Part 2: " ++ (show answer_2_live) ++ " (live)"


{-

-- TEST CASE 1 --

12345

0..111....22222
02.111....2222.
022111....222..
0221112...22...
02211122..2....
022111222......

[0,2,2,1,1,1,2,2,2] ==> 60


-- TEST CASE 2 --

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

type Disk = [Int]


-- Part 1 --

part1 :: String -> Int
part1 ss = getChecksum compressedDisk
    where
        compressedDisk = compressDisk (length disk) disk
        disk = stringToDisk 0 $ init ss

stringToDisk :: Int -> String -> Disk
stringToDisk fileId []       = []
stringToDisk fileId (a:[])   = take (read [a]) $ repeat fileId
stringToDisk fileId (a:b:ss) = blocks ++ spaces ++ stringToDisk (fileId+1) ss
    where
        blocks = take (read [a]) $ repeat fileId
        spaces = take (read [b]) $ repeat (-1)

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
getChecksum = sum . zipWith (*) [0..] . filter (>=0)


-- Part 2 --

part2 :: String -> Int
part2 = length


