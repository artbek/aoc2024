module Main where

import Data.Bits (xor)
import Debug.Trace


main = do
    answer_test <- part1 <$> readFile "test_input_1.txt"
    putStrLn $ "(test) Part 1 (4,6,3,5,6,3,5,2,1,0): " ++ (show answer_test)

    answer_1 <- part1 <$> readFile "input.txt"
    putStrLn $ "(live) Part 1 (???): " ++ (show answer_1)

    -- answer_test <- part2 0 <$> strToProg <$> readFile "test_input_2.txt"
    -- putStrLn $ "(test) Part 2 (117440): " ++ (show answer_test)

    answer_2 <- part2 (2^45+6) <$> strToProg <$> readFile "input.txt"
    putStrLn $ "(live) Part 2 (???): " ++ (show answer_2)

-- Data --

data Regs = Regs { a :: Int, b :: Int, c :: Int, ip :: Int } deriving Show

type Program = [Int]
type Opcode  = Int
type Operand = Int


-- Part 1 --

initialRegs = Regs 729 0 0 0

strToRegs :: String -> Regs
strToRegs ss = Regs regA regB regC 0
    where
        regA = read $ last $ words $ (!! 0) $ ll
        regB = read $ last $ words $ (!! 1) $ ll
        regC = read $ last $ words $ (!! 2) $ ll
        ll = lines ss

strToProg :: String -> Program
strToProg ss = program
    where
        progStr = last $ words $ last $ lines ss
        program = map (read) $ words $ replaceChars "," ' ' progStr

part1 :: String -> String
part1 ss = show output
    where
        initialRegs    = strToRegs ss
        program        = strToProg ss
        (regs, output) = processAll initialRegs program []

processAll :: Regs -> Program -> [Int] -> (Regs, [Int])
processAll regs program acc
    | doHalt    = (regs, acc)
    | otherwise = processAll regs' program (acc ++ output)
    where
        doHalt          = ip regs >= length program
        opcode          = program!!(ip regs)
        operand         = program!!(ip regs + 1)
        (regs', output) = process regs opcode operand


process :: Regs -> Opcode -> Operand -> (Regs, [Int])
process regs 0 operand = adv regs operand
process regs 1 operand = bxl regs operand
process regs 2 operand = bst regs operand
process regs 3 operand = jzn regs operand
process regs 4 operand = bxc regs operand
process regs 5 operand = out regs operand
process regs 6 operand = bdv regs operand
process regs 7 operand = cdv regs operand

-- The adv instruction (opcode 0) performs division. The numerator is the value in the A register.
-- The denominator is found by raising 2 to the power of the instruction's combo operand. (So,
-- an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The result
-- of the division operation is truncated to an integer and then written to the A register.
adv :: Regs -> Operand -> (Regs, [Int])
adv regs operand = (regs', [])
    where
        v     = getOperandValue regs operand
        res   = a regs `div` (2^v)
        ip'   = ip regs + 2
        regs' = regs { a = res, ip = ip' }

-- The bxl instruction (opcode 1) calculates the bitwise XOR of register B and
-- the instruction's literal operand, then stores the result in register B.
bxl :: Regs -> Operand -> (Regs, [Int])
bxl regs operand = (regs', [])
    where
        v     = operand
        res   = xor (b regs) v
        ip'   = ip regs + 2
        regs' = regs { b = res, ip = ip' }

-- The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby
-- keeping only its lowest 3 bits), then writes that value to the B register.
bst :: Regs -> Operand -> (Regs, [Int])
bst regs operand = (regs', [])
    where
        v     = getOperandValue regs operand
        res   = mod v 8
        ip'   = ip regs + 2
        regs' = regs { b = res, ip = ip'}

-- The jnz instruction (opcode 3) does nothing if the A register is 0. However, if
-- the A register is not zero, it jumps by setting the instruction pointer to the value of
-- its literal operand; if this instruction jumps, the instruction pointer is not increased
-- by 2 after this instruction.
jzn :: Regs -> Operand -> (Regs, [Int])
jzn regs operand
    | a regs > 0 = (regs', [])
    | otherwise  = (regs'', [])
    where
        v      = operand
        ip'    = v
        regs'  = regs { ip = ip' }
        ip''   = ip regs + 2
        regs'' = regs { ip = ip'' }

-- The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C,
-- then stores the result in register B. (For legacy reasons, this instruction reads
-- an operand but ignores it.)
bxc :: Regs -> Operand -> (Regs, [Int])
bxc regs operand = (regs', [])
    where
        res   = xor (b regs) (c regs)
        ip'   = ip regs + 2
        regs' = regs { b = res, ip = ip' }

-- The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then
-- outputs that value. (If a program outputs multiple values, they are separated by commas.)
out :: Regs -> Operand -> (Regs, [Int])
out regs operand = (regs', [res])
    where
        v     = getOperandValue regs operand
        res   = mod v 8
        ip'   = ip regs + 2
        regs' = regs { ip = ip' }

-- The bdv instruction (opcode 6) works exactly like the adv instruction except that the result
-- is stored in the B register. (The numerator is still read from the A register.)
bdv :: Regs -> Operand -> (Regs, [Int])
bdv regs operand = (regs', [])
    where
        v     = getOperandValue regs operand
        res   = a regs `div` (2^v)
        ip'   = ip regs + 2
        regs' = regs { b = res, ip = ip' }

-- The cdv instruction (opcode 7) works exactly like the adv instruction except that the result
-- is stored in the C register. (The numerator is still read from the A register.)
cdv :: Regs -> Operand -> (Regs, [Int])
cdv regs operand = (regs', [])
    where
        v     = getOperandValue regs operand
        res   = a regs `div` (2^v)
        ip'   = ip regs + 2
        regs' = regs { c = res, ip = ip' }


getOperandValue :: Regs -> Operand -> Int
getOperandValue regs o
    | elem o [0..3] = o
    | o == 4        = a regs
    | o == 5        = b regs
    | o == 6        = c regs
    | otherwise     = error "!!! INVALID OPERAND !!!"


-- Part 2 --

-- 2^45 - 2^48

part2 :: Int -> Program -> String
part2 counter program
    | output == program = show counter
    | drop 14 output == [3,0] = "CORRECT ENDING: " ++ show counter
    | length output > 16 = "!!! TOO FAR !!! (" ++ show counter ++ ")"
    | otherwise = trace (show counter ++ " " ++ show output)
                  part2 (counter') program
    where
        regs        = Regs counter 0 0 0
        (_, output) = processAll regs program []
        counter'    = counter + 10000000000


-- Helpers --

replaceChars :: [Char] -> Char -> String -> String
replaceChars aa b cs = [ if elem c aa then b else c | c <- cs ]
