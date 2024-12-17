module Advent.Day17 where

import Advent.Prelude
import Data.List.Extra (replace)
import Data.Bits (xor)

data Instruction = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv deriving (Enum, Show)
type Ptr         = Int
type Registers   = (Int, Int, Int) -- (A, B, C)
type Tape        = [Int]

main :: IO ()
main = readInput "data/Day17.txt" >>= \(registers, tape) -> do
  putStrLn $ "Registers = " <> show registers
  putStrLn $ "Tape = " <> show tape
  let (output, registers') = program 0 registers tape
  putStrLn $ "Registers = " <> show registers'
  putStrLn $ "Output = " <> show output

program :: Ptr -> Registers -> Tape -> ([Int], Registers)
program _   registers []   = ([], registers)
program ptr (a, b, c) tape = do
  let instruction  = toEnum $ tape !! ptr
  let litOperand   = tape !! (ptr + 1)
  let comboOperand = case litOperand of
        4 -> a; 5 -> b; 6 -> c; _ -> litOperand
  let xdv = floor @Double $ fromIntegral a / (2.0 ^ comboOperand)
  let registers' = case instruction of
        Adv -> (xdv, b, c)
        Bxl -> (a, b `xor` litOperand, c)
        Bst -> (a, comboOperand `mod` 8, c)
        Bxc -> (a, b `xor` c, c)
        Bdv -> (a, xdv, c)
        Cdv -> (a, b, xdv)
        _   -> (a, b, c) -- No modification to registers.
  let ptr' = case (instruction, a == 0) of
        (Jnz, False) -> litOperand -- Jump to literal operand.
        _            -> ptr + 2    -- Jump past operand.
  let outF = case instruction of
        Out -> (comboOperand `mod` 8:) -- Output.
        _   -> id                      -- No output.
  first outF $
    if ptr' >= length tape - 1
    then ([], registers')
    else program ptr' registers' tape

readInput :: String -> IO (Registers, Tape)
readInput = fmap (parse . lines) . readFile
 where
  parse = toTriple . (<&> parseRegister) . take 3 &&& parseOpCodes . last
  parseOpCodes = map read . drop 1 . words . replace "," " "
  parseRegister = read . last . words
  toTriple [a, b, c] = (a, b, c)
  toTriple _ = error "Expected 3 registers"
