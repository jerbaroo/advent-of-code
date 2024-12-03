module Advent.Day3 where

import Advent.Prelude
import Data.Char (isDigit)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC

data Instruction = Do | Don't | Mul (Int, Int) deriving Show

main :: IO ()
main = readMuls >>= \case
  Left  err          -> putStrLn $ "Error: " <> err
  Right instructions -> -- Print part 1 and part 2.
    print $ (run Nothing 0 &&& run (Just True) 0) instructions

readMuls :: IO (Either String [Instruction])
readMuls = readFile "data/Day3.txt"
  <&> left show . M.runParser parseInstructions ""

parseInstructions :: Parsec Void String [Instruction]
parseInstructions =
  let go = M.try parseInstruction <|> M.try (M.anySingle >> go) in M.many go

parseInstruction :: Parsec Void String Instruction
parseInstruction =
      M.try (MC.string "do()" $> Do)
  <|> M.try (MC.string "don't()" $> Don't)
  <|> Mul <$> parseMul

parseMul :: Parsec Void String (Int, Int)
parseMul = do
  _ <- MC.string "mul("
  x <- read <$> M.many (M.satisfy isDigit)
  _ <- M.single ','
  y <- read <$> M.many (M.satisfy isDigit)
  _ <- M.single ')'
  pure (x, y)

-- | Run a sequence of 'Instruction'.
--
-- First argument must be 'Just' to take into account Do/Don't:
--   Nothing: ignore Do and Don't instructions.
--   Just True: Mul instructions are enabled.
--   Just False: Mul instructions are disabled.
run :: Maybe Bool -> Int -> [Instruction] -> Int
run _ x [] = x
run doMay x (Do:xs) = run (doMay $> True) x xs
run doMay x (Don't:xs) = run (doMay $> False) x xs
run doMay@(Just False) x (Mul _:xs) = run doMay x xs -- Mul disabled.
run doMay x (Mul (a, b):xs) = run doMay (x + a * b) xs
