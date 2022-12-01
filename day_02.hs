{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- Day 2 / 2020

import Data.Text
import Debug.Trace
import System.IO
import Text.Regex.Posix

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  print (solve1 parsed)
  print (solve2 parsed)

data InputRow = Row
  { -- | The first number
    num1 :: !Int,
    -- | The second number
    num2 :: !Int,
    -- | The letter
    letter :: !Char,
    -- | The actual password
    password :: !String
  }
  deriving (Show, Eq)

parse :: String -> [InputRow]
parse x = do
  Prelude.map parseLine (Prelude.lines x)

-- 1-3 a: abcde
-- 1-3 b: cdefg
-- 2-9 c: ccccccccc
parseLine :: String -> InputRow
parseLine line = do
  let numbers = getAllTextMatches (line =~ ("[0-9]+" :: String) :: AllTextMatches [] String)
  let strings = getAllTextMatches (line =~ ("[a-z]+" :: String) :: AllTextMatches [] String)
  let minRange = read (numbers !! 0) :: Int
  let maxRange = read (numbers !! 1) :: Int
  let letter = Prelude.head (Prelude.head strings)
  let password = strings !! 1
  Row minRange maxRange letter password

solve1 :: [InputRow] -> Integer
solve1 (x : rest) = (if isCorrectPassword1 x then 1 else 0) + solve1 rest
solve1 _ = 0

solve2 :: [InputRow] -> Integer
solve2 (x : rest) = (if isCorrectPassword2 x then 1 else 0) + solve2 rest
solve2 _ = 0

isCorrectPassword1 :: InputRow -> Bool
isCorrectPassword1 x = do
  let count = Prelude.length (Prelude.filter (\c -> c == letter x) (password x))
  num1 x <= count && count <= num2 x

isCorrectPassword2 :: InputRow -> Bool
isCorrectPassword2 x = do
  (password x !! (num1 x - 1) == letter x) /= (password x !! (num2 x - 1) == letter x)
