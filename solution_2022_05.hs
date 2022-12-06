{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- Day 4 / 2022

import Data.Text (Text, pack, replace, splitOn, unpack)
import Test.HUnit

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  print ()

-- print (solve1 parsed)
-- print (solve2 parsed)

type Stack = [Char]

type Instruction = (Int, Int, Int)

type Output = ([Stack], [Instruction])

parse :: String -> Output
parse input = do
  let (a : b : _) = splitOn "\n\n" (pack input)
  let (m : _) = splitOn "1" a
  let stack = map (unpack . replace "-" "" . pack) (transposeString (filter (not . null) (map replaceAllUnnecessary (map unpack (splitOn "\n" m)))))
  let instructions = map mapInstruction (splitOn "\n" b)
  (stack, instructions)

mapInstruction :: Text -> Instruction
mapInstruction w = do
  let (x : y : z : _) = filter (not . null) (map (unpack) (splitOn "/" (replace " " "" (replace "move" "/" (replace "from" "/" (replace "to" "/" w))))))
  (stripAndParse x, stripAndParse y, stripAndParse z)

stripAndParse :: String -> Int
stripAndParse x = read (strip x)

strip :: String -> String
strip x = unpack (replace " " "" (pack x))

replaceAllUnnecessary :: String -> String
replaceAllUnnecessary t = unpack (removeBrackes (replace " [" "-" (replace "] " "-" (replace "] [" "" (pack t)))))

removeBrackes :: Text -> Text
removeBrackes x = replace "[" "" (replace "]" "" (replace " " "" x))

-- https://stackoverflow.com/questions/36962520/haskell-transposition-on-a-list-of-strings
transposeString :: [String] -> [String]
transposeString ([] : _) = []
transposeString x = (map head x) : transposeString (map tail x)

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = take n xs : partition n (drop n xs)

-- Tests

exampleInput :: String
exampleInput = "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"

exampleList :: Output
exampleList = (["NZ", "DCM", "P"], [(1, 2, 1), (3, 1, 3), (2, 2, 1), (1, 1, 2)])

tests :: Test
tests =
  TestList
    [ TestCase (assertEqual "Parse Example" exampleList (parse exampleInput))
    -- TestCase (assertEqual "Part1 Example" 2 (solve1 exampleList)),
    -- TestCase (assertEqual "Part2 Example" 4 (solve2 exampleList))
    ]
