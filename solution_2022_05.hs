{-# OPTIONS -Wunused-imports #-}
-- {-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- Day 5 / 2022

import Data.Text (Text, pack, replace, splitOn, unpack)
import Test.HUnit

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  print (solve1 parsed)
  print (solve2 parsed)

type Stack = [Char]

type Instruction = (Int, Int, Int)

type Output = ([Stack], [Instruction])

parse :: String -> Output
parse input = do
  let (stacksAndNumbers : instructions : _) = splitOn "\n\n" (pack input)
  let (stacks_ : _) = splitOn "1" stacksAndNumbers
  (parseStacks stacks_, parseInstructions instructions)

parseStacks :: Text -> [Stack]
-- parseStacks x | trace ("parseStacks: " ++ show x) False = undefined
parseStacks x = map (unpack . replace "-" "" . pack) (transposeString (filter (not . null) (map processStackLine (lines (unpack x)))))

parseInstructions :: Text -> [Instruction]
-- parseInstructions x | trace ("parseInstructions: " ++ show x) False = undefined
parseInstructions x = map parseInstruction (filter (not . null . unpack) (splitOn "\n" x))

parseInstruction :: Text -> Instruction
-- parseInstruction x | trace ("parseInstruction: " ++ show x) False = undefined
parseInstruction x = do
  let (move : from : to : _) = filter (not . null) (map unpack (splitOn "/" (replace "move" "/" (replace "from" "/" (replace "to" "/" x)))))
  (stripAndParse move, stripAndParse from, stripAndParse to)

stripAndParse :: String -> Int
stripAndParse x = read (filter (/= ' ') x)

processStackLine :: String -> String
processStackLine x = filter (`notElem` ['[', ']', ' ']) (unpack (replace "    " "-" (pack x)))

-- https://stackoverflow.com/questions/36962520/haskell-transposition-on-a-list-of-strings
transposeString :: [String] -> [String]
transposeString ([] : _) = []
-- transposeString x | trace ("transpose: " ++ show x) False = undefined
transposeString x = map head x : transposeString (map tail x)

solve :: [Stack] -> String
solve = concatMap (take 1) 

solve1 :: Output -> String
solve1 x = solve $ applyInstructions1 x

solve2 :: Output -> String
solve2 x = solve $ applyInstructions2 x

applyInstructions1 :: Output -> [Stack]
applyInstructions1 (s, []) = s
applyInstructions1 (s, x : xs) = applyInstructions1 (applyInstruction1 s x, xs)

applyInstructions2 :: Output -> [Stack]
applyInstructions2 (s, []) = s
applyInstructions2 (s, x : xs) = applyInstructions2 (applyInstruction2 s x, xs)

applyInstruction1 :: [Stack] -> Instruction -> [Stack]
applyInstruction1 w (0, _, _) = w
applyInstruction1 w (x, y, z) = do
  let item = take 1 (w !! (y - 1))
  let newStack = zipWith (\i s -> switchIt (i, s) 1 y z item) [1 ..] w
  applyInstruction1 newStack (x - 1, y, z)

applyInstruction2 :: [Stack] -> Instruction -> [Stack]
applyInstruction2 w (0, _, _) = w
applyInstruction2 w (x, y, z) = zipWith (\i s -> switchIt (i, s) x y z (take x (w !! (y - 1)))) [1 ..] w

switchIt :: (Int, Stack) -> Int -> Int -> Int -> [Char] -> Stack
switchIt (i, s) x y z w
  | i == y = drop x s
  | i == z = w ++ s
  | otherwise = s

-- Tests

exampleInput :: String
exampleInput = "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"

exampleList :: Output
exampleList = (["NZ", "DCM", "P"], [(1, 2, 1), (3, 1, 3), (2, 2, 1), (1, 1, 2)])

tests :: Test
tests =
  TestList
    [ TestCase (assertEqual "Parse Example" exampleList (parse exampleInput)),
      TestCase (assertEqual "Part1 Example" "CMZ" (solve1 exampleList)),
      TestCase (assertEqual "Part2 Example" "MCD" (solve2 exampleList))
    ]
