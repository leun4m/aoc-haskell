{-# OPTIONS -Wunused-imports #-}
-- {-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- Day 4 / 2022

import Data.Text (Text, pack, replace, splitOn, unpack)
import Test.HUnit

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  print (solve1 parsed)

-- print (solve2 parsed)

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

solve1 :: Output -> String
-- solve1 x | trace ("solve1" ++ show x) False = undefined
solve1 x = concatMap (take 1) (applyInstructions x)

applyInstructions :: Output -> [Stack]
applyInstructions (s, []) = s
applyInstructions (s, x : xs) = applyInstructions (applyInstruction s x, xs)

applyInstruction :: [Stack] -> Instruction -> [Stack]
applyInstruction w (0, _, _) = w
applyInstruction w (x, y, z) = do
  let item = take 1 (w !! (y - 1))
  let newStack = zipWith (\i s -> switchIt (i, s) y z item) [1 ..] w
  applyInstruction newStack (x -1, y, z)

switchIt :: (Int, Stack) -> Int -> Int -> [Char] -> Stack
switchIt (i, s) y z item
  | i == y = drop 1 s
  | i == z = item ++ s
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
      TestCase (assertEqual "Part1 Example" "CMZ" (solve1 exampleList))
      -- TestCase (assertEqual "Part2 Example" 4 (solve2 exampleList))
    ]
