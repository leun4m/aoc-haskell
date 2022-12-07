{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- Day 4 / 2022

import Data.Text (Text, pack, replace, splitOn, unpack)
import Test.HUnit
import Debug.Trace ( trace )

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
  let (a : b : _) = splitOn "\n\n" (pack input)
  let (m : _) = splitOn "1" a
  (parseStacks m, parseInstructions b)

parseStacks :: Text -> [Stack]
parseStacks w | trace ("parseStacks: " ++ show w) False = undefined
parseStacks m = map (unpack . replace "-" "" . pack) (transposeString (filter (not . null) (map (replaceAllUnnecessary . unpack) (splitOn "\n" m))))

parseInstructions :: Text -> [Instruction]
parseInstructions w | trace ("parseInstructions: " ++ show w) False = undefined
parseInstructions b = map mapInstruction (filter (not . null . unpack) (splitOn "\n" b))

mapInstruction :: Text -> Instruction
mapInstruction w | trace ("mapInstruction " ++ show w) False = undefined
mapInstruction w = do
  let (x : y : z : _) = filter (not . null) (map unpack (splitOn "/" (replace " " "" (replace "move" "/" (replace "from" "/" (replace "to" "/" w))))))
  (stripAndParse x, stripAndParse y, stripAndParse z)

stripAndParse :: String -> Int
stripAndParse x = read (strip x)

strip :: String -> String
strip x = unpack (replace " " "" (pack x))

replaceAllUnnecessary :: String -> String
replaceAllUnnecessary t = unpack (removeBrackes (replace "    " "-" (pack t)))

removeBrackes :: Text -> Text
removeBrackes x = replace "[" "" (replace "]" "" (replace " " "" x))

-- https://stackoverflow.com/questions/36962520/haskell-transposition-on-a-list-of-strings
transposeString :: [String] -> [String]
transposeString ([] : _) = []
transposeString x | trace ("transpose: " ++ show x) False = undefined
transposeString x = map head x : transposeString (map tail x)

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = take n xs : partition n (drop n xs)

solve1 :: Output -> String
solve1 (x,y) | trace ("solve1" ++ show x) False = undefined
solve1 x = concatMap (take 1) (applyInstructions x)

applyInstructions :: Output -> [Stack]
applyInstructions (s, []) = s
applyInstructions (s, x : xs) = applyInstructions (applyI s x, xs)

applyI :: [Stack] -> Instruction -> [Stack]
applyI s (0, y, z) = s
applyI s (x, y, z) = do
  let elem = take 1 (s !! (y - 1))
  let m = zipWith (\i s -> switchIt (i, s) y z elem) [1 ..] s
  applyI m (x -1, y, z)

switchIt :: (Int, Stack) -> Int -> Int -> [Char] -> Stack
switchIt (i, s) y z elem
  | i == y = drop 1 s
  | i == z = elem ++ s
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
