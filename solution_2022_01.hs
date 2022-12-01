{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- Day 1 / 2022

import Data.List (sort)
import Data.Maybe ( mapMaybe )
import Data.Text (Text, pack, splitOn, unpack)
import Test.HUnit ( assertEqual, Test(..) )
import Text.Read ( readMaybe )

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  print (solve1 parsed)
  print (solve2 parsed)

parse :: String -> [[Int]]
parse input = map parseGroup (splitOn "\n\n" (pack input))

parseGroup :: Text -> [Int]
parseGroup group = mapMaybe (readMaybe . unpack) (splitOn "\n" group)

solve1 :: [[Int]] -> Int
solve1 elves = maximum (map sum elves)

solve2 :: [[Int]] -> Int
solve2 elves = sum (take 3 (reverse (sort (map sum elves))))

-- Tests

exampleInput :: String
exampleInput = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"

exampleList :: [[Int]]
exampleList = [[1000, 2000, 3000], [4000], [5000, 6000], [7000, 8000, 9000], [10000]]

testParse :: Test
testParse = TestCase (assertEqual "Example" exampleList (parse exampleInput))

testPart1 :: Test
testPart1 = TestCase (assertEqual "Example" 24000 (solve1 exampleList))

testPart2 :: Test
testPart2 = TestCase (assertEqual "Example" 45000 (solve2 exampleList))

tests = TestList [
  TestLabel "Parse" testParse, 
  TestLabel "Part 1" testPart1,
  TestLabel "Part 2" testPart2]
