{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- Day 3 / 2022

import Data.Char (isUpper, ord)
import Data.Text (pack, splitOn, unpack)
import Test.HUnit

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  print (solve1 parsed)
  print (solve2 parsed)

parse :: String -> [String]
parse input = filter (not . null) (map unpack (splitOn "\n" (pack input)))

compartments :: String -> [String]
compartments rucksack = partition (length rucksack `div` 2) rucksack

priority :: Char -> Int
priority x
  | isUpper x = ord x - ord 'A' + 27
  | otherwise = ord x - ord 'a' + 1

solve1 :: [String] -> Int
solve1 rucksacks = sum (map (priority . head . commonElements . compartments) rucksacks)

solve2 :: [String] -> Int
solve2 rucksacks = sum (map (priority . head . commonElements) (partition 3 rucksacks))

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = take n xs : partition n (drop n xs)

commonElements :: Eq a => [[a]] -> [a]
commonElements [] = []
commonElements [x] = x
commonElements (x : xs) = filter (\z -> z `elem` commonElements xs) x

-- Tests

exampleInput :: String
exampleInput = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw\n"

exampleList :: [String]
exampleList =
  [ "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
  ]

tests :: Test
tests =
  TestList
    [ TestCase (assertEqual "Parse Example" exampleList (parse exampleInput)),
      TestCase (assertEqual "Part1 Example" 157 (solve1 exampleList)),
      TestCase (assertEqual "Part2 Example" 70 (solve2 exampleList))
    ]
