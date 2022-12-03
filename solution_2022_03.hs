{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- Day 3 / 2022
import Data.Char (isUpper, ord)
import Data.Text (pack, splitOn, unpack)
import GhcPlugins (panic)
import Test.HUnit

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  print (solve1 parsed)
  print (solve2 parsed)

parse :: String -> [String]
parse input = filter (not . null) (map unpack (splitOn "\n" (pack input)))

compartments :: String -> (String, String)
compartments rucksack = splitAt (length rucksack `div` 2) rucksack

findDouble :: (String, String) -> Char
findDouble (x, y) = head (filter (`elem` y) x)

findCommonElem :: [String] -> Char
findCommonElem (x : y : z : _) = head (filter (\c -> c `elem` x && c `elem` y) z)
findCommonElem _ = panic "Expected list with 3 elements"

priority :: Char -> Int
priority x
  | isUpper x = ord x - ord 'A' + 27
  | otherwise = ord x - ord 'a' + 1

solve1 :: [String] -> Int
solve1 rucksack = sum (map (priority . findDouble . compartments) rucksack)

solve2 :: [String] -> Int
solve2 rucksack = sum (map (priority . findCommonElem) (partition 3 rucksack))

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = take n xs : partition n (drop n xs)

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
