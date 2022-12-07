{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- Day 6 / 2022

import Data.Set as Set
import Test.HUnit

main :: IO ()
main = do
  input <- getContents
  print (solve1 input)
  print (solve2 input)

firstUniqueChars :: Int -> String -> Int
firstUniqueChars n x =
  if Set.size (Set.fromList (Prelude.take n x)) == n
    then n
    else 1 + firstUniqueChars n (tail x)

solve1 :: String -> Int
solve1 = firstUniqueChars 4

solve2 :: String -> Int
solve2 = firstUniqueChars 14

-- Tests

tests :: Test
tests =
  TestList
    [ TestCase (assertEqual "Part1 Example" 7 (solve1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")),
      TestCase (assertEqual "Part1 Example" 5 (solve1 "bvwbjplbgvbhsrlpgdmjqwftvncz")),
      TestCase (assertEqual "Part1 Example" 6 (solve1 "nppdvjthqldpwncqszvftbrmjlhg")),
      TestCase (assertEqual "Part1 Example" 10 (solve1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")),
      TestCase (assertEqual "Part1 Example" 11 (solve1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")),
      TestCase (assertEqual "Part2 Example" 19 (solve2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")),
      TestCase (assertEqual "Part2 Example" 23 (solve2 "bvwbjplbgvbhsrlpgdmjqwftvncz")),
      TestCase (assertEqual "Part2 Example" 23 (solve2 "nppdvjthqldpwncqszvftbrmjlhg")),
      TestCase (assertEqual "Part2 Example" 29 (solve2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")),
      TestCase (assertEqual "Part2 Example" 26 (solve2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))
    ]
