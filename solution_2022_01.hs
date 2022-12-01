{-# LANGUAGE OverloadedStrings #-}

-- Day 1 / 2022

import Data.List (sort)
import Data.Maybe
import Data.Text (Text, pack, splitOn, unpack)
import GHC.Real (reduce)
import System.IO
import Text.Read

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
solve2 elves = do
  let (a : b : c : xs) = reverse (sort (map sum elves))
  a + b + c
