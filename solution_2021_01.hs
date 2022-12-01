{-# LANGUAGE OverloadedStrings #-}

-- Day 1 / 2021

import Debug.Trace
import System.IO

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  print (solve1 parsed)
  print (solve2 parsed)

parse :: String -> [Integer]
parse x = map read (lines x) :: [Integer]

solve1 :: [Integer] -> Integer
solve1 (x : y : rest) = (if x < y then 1 else 0) + solve1 (y : rest)
solve1 _ = 0

solve2 :: [Integer] -> Integer
solve2 (w : x : y : z : rest) = (if w < z then 1 else 0) + solve2 (x : y : z : rest)
solve2 _ = 0
