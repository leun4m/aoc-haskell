{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

-- Day 2 / 2021

import Data.List (stripPrefix)
import Debug.Trace
import System.IO

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  print (solve1 (Pos 0 0) parsed)
  print (solve2 (PosAim 0 0 0) parsed)

data Direction = Forward !Int | Down !Int | Up !Int
  deriving (Show, Eq)

data Position1 = Pos {horizontal1 :: !Int, depth1 :: !Int}
  deriving (Show, Eq)

data Position2 = PosAim {horizontal2 :: !Int, depth2 :: !Int, aim :: !Int}
  deriving (Show, Eq)

parse :: String -> [Direction]
parse x = map parseDirection (lines x)

parseDirection :: String -> Direction
parseDirection string | Just num <- stripPrefix "forward " string = Forward (read num)
parseDirection string | Just num <- stripPrefix "down " string = Down (read num)
parseDirection string | Just num <- stripPrefix "up " string = Up (read num)
parseDirection x = error ("Unexpected direction: " ++ x)

solve1 :: Position1 -> [Direction] -> Int
solve1 pos directions = do
  let endPos = foldl apply1 pos directions
  horizontal1 endPos * depth1 endPos

solve2 :: Position2 -> [Direction] -> Int
solve2 pos directions = do
  let endPos = foldl apply2 pos directions
  horizontal2 endPos * depth2 endPos

apply1 :: Position1 -> Direction -> Position1
apply1 pos direction
  | Forward x <- direction = Pos (horizontal1 pos + x) (depth1 pos)
  | Up x <- direction = Pos (horizontal1 pos) (depth1 pos - x)
  | Down x <- direction = Pos (horizontal1 pos) (depth1 pos + x)

apply2 :: Position2 -> Direction -> Position2
apply2 pos direction
  | Forward x <- direction = PosAim (horizontal2 pos + x) (depth2 pos + aim pos * x) (aim pos)
  | Up x <- direction = PosAim (horizontal2 pos) (depth2 pos) (aim pos - x)
  | Down x <- direction = PosAim (horizontal2 pos) (depth2 pos) (aim pos + x)
