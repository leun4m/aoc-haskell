{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- Day 2 / 2022

import Data.Maybe (mapMaybe)
import Data.Text (Text, pack, splitOn, unpack)
import Test.HUnit

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  print (solve1 parsed)
  print (solve2 parsed)

data Thing = Shape Shape | Symbol Symbol

data Shape = Rock | Paper | Scissors
  deriving (Show, Eq)

data Symbol = X | Y | Z
  deriving (Show, Eq)

data Outcome = Win | Draw | Lose
  deriving (Show, Eq)

parse :: String -> [(Shape, Symbol)]
parse input = mapMaybe parseMatch (filter (not . null . unpack) (splitOn "\n" (pack input)))

parseMatch :: Text -> Maybe (Shape, Symbol)
parseMatch group = do
  let result = mapMaybe mapLetter (splitOn " " group)
  let (opponent : you : xs) = result
  createMatch (opponent, you)

createMatch :: (Thing, Thing) -> Maybe (Shape, Symbol)
createMatch (Shape s, Symbol t) = Just (s, t)
createMatch _ = Nothing

mapLetter :: Text -> Maybe Thing
mapLetter "A" = Just $ Shape Rock
mapLetter "B" = Just $ Shape Paper
mapLetter "C" = Just $ Shape Scissors
mapLetter "X" = Just $ Symbol X
mapLetter "Y" = Just $ Symbol Y
mapLetter "Z" = Just $ Symbol Z
mapLetter _ = Nothing

solve1 :: [(Shape, Symbol)] -> Int
solve1 x = sum (map (calcScore . symbolAsShape) x)

solve2 :: [(Shape, Symbol)] -> Int
solve2 x = sum (map (calcScore . chooseShape . symbolAsOutCome) x)

calcScore :: (Shape, Shape) -> Int
calcScore (oppenent, you) = scoreShape you + scoreOutcome (calcOutcome (oppenent, you))

symbolAsShape :: (Shape, Symbol) -> (Shape, Shape)
symbolAsShape (a, b) = (a, toShape b)

symbolAsOutCome :: (Shape, Symbol) -> (Shape, Outcome)
symbolAsOutCome (a, b) = (a, toOutcome b)

chooseShape :: (Shape, Outcome) -> (Shape, Shape)
chooseShape (opponent, outcome) = (opponent, head (filter (\s -> calcOutcome (opponent, s) == outcome) [Rock, Paper, Scissors]))

toOutcome :: Symbol -> Outcome
toOutcome X = Lose
toOutcome Y = Draw
toOutcome Z = Win

toShape :: Symbol -> Shape
toShape X = Rock
toShape Y = Paper
toShape Z = Scissors

scoreShape :: Shape -> Int
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

scoreOutcome :: Outcome -> Int
scoreOutcome Lose = 0
scoreOutcome Draw = 3
scoreOutcome Win = 6

calcOutcome :: (Shape, Shape) -> Outcome
calcOutcome (Rock, Paper) = Win
calcOutcome (Paper, Scissors) = Win
calcOutcome (Scissors, Rock) = Win
calcOutcome (a, b) = if a == b then Draw else Lose

-- Tests

exampleInput :: String
exampleInput = "A Y\nB X\nC Z"

exampleList :: [(Shape, Symbol)]
exampleList = [(Rock, Y), (Paper, X), (Scissors, Z)]

tests :: Test
tests =
  TestList
    [ TestCase (assertEqual "Parse Example" exampleList (parse exampleInput)),
      TestCase (assertEqual "Part1 Example" 15 (solve1 exampleList)),
      TestCase (assertEqual "Part2 Example" 12 (solve2 exampleList))
    ]
