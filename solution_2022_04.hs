{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- Day 4 / 2022

import Data.Text (Text, pack, splitOn, unpack)
import Test.HUnit

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  print (solve1 parsed)
  print (solve2 parsed)

type Section = (Int, Int)

type SectionPair = (Section, Section)

parse :: String -> [SectionPair]
parse input = map (parseSections . pack) (filter (not . null) (map unpack (splitOn "\n" (pack input))))

parseSections :: Text -> SectionPair
parseSections x = do
  let [left, right] = splitOn "," x
  let [l1, l2] = map (read . unpack) (splitOn "-" left)
  let [r1, r2] = map (read . unpack) (splitOn "-" right)
  ((l1, l2), (r1, r2))

solve1 :: [SectionPair] -> Int
solve1 pairs = length (filter (\(a, b) -> subsetOf a b || subsetOf b a) pairs)

solve2 :: [SectionPair] -> Int
solve2 pairs = length (filter (uncurry overlap) pairs)

subsetOf :: Section -> Section -> Bool
subsetOf (innerLeft, innerRight) (outerLeft, outerRight) = outerLeft <= innerLeft && innerRight <= outerRight

overlap :: Section -> Section -> Bool
overlap (xLeft, xRight) (yLeft, yRight) = not(xRight < yLeft || yRight < xLeft)

-- Tests

exampleInput :: String
exampleInput = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

exampleList :: [((Int, Int), (Int, Int))]
exampleList =
  [ ((2, 4), (6, 8)),
    ((2, 3), (4, 5)),
    ((5, 7), (7, 9)),
    ((2, 8), (3, 7)),
    ((6, 6), (4, 6)),
    ((2, 6), (4, 8))
  ]

tests :: Test
tests =
  TestList
    [ TestCase (assertEqual "Parse Example" exampleList (parse exampleInput)),
      TestCase (assertEqual "Part1 Example" 2 (solve1 exampleList)),
      TestCase (assertEqual "Part2 Example" 4 (solve2 exampleList))
    ]
