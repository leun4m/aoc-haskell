{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- Day 6 / 2022

import Data.Text as Text
import Test.HUnit

main :: IO ()
main = do
  input <- getContents
  let x = parse input
  print ()

-- print (solve1 x)
-- print (solve2 input)

data FileTree = Directory String [FileTree] | File String Int
  deriving (Show, Eq)

data Cmd = ChangeDirectoryUp | ChangeDirectory String | ListDirectory | Tree FileTree
  deriving (Show, Eq)

parse :: String -> [Cmd]
parse x = Prelude.map mapThis (Prelude.lines x)

mapThis :: String -> Cmd
mapThis x = do
  let first = Prelude.take 1 x
  if first == "$"
    then do
      let cmd = Prelude.take 4 x
      if cmd == "$ cd"
        then do
          let arg = Prelude.drop 5 x
          if arg == ".."
            then ChangeDirectoryUp
            else ChangeDirectory arg
        else
          if cmd == "$ ls"
            then ListDirectory
            else error "unknown command"
    else do
      let isDir = Prelude.take 3 x
      if isDir == "dir"
        then Tree (Directory (Prelude.take 3 x) [])
        else do
          let (size : filename : _) = Text.splitOn " " (pack x)
          Tree (File (unpack filename) (read (unpack size)))

-- solve1 :: String -> Int
-- solve1 = firstUniqueChars 4

-- solve2 :: String -> Int
-- solve2 = firstUniqueChars 14

-- Tests

exampleInput :: String
exampleInput = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"

exampleOutput :: [Cmd]
exampleOutput =
  [ ChangeDirectory "/",
    ListDirectory,
    Tree (Directory "dir" []),
    Tree (File "b.txt" 14848514),
    Tree (File "c.dat" 8504156),
    Tree (Directory "dir" []),
    ChangeDirectory "a",
    ListDirectory,
    Tree (Directory "dir" []),
    Tree (File "f" 29116),
    Tree (File "g" 2557),
    Tree (File "h.lst" 62596),
    ChangeDirectory "e",
    ListDirectory,
    Tree (File "i" 584),
    ChangeDirectoryUp,
    ChangeDirectoryUp,
    ChangeDirectory "d",
    ListDirectory,
    Tree (File "j" 4060174),
    Tree (File "d.log" 8033020),
    Tree (File "d.ext" 5626152),
    Tree (File "k" 7214296)
  ]

tests :: Test
tests =
  TestList []

-- [TestCase (assertEqual "Part1 Example" 7 (parse exampleInput))]
