module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n = numberWord !! n
  where
    numberWord =
      [ "zero",
        "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine"
      ]

digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

wordNumber :: Int -> String
wordNumber n = intercalate "-" (map digitToWord (digits n))