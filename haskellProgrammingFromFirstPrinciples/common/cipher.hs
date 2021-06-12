module Cipher where

import Data.Char

caesar :: Int -> [Char] -> [Char]
caesar _ [] = []
caesar shift (x:xs) = go x : caesar shift xs
  where 
    go :: Char -> Char
    go ch 
      | isUpper ch = chr (65 + mod (ord ch - 65 + shift) 26)
      | not $ isUpper ch = chr (97 + mod (ord ch - 97 + shift) 26)


unCaesar :: Int -> [Char] -> [Char]
unCaesar _ [] = []
unCaesar shift str = caesar (-shift) str