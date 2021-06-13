module Cipher where

import Data.Char

shiftChar :: Char -> Int -> Char
shiftChar ch shift =
  if isUpper ch then
    chr (65 + mod (getCharIndex ch + shift) 26)
  else chr (97 + mod (getCharIndex ch + shift) 26)

getCharIndex :: Char -> Int
getCharIndex ch =
  if isUpper ch then
    ord ch - 65
  else ord ch - 97

caesar :: Int -> [Char] -> [Char]
caesar _ [] = []
caesar shift (x:xs) = shiftChar x shift : caesar shift xs

unCaesar :: Int -> [Char] -> [Char]
unCaesar _ [] = []
unCaesar shift str = caesar (-shift) str

vigenere :: [Char] -> [Char] -> [Char]
vigenere key message = go key message
  where
    go _ [] = []
    go [] msg = go key msg
    go xxs@(x:xs) (y:ys) = 
      if isAlpha y then
        shiftChar y (getCharIndex x) : go xs ys
      else
        y : go xxs ys

caesar' :: IO ()
caesar' = do
  putStr "Enter shift: "
  shift <- getLine
  putStr "Enter text: "
  text <- getLine
  putStrLn $ caesar (read shift) text

vigenere' :: IO ()
vigenere' = do
  putStr "Enter key: "
  key <- getLine 
  putStr "Enter text: "
  text <- getLine 
  putStrLn $ vigenere key text
  
  
  

  