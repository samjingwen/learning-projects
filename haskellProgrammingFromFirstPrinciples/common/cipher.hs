module Cipher where

import Data.Char

import Test.Hspec
import Test.QuickCheck

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

unVigenere :: [Char] -> [Char] -> [Char]
unVigenere key message = go key message
  where
    go _ [] = []
    go [] msg = go key msg
    go xxs@(x:xs) (y:ys) =
      if isAlpha y then
        shiftChar y (negate (getCharIndex x)) : go xs ys
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

genString :: Gen String
genString = listOf $ elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

prop_caesarIdentity :: Property
prop_caesarIdentity = forAll genString (\x -> x == (unCaesar 5 . caesar 5 $ x))

prop_vigenereIdentity :: Property
prop_vigenereIdentity = forAll genString (\x -> x == (unVigenere "hello" . vigenere "hello" $ x))


main :: IO ()
main = do
  hspec $ do
    describe "caesar and unCaesar" $ do
      it "idempotent" $ do
        property prop_caesarIdentity
  
  hspec $ do
    describe "vigenere and unVigenere" $ do
      it "idempotent" $ do
        property prop_vigenereIdentity
  