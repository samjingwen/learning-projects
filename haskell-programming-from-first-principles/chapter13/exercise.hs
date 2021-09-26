
-- Intermission: Check your understanding

-- 1) forever, when
-- 2) Data.Bits, Database.Blacktip.Types
-- 3) Database types
-- 4a) Control.Concurrent.MVar, Filesystem.Path.CurrentOS, Control.Concurrent
-- 4b) Filesystem
-- 4c) Control.Monad

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, isAlphaNum)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  if isPalindrome line1 then putStrLn "It's a palindrome!" else (
    do
      putStrLn "Nope!"
      exitSuccess)

isPalindrome :: String -> Bool
isPalindrome word = go word reverseWord True
  where
    reverseWord = reverse word
    go :: [Char] -> [Char] -> Bool -> Bool
    go [] [] bool = bool
    go _ [] bool = bool
    go [] _ bool = bool
    go xxs@(x:xs) yys@(y:ys) bool =
      case (isAlphaNum x, isAlphaNum y) of
        (True, True) -> (toLower x == toLower y) && go xs ys bool
        (False, True) -> go xs yys bool
        (True, False) -> go xxs ys bool
        (False, False) -> go xs ys bool



type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid =
  NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)
mkPerson :: Name
  -> Age
  -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
    Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter age: "
  age <- getLine
  putStr "Enter name: "
  name <- getLine 
  let eitherPerson =  mkPerson name (read age)
  case eitherPerson of
    Right person -> 
      putStrLn $ "Yay! Successfully got a person: " ++ show person
    Left personInvalid -> 
      putStrLn $ "Error" ++ show personInvalid

