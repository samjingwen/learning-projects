{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseDecimal :: (Monad m, TokenParsing m) => m Rational
parseDecimal = do
  num <- decimal
  char '.'
  dec <- decimal
  let exp = (10 ^) . length . show $ dec
  return ((num * exp + dec) % exp)

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction ::
  (Monad m, TokenParsing m, MonadFail m) =>
  m Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimalOrFraction :: Parser Rational
parseDecimalOrFraction = try parseFraction <|> try parseDecimal

myInteger :: Parser Integer
myInteger = do
  num <- integer
  eof
  return num

main :: IO ()
main = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

testVirtuous :: IO ()
testVirtuous = do
  let virtuousFraction' =
        parseString virtuousFraction mempty
  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork

testInteger' :: IO ()
testInteger' = do
  print $ parseString integer mempty "123"
  print $ parseString (integer >> eof) mempty "123abc"
  print $ parseString (integer >> eof) mempty "123"

  print $ parseString myInteger mempty "123"
  print $ parseString myInteger mempty "123abc"

testDecimalOrFraction :: IO ()
testDecimalOrFraction = do
  print $ parseString parseDecimalOrFraction mempty "12.3"
  print $ parseString parseDecimalOrFraction mempty "12.0"
  print $ parseString parseDecimalOrFraction mempty "12/3"
  print $ parseString parseDecimalOrFraction mempty "12/0"

main' :: IO ()
main' = do
  let attoP = parseOnly virtuousFraction
  print $ attoP badFraction
  print $ attoP shouldWork
  print $ attoP shouldAlsoWork
  print $ attoP alsoBad

  -- parseString is Trifecta
  let p f i =
        parseString f mempty i
  print $ p virtuousFraction badFraction
  print $ p virtuousFraction shouldWork
  print $ p virtuousFraction shouldAlsoWork
  print $ p virtuousFraction alsoBad
