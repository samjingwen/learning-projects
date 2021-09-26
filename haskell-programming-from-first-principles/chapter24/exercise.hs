{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Word
import Data.Bits (shiftL)
import qualified Data.Text as T
import Text.RawString.QQ
import Text.Trifecta

-- Chapter Exercises
data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Eq, Ord, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer
  = SemVer Major Minor Patch Release Metadata
  deriving (Eq, Ord, Show)

parseNumberOrString :: Parser NumberOrString
parseNumberOrString =
  try (NOSS <$> some letter)
    <|> try (NOSI <$> integer)

parseRelease :: Parser [NumberOrString]
parseRelease =
  try (char '-' *> parseNumberOrString `sepBy` char '.')
    <|> return []

parseMetadata :: Parser [NumberOrString]
parseMetadata =
  try (char '+' *> parseNumberOrString `sepBy` char '.')
    <|> return []

skipDot :: Parser ()
skipDot = skipMany (oneOf ".")

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  char '.'
  minor <- decimal
  char '.'
  patch <- decimal
  release <- parseRelease
  metadata <- parseMetadata
  return $ SemVer major minor patch release metadata

parseDigit :: Parser Char
parseDigit = try (oneOf numbers) <|> fail "Must be integer"

numbers = "0123456789"

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' =
  (negate . read <$> (char '-' >> some parseDigit))
    <|> (read <$> some parseDigit)

-- 4
type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber
  = PhoneNumber
      NumberingPlanArea
      Exchange
      LineNumber
  deriving (Eq, Show)

digits n = replicateM n digit

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea =
  read
    <$> ( try (digits 3)
            <|> try (char '(' *> digits 3 <* char ')')
            <|> try (string "1-" >> digits 3)
        )

parseExchange :: Parser Exchange
parseExchange =
  read
    <$> ( try (char '-' *> digits 3)
            <|> try (digits 3)
            <|> try (char ' ' *> digits 3)
        )

parseLineNumber :: Parser LineNumber
parseLineNumber =
  read
    <$> ( try (char '-' *> digits 4)
            <|> try (digits 4)
        )

parsePhone :: Parser PhoneNumber
parsePhone = do
  numberingPlanArea <- parseNumberingPlanArea
  exchange <- parseExchange
  lineNumber <- parseLineNumber
  return $ PhoneNumber numberingPlanArea exchange lineNumber

data Time = Time {hour :: Integer, minutes :: Integer} deriving (Eq, Show)
data Date = Date {year :: Integer, month:: Integer, day :: Integer } deriving (Eq, Show)
data LogEntry = LogEntry Time String deriving (Eq, Show)

data Log = Log Date [LogEntry] deriving (Eq, Show)

parseComments :: Parser ()
parseComments = token $ do
  string "--"
  manyTill anyChar (void newline <|> eof)
  return ()

myToken :: Parser a -> Parser a
myToken p = token p <* token (skipSome parseComments <|> return ())

parseTimeOfDay :: Parser Time
parseTimeOfDay = myToken $ do
  hour <- read <$> digits 2
  _ <- char ':'
  minutes <- read <$> digits 2
  return $ Time hour minutes

parseDate :: Parser Date
parseDate = myToken $ do
  _ <- token (char '#')
  year <- read <$> digits 4
  _ <- char '-'
  month <- read <$> digits 2
  _ <- char '-'
  day <- read <$> digits 2
  return $ Date year month day

parseActivity :: Parser String
parseActivity = myToken $
  manyTill
    anyChar
    ( try (void newline)
        <|> try (void parseComments)
        <|> try eof
    )

parseLog :: Parser LogEntry
parseLog = myToken $ do
  timeOfDay <- parseTimeOfDay
  entry <- parseActivity
  return $ LogEntry timeOfDay entry

parseLogEntries :: Parser Log
parseLogEntries = myToken $ do
  _ <- skipMany parseComments
  date <- parseDate
  logEntries <- many parseLog
  return $ Log date logEntries

logs =
  [r|-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

-- 6

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

parseOctet :: Parser Integer
parseOctet = read <$> (case1 <|> case2 <|>  case3 <|> case4 <|> case5)
    where case1 = try $ sequenceA [char '2', char '5', oneOf "012345"]
          case2 = try $ sequenceA [char '2', oneOf "01234", digit]
          case3 = try $ sequenceA [char '1', digit, digit]
          case4 = try $ sequenceA [oneOf "123456789", digit]
          case5 = pure <$> digit

convertToDecimal :: [Integer] -> Integer
convertToDecimal = fst . foldr f (0, 0)
  where f octet (total, bits) = (shiftL octet bits + total, bits + 8)

parseIPv4 :: Parser IPAddress
parseIPv4 = do
  octets <- parseOctet `sepBy` char '.'
  return $ IPAddress . fromInteger . convertToDecimal $ octets

-- 7 
data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  print $ parseString parseSemVer mempty "2.1.1"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92+123.abc"
  print $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []

  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "abc"

  print $ parseString base10Integer' mempty "-123abc"

  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"

  print $ parseString parseComments mempty "-- wheee a comment"
  print $ parseString parseDate mempty "# 2025-02-05"
  print $ parseString parseLog mempty "09:00 Bumped head, passed out"
  print $ parseString parseLogEntries mempty logs

  print $ parseString parseIPv4 mempty "172.16.254.1"
  print $ parseString parseIPv4 mempty "204.120.0.15"

  print $ parseString parseIPv4 mempty "0:0:0:0:0:ffff:ac10:fe01"
  print $ parseString parseIPv4 mempty "0:0:0:0:0:ffff:cc78:f"