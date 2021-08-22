module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

oneEof :: Parser ()
oneEof = char '1' >> eof

oneTwoEof :: Parser ()
oneTwoEof = char '1' >> char '2' >> eof

oneTwoThreeEof :: Parser ()
oneTwoThreeEof = char '1' >> char '2' >> char '3' >> eof

str1 :: Parser String
str1 = string "1"

str12 :: Parser String
str12 = string "12" >> stop

str123 :: Parser String
str123 = string "123"

str'1 :: Parser String
str'1 = string' "1"

str'12 :: Parser String
str'12 = string' "12" >> stop

str'123 :: Parser String
str'123 = string' "123"

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParse' :: Parser () -> IO ()
testParse' p = print $ parseString p mempty "123"

strParse :: Parser String -> IO ()
strParse p = print $ parseString p mempty "123"

string' :: String -> Parser String
string' = foldr (\ch str -> (:) <$> char ch <*> str) (pure "")

pNL s = putStrLn ('\n' : s)
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

  pNL "eof:"
  testParse' oneEof
  pNL "eof:"
  testParse' oneTwoEof
  pNL "eof:"

  pNL "str1:"
  strParse str1
  pNL "str12:"
  strParse str12
  pNL "str123:"
  strParse str123

  pNL "str'1:"
  strParse str'1
  pNL "str'12:"
  strParse str'12
  pNL "str'123:"
  strParse str'123
