module PoemLines where

-- Exercises: Thy Fearful Symmetry

myWords :: [Char] -> [[Char]]
myWords "" = []
myWords (' ':xs) = myWords xs
myWords words = takeWhile (/= ' ') words : myWords (dropWhile (/= ' ') words)

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
            \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines lines = go lines
  where 
    go :: String -> [String]
    go "" = []
    go ('\n':xs) = go xs
    go l = takeWhile (/= '\n') l : (go (dropWhile (/= '\n') l))

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main = do
      print $
        "Are they equal? "
        ++ show (myLines sentences
                  == shouldEqual)
      print $
        "Are they equal? "
        ++ show (newMyLines sentences
                  == shouldEqual)

splitBy :: Char -> [Char] -> [[Char]]
splitBy _ "" = []
splitBy separator str = subStr: splitBy separator remainingStr
  where subStr = takeWhile (/= separator) 
                  . dropWhile (== separator) $ str
        remainingStr = dropWhile (== separator) 
                        . dropWhile (/= separator) 
                        . dropWhile (== separator) $ str

newMyWords = splitBy ' '
newMyLines = splitBy '\n'