import Data.Time

-- Exercises: Understanding Folds
-- 1) b, c
-- 2) 
{-
foldl (flip (*)) 1 [1..3]
foldl (flip (*)) ((flip (*)) 1 1) [2, 3]
foldl (flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) [3]
foldl (flip (*)) ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3) []
((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3)
((flip (*)) ((flip (*)) 1 2) 3)
((flip (*)) 2 3)
6
-}
-- 3) c
-- 4) a

-- 5)
a = foldr (++) "" ["woot", "WOOT", "woot"]
b = foldr max ' ' "fear is the little death"
c = foldr (&&) True [False, True]
d = foldr (||) False [False, True]
e = foldr ((++) . show) "" [1..5]
f = foldl const 'a' [1..5]
g = foldl const 0 "tacos"
h = foldr (flip const) 0 "burritos"
i = foldr (flip const) 'z' [1..5]

-- Exercises: Database Processing
data DatabaseItem = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
  [
    DbDate 
      (UTCTime 
      (fromGregorian 1911 5 1) 
      (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!",
    DbNumber 123,
    DbDate 
      (UTCTime 
      (fromGregorian 1921 5 1) 
      (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate (DbDate x:xs) = x : filterDbDate xs
filterDbDate (_:xs) = filterDbDate xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber (DbNumber x:xs) = x : filterDbNumber xs
filterDbNumber (_:xs) = filterDbNumber xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = 
  foldr 
    (\x y -> max (go x) y)
    minTime 
    xs
    where go (DbDate x) = x
          go _ = minTime
          minTime = (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0))

sumDb :: [DatabaseItem] -> Integer
sumDb xs = 
  foldr 
    (\x y -> (go x) + y)
    0
    xs
    where go (DbNumber x) = x
          go _ = 0

avgDb :: [DatabaseItem] -> Double
avgDb xs = if count > 1 then total / count else 0
  where 
    tuple = 
      foldr 
        go
        (0, 0)
        xs
        where go (DbNumber x) y = ((fst y + x), snd y + 1)
              go _ y = y
    total = fromIntegral (fst tuple)
    count = fromIntegral (snd tuple)

-- Scans Exercises

fibs = 1 : scanl (+) 1 fibs
fibsFirst20 = take 20 fibs
fibsLessThan100 = filter (< 100) fibs
factorial = scanl (*) 1 [1..]
factorialFirst20 = take 20 factorial

-- Chapter Exercises
stops = "pbtdkg"
vowels = "aeiou"

stopsVowelsStops = [(x, y, z) | x <- stops, y <- vowels, z <- stops] 
stopsVowelsStopsEqualsP = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

nouns = ["cat", "dog", "shark", "batman"]
verbs = ["talk", "bark", "shout", "bite"]
nounsVerbsNouns = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]


seekritFunc x =
  div (sum (map length (words x)))
    (length (words x))
-- average length of each word
seekritFuncPrecise x =
  (/) (fromIntegral (sum (map length (words x))))
    (fromIntegral (length (words x)))


-- Rewriting functions using folds

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x y -> y || f x) False xs

myElem :: Eq a => a -> [a] -> Bool
myElem a as = foldr (\x y -> y || x == a) False as

myReverse :: [a] -> [a]
myReverse xs = foldl (flip (:)) [] xs

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x y -> f x : y) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr (\x y -> if (f x) then x: y else y) [] xs

squish :: [[a]] -> [a]
squish xs = foldr go [] xs
              where go x y = foldr (:) y x

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = foldr go [] xs
                  where go x y = foldr (:) y (f x)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\a b -> if (f a b == GT) then a else b) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\a b -> if (f a b == LT) then a else b) x xs

