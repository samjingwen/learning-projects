import Data.Bool (bool)
import Data.Char

-- Exercise: EnumFromTo
eft :: (Enum a, Ord a) => a -> a -> [a]
eft start stop
  | start > stop = []
  | start == stop = [start]
  | otherwise = start : eft (succ start) stop

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft


-- Exercises: Comprehend Thy Lists
-- [1,4,9,16,25,36,49,64,81,100]
-- [4,16,36,64,100]
{-
[
  (1,64),(1,81),(1,100),
  (4,64),(4,81),(4,100),
  (9,64),(9,81),(9,100),
  (16,64),(16,81),(16,100),
  (25,64),(25,81),(25,100),
  (36,64),(36,81),(36,100),
  (49,64),(49,81),(,100)
]
-}
-- [(1,64),(1,81),(1,100),(4,64),(4,81)]


-- Exercises: Square Cube
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

qns1 = [(x, y) | x <- mySqr, y <- myCube]

qns2 = [(x, y) | x <- mySqr, y <- myCube, x < 50 && y < 50]

qns3 = length qns2

-- Exercises: Bottom Madness
-- 1) undefined
-- 2) [1]
-- 3) undefined
-- 4) 3
-- 5) undefined
-- 6) [2]
-- 7) undefined
-- 8) [1]
-- 9) [1,3]
-- 10) undefined


-- Intermission: Is it in normal form?
-- 1) yes
-- 2) yes
-- 3) no
-- 4) no
-- 5) no
-- 6) no
-- 7) yes

-- Exercises: More Bottoms
-- 1) undefined
-- 2) [2]
-- 3) undefined
-- 4) check if letters are vowels
-- 5a) [1,4,9,16,25,36,49,64,81,100]
-- 5b) [1,10,20]
-- 5c) [15,15,15]

qns6 = map (\x -> bool x (x^2) (even x)) [1..10]


-- Exercises: Filtering
multiplesOf3 = filter (\x -> rem x 3 == 0) [1..30]

lengthOfMultiplesOf3 = length . filter (\x -> rem x 3 == 0) $ [1..30]

myFilter = filter (\x -> not (x == "the" || x == "a" || x == "an")) . words


-- Zipping exercises
oldZip :: [a] -> [b] -> [(a, b)]
oldZip _ [] = []
oldZip [] _ = []
oldZip (x:xs) (y:ys) = (x, y) : oldZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (,)


-- Chapter Exercises

-- 1)
-- isUpper :: Char -> Bool
-- toUpper :: Char -> Char

-- 2) 
filterUpper :: [Char] -> [Char]
filterUpper = filter isUpper

-- 3)
capitalizeFirstLetter :: [Char] -> [Char]
capitalizeFirstLetter [] = []
capitalizeFirstLetter (x:xs) = toUpper x : xs

-- 4)
capitalizeAll :: [Char] -> [Char]
capitalizeAll = map toUpper

-- 5)
getFirstLetter :: [Char] -> Char
getFirstLetter = toUpper . head


-- Writing your own standard functions
-- 1)
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- 2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

-- 3)
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = (a == x) || myElem a xs

-- 4)
myReverse :: [a] -> [a]
myReverse lst = go lst []
  where
    go [] acc = acc
    go (x:xs) acc = go xs (x : acc)

-- 5)
squish :: [[a]] -> [a]
squish [] = []
squish ([]:xs) = squish xs
squish ((x:xs):xss) = x : squish (xs:xss)

-- 6)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

-- 7)
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8)
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = if f x last == GT then x else last
  where last = myMaximumBy f xs

-- 9)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [x] = x
myMinimumBy f (x:xs) = if f x last == LT then x else last
  where last = myMinimumBy f xs

-- 10)
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

main = do
  print $ myOr []
  print $ myOr [1 == 1, 'a' == 'A']
  print $ myOr [1 == 2, 'a' == 'A']
  
  print $ myAny (>20) [1..10]
  print $ myAny (<5) [1..10]
  print $ myAny (\x -> rem 2 x == 0) [1..10]
  print $ myAny even [1, 3, 5]
  print $ myAny odd [1, 3, 5]

  print $ myElem 1 [1..10]
  print $ myElem 1 [2..10]

  print $ myReverse "blah"
  print $ myReverse [1..5]

  print $ squishMap (\x -> [1, x, 3]) [2,3]
  print $ squishMap (\x -> "WO "++[x]++" HOO ") "123"
  
  print $ myMaximumBy compare [1, 53, 9001, 10]
  print $ myMinimumBy compare [1, 53, 9001, 10]

  print $ myMaximum [1, 53, 9001, 10]
  print $ myMinimum [1, 53, 9001, 10]

