{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import Data.List
import Data.Maybe


-- Exercises: Dog Types
-- 1) type constructor
-- 2) * -> *
-- 3) *
-- 4) Num a => Doggies a
-- 5) Doggies Integer
-- 6) Doggies [Char]
-- 7) both
-- 8) a -> DogueDeBordeaux a
-- 9) DogueDeBordeaux [Char]


-- Exercises: Vehicles

data Price = Price Integer deriving (Eq, Show)
data Size = Size Double deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 1000)

-- 1) myCar :: Vehicle

-- 2)
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars [] = []
areCars (x:xs) = isCar x : areCars xs

-- 3)
getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

-- 4) Exception: Non-exhaustive patterns in function getManu

-- Exercises: Cardinality

-- 1) 1
-- 2) 3
-- 3) 65536
-- 4) No instance for (Bounded Integer), Int is bounded 
-- 5) 2^8 = 256

-- Exercises: For Example
data Example = MakeExample Int deriving Show

-- 1) MakeExample :: Example, Data constructor not in scope: Example
-- 2) instance Show Example
-- 4) MakeExample :: Int -> Example


-- Exercises: Logic Goats
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where 
  tooMany(n, _) = tooMany n

instance TooMany (Int, Int) where
  tooMany(n, m) = tooMany(n + m)

instance (Num a, TooMany a) => TooMany(a, a) where
  tooMany (n, m) = (tooMany n) && (tooMany m)


-- Exercises: Pity the Bool
-- 1) 4
-- 2) 258, overflow 

-- Exercises: How Does Your Garden Grow?
data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType deriving Show

data Garden'
  = Gardenia' Gardener
  | Daisy' Gardener
  | Rose' Gardener
  | Lilac' Gardener
  deriving Show

-- Exercise: Programmers

data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer {os :: OperatingSystem, lang :: ProgLang}
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = 
  [Programmer {os = os, lang = lang} | os <- allOperatingSystems, lang <- allLanguages]


-- Exercises: The Quad

-- 1) 8
-- 2) 16
-- 3) 256
-- 4) 8
-- 5) 16
-- 6) 65536

-- Write map for BinaryTree
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

t1 = insert' 0 Leaf
t2 = insert' 3 t1 
t3 = insert' 5 t2

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
mapOkay = if mapTree (+1) testTree' == mapExpected
  then print "yup okay!" else error "test failed!"

-- Convert binary trees to lists

preorder :: BinaryTree a -> [a]
preorder tree = go [] tree
  where go lst Leaf = lst
        go lst (Node Leaf n Leaf) = n : lst
        go lst (Node left n right) = n : go (go lst right) left

inorder :: BinaryTree a -> [a]
inorder tree = go [] tree
  where go lst Leaf = lst
        go lst (Node Leaf n Leaf) = n : lst
        go lst (Node left n right) = go (n : go lst right) left

postorder :: BinaryTree a -> [a]
postorder tree = go [] tree
  where go lst Leaf = lst
        go lst (Node Leaf n Leaf) = n : lst
        go lst (Node left n right) = go (go (n : lst) right) left

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
  2
  (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left n right) = foldTree f (foldTree f (f n acc) left) right


-- Chapter Exercises
-- 1) a
-- 2) c
-- 3) b
-- 4) c

-- As-patterns
-- 1)
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xxs@(x:xs) yys@(y:ys)
  | x == y = isSubseqOf xs ys
  | x /= y = isSubseqOf xxs ys

-- 2)
capitalizeWords :: String -> [(String, String)]
capitalizeWords str = map f (words str)
  where f word = (word, capitalizeWord' word)

capitalizeWord' :: String -> String
capitalizeWord' "" = ""
capitalizeWord' (x:xs) = if isAlpha x then toUpper x : xs else x : capitalizeWord xs

-- 3)
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph = concatMap capitalizeWord . groupBy (const (/= '.'))


-- Phone exercise
data DaPhone = DaPhone [Button]

data Button = Button Digit String

phone = DaPhone [one, two, three, four, five, six, seven, eight, nine, zero, star, hash]
  where
    one = Button '1' "1"
    two = Button '2' "abc2"
    three = Button '3' "def3"
    four = Button '4' "ghi4"
    five = Button '5' "jkl5"
    six = Button '6' "mno6"
    seven = Button '7' "pqrs7"
    eight = Button '8' "tuv8"
    nine = Button '9' "wxyz9"
    zero = Button '0' " 0"
    star = Button '*' "^"
    hash = Button '#' ".,"

convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Just making sure rofl ur turn"]

type Digit = Char

type Presses = Int

reverseTaps :: DaPhone
  -> Char
  -> [(Digit, Presses)]
reverseTaps (DaPhone buttons) chr =
  if isAlpha chr && isUpper chr
    then
      [('*', 1), getTaps chr . getButton $ buttons]
    else
      [getTaps chr . getButton $ buttons]
  where
    getButton buttons = fromJust . find (f . toLower $ chr) $ buttons
    getTaps chr (Button btn chrs) = (btn, (+1) . fromJust . (elemIndex . toLower $ chr) $ chrs)
    f chr (Button btn chrs) = chr `elem` chrs

cellPhonesDead :: DaPhone
  -> String
  -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\a b -> snd a + b ) 0

mostPopularLetter :: String -> Char
mostPopularLetter = head . last . sortOn length . group . sort

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . map mostPopularLetter

coolestWord :: [String] -> String
coolestWord = head . last . sortOn length . group . sort


-- Hutton’s Razor
data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit num) = num
eval (Add exp1 exp2) = eval exp1 + eval exp2

printExpr :: Expr -> String
printExpr (Lit num) = show num
printExpr (Add exp1 exp2) = printExpr exp1 ++ " + " ++ printExpr exp2


a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2



