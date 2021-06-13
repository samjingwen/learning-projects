
-- Chapter Exercises
-- 1) a :: * 
-- 2) a :: * | f :: * -> *

-- String processing
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe = unwords . go . words
  where go [] = []
        go (x:xs) =
          case notThe x of
            Just x -> x : go xs
            Nothing -> "a" : go xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where go [] = 0
        go (x1:x2:xs) =
          case (x1, x2) of
            ("the", 'a':_) -> 1 + go (x2:xs)
            ("the", 'e':_) -> 1 + go (x2:xs)
            ("the", 'i':_) -> 1 + go (x2:xs)
            ("the", 'o':_) -> 1 + go (x2:xs)
            ("the", 'u':_) -> 1 + go (x2:xs)
            (_, _) -> 0 + go (x2:xs)
        go (x1:_) = 0

countVowels :: String -> Integer
countVowels = go
  where go [] = 0
        go (x:xs) =
          if isVowel x then 1 + go xs else 0 + go xs

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

-- Validate the word

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = go str
  where
    countOfVowels = countVowels str
    countOfConsonants = fromIntegral (length str) - countOfVowels
    vowelsMoreThanConsonants = countOfVowels > countOfConsonants
    go str =
        if vowelsMoreThanConsonants then Nothing else Just $ Word' str


-- It’s only Natural

data Nat = Zero | Succ Nat deriving (Eq, Show)
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x =
  if x < 0
    then Nothing
    else Just (go x)
  where go 0 = Zero
        go n = Succ (go (n - 1))

-- Small library for Maybe

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing = True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee y f (Just x) = f x
mayybee y f Nothing = y

fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybee x id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes [Just x] = [x]
catMaybes ((Just x):xs) = x: catMaybes xs
catMaybes [Nothing] = []
catMaybes (Nothing:xs) = catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (x:xs) =
  case x of
    Just a -> fmap (a: ) (flipMaybe xs)
    Nothing -> Nothing

-- Small library for Either

lefts' :: [Either a b] -> [a]
lefts' = foldr go []
  where go (Left a) acc = a: acc
        go _ acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr go []
  where go (Right b) acc = b: acc
        go _ acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([], [])
  where go (Left a) acc = (a: fst acc, snd acc)
        go (Right b) acc = (fst acc, b: snd acc)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' ac bc (Left a) = ac a
either' ac bc (Right b) = bc b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' ac bc
  where ac a = Nothing
        bc b = Just $ f b

-- Write your own iterate and unfoldr

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = go (f x)
  where go (Just (a, b)) = a : myUnfoldr f b
        go Nothing = []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr go
  where go a = Just (a, f a)

-- Finally something other than a list!

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = go (f x)  
  where go (Just (left, x, right)) = Node (go (f left)) x (go (f right))
        go Nothing = Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold go 0
  where go x = if x == n then Nothing else Just (x + 1, x, x + 1)







