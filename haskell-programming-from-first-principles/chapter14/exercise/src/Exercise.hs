module Exercise where

import Data.List (sort)
import Data.Char (toUpper)

half x = x / 2
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative x y = x + y == y + x

multiplyAssociative x y z = x * (y * z) == (x * y) * z

multiplyCommutative x y = x * y == y * x

quotRem x y = quot x y * y + rem x y == x

divMod x y = div x y * y + mod x y == x

powerAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

powerCommutative x y = x ^ y == y ^ x

reverseTwice xs = (reverse . reverse $ xs) == id xs

dollarFn f x = f x == (f $ x)

dotFn f g x = (f . g $ x) == (f (g x))

constructorCheck x y = foldr (:) x y == (flip (++)) x y

appendCheck x = foldr (++) [] x == concat x

isThatSo n xs = length (take n xs) == n

readShow x = (read (show x)) == x

square x = x * x

squareIdentity x = (square . sqrt $ x) == id x

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

twice f = f . f
fourTimes = twice . twice

capitalizeWordCheck x = (capitalizeWord x == twice capitalizeWord x)
  &&
  (capitalizeWord x == fourTimes capitalizeWord x)

sortCheck x = (sort x == twice sort x) 
  && 
  (sort x == fourTimes sort x)

data Fool = Fulse | Frue deriving (Eq, Show)


