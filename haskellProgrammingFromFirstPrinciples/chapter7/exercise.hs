
-- Exercises: Grab Bag

-- 1) a, b, c
-- 2) d
-- 3) 
addOneIfOdd n = case odd n of
  True -> (\n -> n + 1) n
  False -> n

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x

-- Exercises: Variety Pack
-- 1a) k :: (a, b) -> a
-- 1b) k2 :: [Char], Not the same
-- 1c) k1, k3

-- 2)
f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

-- Exercises: Case Practice
functionC x y =
  case (x > y) of
    True -> x
    False -> y

ifEvenAdd2 n =
  case (even n) of
    True -> n + 2
    False -> n

nums x =
  case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0

-- Exercises: Artful Dodgy
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1

oneIsTwo :: Integer -> Integer
oneIsTwo = (flip dodgy) 2

-- 1) 1
-- 2) 11
-- 3) 22
-- 4) 21
-- 5) 12
-- 6) 11
-- 7) 21
-- 8) 21
-- 9) 22
-- 10) 31
-- 11) 23

-- Exercises: Guard Duty
-- 1) always return 'F'
-- 2) No
-- 3) b
-- 4) xs :: Eq a => [a]
-- 5) pal :: Eq a => [a] -> Bool
-- 6) c
-- 7) x :: (Ord a, Num a) => a
-- 8) numbers :: (Ord a, Num a, Num p) => a -> p

-- Chapter Exercises
-- 1) d
-- 2) b
-- 3) d
-- 4) b
-- 5) a

-- Let’s write code
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = fst (x `divMod` 10)
        d = snd (xLast `divMod` 10)

-- 1b) yes

-- 1c)
hunsD x = d
  where
    xLast = fst (x `divMod` 100)
    d = snd (xLast `divMod` 10)

-- 2)

foldBool :: a -> a -> Bool -> a
foldBool x y p = 
  case p of 
    True -> x
    False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y p
  | p = x
  | otherwise = y


-- 3)
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)





