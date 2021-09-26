
-- Exercises: Type Matching

-- 1a) not :: Bool -> Bool
-- 1b) length :: Foldable t => t a -> Int
-- 1c) concat :: Foldable t => t [a] -> a
-- 1d) head :: [a] -> a
-- 1e) (<) :: Ord a => a -> a -> Bool

-- 2a) head
-- 2b) concat
-- 2c) not
-- 2d) length
-- 2e) (<)

-- Exercises: Type Arguments
-- 1) Char -> Char -> Char
-- 2) Char
-- 3) Integer
-- 4) Double
-- 5) [Char] or String
-- 6) Eq b => b -> [Char]
-- 7) (Ord a, Num a) => a
-- 8) (Ord a, Num a) => a
-- 9) Integer

-- Exercises: Parametricity

q2 :: a -> a -> a
q2 x y = x
-- q2 x y = y

q3 :: a -> b -> b
q3 x y = y

-- Exercises: Apply Yourself
-- 1) myConcat :: [Char] -> [Char]
-- 2) myMult :: Fractional a => a -> a
-- 3) myTake :: Int -> [Char]
-- 4) myCom :: Int -> Bool
-- 5) myAlph :: Char -> Bool

-- Chapter Exercises
-- 1) c
-- 2) a
-- 3) b
-- 4) c

-- Determine the type
-- 1a) 54 :: Num a => a
-- 1b) (0, "doge") :: Num a => (a, [Char])
-- 1c) (0, "doge") :: (Integer, [Char])
-- 1d) False :: Bool
-- 1e) 5 :: Int
-- 1f) False :: Bool

-- 2) Num a => a
-- 3) Num a => a -> a
-- 4) Fractional a => a
-- 5) [Char]

-- 1)
bigNum = (^) 5 
wahoo = bigNum $ 10

-- 2)
x = print
y = print "woohoo!"
z = x "hello world"

-- 3)
a = (+)
b = 5
c = a 10
d = c 200


-- 4) 
aa c = 12 + bb c
bb c = 10000 * c

-- Type variable or specific type constructor?
-- 1) f :: Num a => a -> b -> Int -> Int 
-- constrained polymorphic, fully polymorphic, concrete, concrete
-- 2) f :: zed -> Zed -> Blah
-- fully polymorphic, concrete, concrete
-- 3) f :: Enum b => a -> b -> C
-- fully polymorphic, constrained polymorphic, concrete
-- 4) f :: f -> g -> C
-- fully polymorphic, fully polymorphic, concrete

-- Write a type signature

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool 
functionC x y =
  if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function
i :: a -> a
i x = x

cc :: a -> b -> a
cc x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r = tail

co :: (b -> c) -> (a -> b) -> a -> c
co f g x = f $ g x

aaa :: (a -> c) -> a -> a
aaa f x = x

aaa' :: (a -> b) -> a -> b
aaa' f x = f x

-- Type-Kwon-Do
f :: Int -> String
f = undefined
g :: String -> Char
g = undefined
h :: Int -> Char
h x = g $ f x

data A
data B
data C
q :: A -> B
q = undefined
w :: B -> C
w = undefined
e :: A -> C
e x = w $ q x

data X
data Y
data Z
xz :: X -> Z
xz = undefined
yz :: Y -> Z
yz = undefined
xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

munge :: (x -> y)
  -> (y -> (w, z))
  -> x
  -> w
munge xy f x = fst $ f $ xy x





