
-- Exercises: Mood Swing

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah


-- Exercises: Find the Mistakes
q1 = not True && True

q2 x = not (x == 6)

q3 = (1 * 2) > 5

q4 = ["Merry"] > ["Happy"]

q5 = ["1", "2", "3"] ++ ["look at me!"]


-- Chapter Exercises
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- 1) Foldable t => t a -> Int
-- 2) a)5 b)3 c)2 d)5
-- 3) (/) :: Fractional a => a -> a -> a // No Fractional instance

qns4 = 6 `div` length [1,2,3]

-- 5) Bool
-- 6) False
-- 7) True, won't work, 5, False, won't work

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == y
  where y = reverse x

myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else (-x)

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

x = (+)
f1 xs = w `x` 1
  where w = length xs

-- \x = x

f3 a b = a

-- Match the function names to their types
-- 1) c
-- 2) b
-- 3) a
-- 4) c


