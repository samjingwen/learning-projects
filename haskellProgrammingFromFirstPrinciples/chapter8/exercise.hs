
-- Intermission: Exercise

{-|

applyTimes 5 (+1) 5
(+1) (applyTimes 4 (+1) 5)
(+1) ((+1) applyTimes 3 (+1) 5)
(+1) ((+1) ((+1) applyTimes 2 (+1) 5))
(+1) ((+1) ((+1) ((+1) applyTimes 1 (+1) 5)))
(+1) ((+1) ((+1) ((+1) ((+1) applyTimes 0 (+1) 5))))
(+1) ((+1) ((+1) ((+1) ((+1) 5))))
(+1) ((+1) ((+1) ((+1) 6)))
(+1) ((+1) ((+1) 7))
(+1) ((+1) 8)
(+1) 9
10

-}

-- Chapter Exercises
-- 1) d
-- 2) b
-- 3) d
-- 4) b

-- Reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1) "woops mrow woohoo!"
-- 2) "1 mrow haha"
-- 3) "woops mrow 2 mrow haha"
-- 4) "woops mrow blue mrow haha"
-- 5) "pink mrow haha mrow green mrow woops mrow blue"
-- 6) "are mrow Pugs mrow awesome"

-- Recursion

-- dividedBy 15 2
-- go 15 2 0
-- go 13 2 1
-- go 11 2 2
-- go 9 2 3
-- go 7 2 4
-- go 5 2 5
-- go 3 2 6
-- go 1 2 7
-- (7, 1)

-- 2)
myRecursiveSum :: (Eq a, Num a) => a -> a
myRecursiveSum n = go n 0
  where go count sum
          | count == 0 = sum
          | otherwise = go (count - 1) (sum + count)

-- 3)
myMultiply :: (Integral a) => a -> a -> a
myMultiply = go
  where go sum count
          | sum == 0 = 0
          | count == 0 = 0
          | count == 1 = sum
          | otherwise = go sum (count - 1) + sum

-- Fixing dividedBy
data DividedResult = Result (Integer, Integer) | DividedByZero deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom = go num denom 0
  where go n d count
          | d == 0 = DividedByZero
          | n < d = Result (count, n)
          | otherwise = go (n - d) d (count + 1)

mc91 n 
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))