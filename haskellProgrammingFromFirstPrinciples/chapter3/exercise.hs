
-- Exercises: Scope

-- 1) yes
-- 2) no
-- 3) no
-- 4) yes

-- Exercises: Syntax Errors

q1 = (++) [1, 2, 3] [4, 5, 6]

q2 = "<3" ++ " Haskell"

q3 = concat ["<3", " Haskell"]

-- Chapter Exercises

q1a = concat [[1, 2, 3], [4, 5, 6]]

q1b = (++) [1, 2, 3] [4, 5, 6]

q1c = (++) "hello" " world"

q1d = ["hello" ++ " world"]

q1e = "hello" !! 4

q1f = (!!) "hello" 4

q1g = take 4 "lovely"

q1h = take 3 "awesome"

-- 2)
-- a = d, b = c, c = e, d = a, e = b

qns1a x = x ++ "!"

qns1b x = drop 4 x

qns1c x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

