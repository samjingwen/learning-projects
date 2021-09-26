
-- Exercises: Comprehension Check

-- 1)
-- let half x = x / 2
-- let square x = x * x

-- 2)
area1 r = 3.14 * r * r

-- 3)
area2 r = pi * r * r

-- Exercises: Parentheses and Association

-- 1) Not the same
-- 2) Same
-- 3) Not the same

-- Exercises: Heal the Sick

-- 1)
area x = 3.14 * (x * x)

-- 2)
double x = x * 2

-- 3)
f = x + y
  where x = 7
        y = 10


-- Exercises: A Head Code
-- 1) 5
-- 2) 25
-- 3) 30
-- 4) 6


-- Chapter Exercises

qns1 = 2 + (2 * 3) - 1

qns2 = (^) 10 (1 + 1)

qns3 = ((2 ^ 2) * (4 ^ 5)) + 1

-- 1) Same
-- 2) Same
-- 3) Not the same
-- 4) Not the same
-- 5) Not the same

z = 7
x = y ^ 2
waxOn = x * 5
y = z + 8

-- 1) 1135, 1135, -1110, 1110

triple x = x * 3

-- 3) 3375

-- 4)
waxOn2 = x * 5
  where x = y ^ 2
        z = 7
        y = z + 8

waxOff x = triple x


