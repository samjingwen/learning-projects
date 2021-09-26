
-- Exercises: Eq Instances
data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  TisAn a == TisAn a' = a == a'

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  Two a b == Two a' b' = a == a' && b == b'

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  TisAnInt a == TisAnInt a' = a == a'
  TisAString a == TisAString a' = a == a'
  _ == _ = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  Pair x y == Pair x' y' = x == x' && y == y'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple a b == Tuple a' b' = a == a' && b == b'

data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
  ThisOne a == ThisOne a' = a == a'
  ThatOne a == ThatOne a' = a == a'

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello a == Hello a' = a == a'
  Goodbye a == Goodbye a' = a == a'


-- Exercises: Will They Work?
-- 1) 5
-- 2) LT
-- 3) won't work
-- 4) False


-- Chapter Exercises
-- 1) c
-- 2) b
-- 3) a
-- 4) c
-- 5) a

-- Does it typecheck?

-- 1) no, no instance for Show Person
data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

--2) no, no instance for Eq Mood
data Mood = Blah
  | Woot deriving (Show, Eq)
settleDown x = if x == Woot
  then Blah
  else x

--3a) Blah or Woot
--3b) no instance for Num Mood
--3c) no instance for Ord Mood

-- 4) yes

-- Given a datatype declaration, what can we do?
data Rocks =
  Rocks String deriving (Eq, Show)
data Yeah =
  Yeah Bool deriving (Eq, Show)
data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

-- 1) no
phew = Papu (Rocks "chases") (Yeah True)
-- 2) yes
-- 3) yes
-- 4) no, no instance for Ord Papu


-- Match the types
-- 1) 1 :: Num a => a
-- 2) 1.0 :: Fractional a => a
-- 3) yes
-- 4) yes
-- 5) yes
-- 6) yes
-- 7) no, cannot match a to Int
-- 8) no, cannot match a to Int
-- 9) yes
-- 10) yes
-- 11) no, cannot match a with Char


-- Type-Kwon-Do Two: Electric Typealoo
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

arith :: Num b
  => (a -> b)
  -> Integer
  -> a
  -> b
arith f n x = fromInteger n + f x

