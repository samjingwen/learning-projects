{-# LANGUAGE InstanceSigs #-}

import Data.Char

-- Short Exercise: Warming Up

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
-- tupled = (,) <$> rev <*> cap
-- tupled = do
--   a <- rev
--   b <- cap
--   return (a, b)
tupled = cap >>= \b -> rev >>= \a -> return (a, b)

newtype Reader r a = Reader { runReader :: r -> a }

-- Exercise: Ask
ask :: Reader a a
ask = Reader id

-- Exercise: Reading Comprehension
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader (const a)
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader (\r -> rab r (ra r))

-- Exercise: Reader Monad
instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

-- with Reader Monad
newtype HumanName =
  HumanName String
  deriving (Eq, Show)
newtype DogName =
  DogName String
  deriving (Eq, Show)
newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
  humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)
data Dog =
  Dog {
  dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

getDogRM :: Reader Person Dog
getDogRM = do
  name <- Reader dogName
  addy <- Reader address
  return $ Dog name addy


