import Control.Applicative
import Data.List (elemIndex)
import Data.Monoid
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercises: Lookups
-- 1)
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2)
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z

-- 3)
x3 :: Maybe Int
x3 = elemIndex 3 [1, 2, 3, 4, 5]

y3 :: Maybe Int
y3 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max' x3  y3

-- 4)
xs = [1, 2, 3]
ys = [4, 5, 6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs ys

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> liftA2 (,) x4 y4

-- Exercise: Identity Instance
newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

-- Exercise: Constant Instance
newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)
instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x
instance Monoid a => Applicative (Constant a) where
  pure a = Constant mempty
  (<*>) (Constant f) (Constant x) = Constant (f <> x)

-- Exercise: Fixer Upper

q1 = const <$> Just "Hello" <*> pure "World"

q2 = (,,,) <$> Just 90 <*> Just 10 <*>
  Just "Tierness" <*> pure [1, 2, 3]

-- List Applicative Exercise
data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = fmap f xs `append` (fs <*> xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f Nil = Nil
flatMap f xs = concat' $ fold (Cons . f) Nil xs

-- ZipList Applicative Exercise
take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' num (Cons x xs) = Cons x (take' (num - 1) xs)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
            in take' 3000 l
          ys' = let (ZipList' l) = ys
            in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

repeat' :: a -> List a
repeat' x = xs where xs = Cons x xs

instance Applicative ZipList' where
  pure x = ZipList' $ repeat' x
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons x xs)) =
    ZipList' $ Cons (f x) ys
      where (ZipList' ys) = ZipList' fs <*> ZipList' xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (2, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

-- Exercise: Variations on Either
data Validation e a = Failed e | Succeeded a deriving (Eq, Show)
instance Functor (Validation e) where
  fmap f (Succeeded a) = Succeeded (f a)
  fmap _ (Failed a) = Failed a
instance Monoid e => Applicative (Validation e) where
  pure x = Succeeded x
  (<*>) (Failed f) (Failed a) = Failed (f <> a)
  (<*>) (Failed f) (Succeeded a) = Failed f
  (<*>) (Succeeded f) (Failed a) = Failed a
  (<*>) (Succeeded f) (Succeeded a) = Succeeded (f a)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = frequency [(1, Failed <$> arbitrary), (2, Succeeded <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (Validation a b) where
  (=-=) = eq

-- 17.9 Chapter Exercises

-- 1)
-- pure :: a -> [a]
-- (<*>) :: [(a -> b)] -> [a] -> [b]

-- 2)
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3)
-- pure :: Monoid a => b -> (a, b)
-- (<*>) :: Monoid a => (a, (b1 -> b2)) -> (a, b2)

-- 4)
-- pure :: a -> (e -> a)
-- (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)


data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b
instance Eq a => EqProp (Pair a) where
  (=-=) = eq

data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two a f) (Two a' b') = Two (a <> a') (f b')
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b
instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three a b f) (Three a' b' c') = Three (a <> a') (b <> b') (f c')
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')
instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' a f f') (Three' a' x x') = Three' (a <> a') (f x) (f' x')
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c
instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four a b c f) (Four a' b' c' x) = Four (a <> a') (b <> b') (c <> c') (f x)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d
instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

data Four' a b = Four' a a a b deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)
instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (<*>) (Four' a b c f) (Four' a' b' c' x) = Four' (a <> a') (b <> b') (c <> c') (f x)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d
instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq


stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)


main :: IO ()
main =
  hspec $ do
    describe "List" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: List (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative (undefined :: List (Int, Float, String)))
    describe "ZipList'" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: ZipList' (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative (undefined :: ZipList' (Int, Float, String)))
    describe "Validation" $ do
      it "Functor" $
        property
          (quickBatch $ functor
            (undefined :: Validation (Sum Int, Product Int, String) (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative
            (undefined :: Validation (Sum Int, Product Int, String) (Int, Float, String)))
    describe "Pair" $ do
      it "Functor" $
        property
          (quickBatch $ functor
            (undefined :: Pair (Sum Int, Product Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative
            (undefined :: Pair (Sum Int, Product Float, String)))
    describe "Two" $ do
      it "Functor" $
        property
          (quickBatch $ functor
            (undefined :: Two (Sum Int, Product Int, String) (Sum Int, Product Int, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative
            (undefined :: Two (Sum Int, Product Int, String) (Sum Int, Product Int, String)))
    describe "Three" $ do
      it "Functor" $
        property
          (quickBatch $ functor
            (undefined :: Three (Sum Int, Product Int, String) (Sum Int, Product Int, String) (Sum Int, Product Int, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative
            (undefined :: Three (Sum Int, Product Int, String) (Sum Int, Product Int, String) (Sum Int, Product Int, String)))
    describe "Three'" $ do
      it "Functor" $
        property
          (quickBatch $ functor
            (undefined :: Three' (Sum Int, Product Int, String) (Sum Int, Product Int, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative
            (undefined :: Three' (Sum Int, Product Int, String) (Sum Int, Product Int, String)))
    describe "Four" $ do
      it "Functor" $
        property
          (quickBatch $ functor
            (undefined :: Four (Sum Int, Product Int, String) (Sum Int, Product Int, String) (Sum Int, Product Int, String) (Sum Int, Product Int, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative
            (undefined :: Four (Sum Int, Product Int, String) (Sum Int, Product Int, String) (Sum Int, Product Int, String) (Sum Int, Product Int, String)))
    describe "Four'" $ do
      it "Functor" $
        property
          (quickBatch $ functor
            (undefined :: Four' (Sum Int, Product Int, String) (Sum Int, Product Int, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative
            (undefined :: Four' (Sum Int, Product Int, String) (Sum Int, Product Int, String)))


