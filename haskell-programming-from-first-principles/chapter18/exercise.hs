import Control.Monad (join)
import Data.Monoid (Sum, Product)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

-- Short Exercise: Either Monad
data Sum' a b = First' a | Second' b deriving (Eq, Show)
instance Functor (Sum' a) where
  fmap f (First' a) = First' a
  fmap f (Second' b) = Second' (f b)
instance Applicative (Sum' a) where
  pure x = Second' x
  (<*>) (Second' f) (Second' x) = Second' (f x)
  (<*>) (First' f) _ = First' f
  (<*>) _ (First' x) = First' x
instance Monad (Sum' a) where
  return = pure
  (>>=) (Second' x) f = f x
  (>>=) (First' x) _ = First' x

-- Chapter Exercises

data Nope a = NopeDotJpg deriving (Eq, Show)
instance Functor Nope where
  fmap _ _ = NopeDotJpg
instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg
instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg
instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg
instance (Eq a) => EqProp (Nope a) where
  (=-=) = eq

data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)
instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap _ (Right' b) = Right' b
instance Applicative (PhhhbbtttEither b) where
  pure x = Left' x
  (<*>) (Left' f) (Left' a) = Left' (f a)
  (<*>) (Right' b) _  = Right' b
  (<*>) _ (Right' b) = Right' b
instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (Left' a) f = f a
  (>>=) (Right' b) _ = Right' b
instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = frequency [(3, Left' <$> arbitrary), (1, Right' <$> arbitrary)]
instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)
instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary
instance Eq a => EqProp (Identity a) where
  (=-=) = eq

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = fmap f xs `append` (fs <*> xs)
instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = f x `append` (xs >>= f)
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (3, Cons <$> arbitrary <*> arbitrary)]
instance Eq a => EqProp (List a) where
  (=-=) = eq

j :: Monad m => m (m a) -> m a
j x = x >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = (<$>)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = ma >>= \x -> mb >>= \y -> return $ f x y

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = ma >>= \x -> mf >>= \f -> return $ f x

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = pure []
meh (x:xs) f = f x >>= \b -> (b :) <$> meh xs f

flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas id 

main = do
  hspec $ do
    describe "Nope" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Nope (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative (undefined :: Nope (Int, Float, String)))
      it "Monad" $
        property
          (quickBatch $ monad (undefined :: Nope (Int, Float, String)))
    describe "PhhhbbtttEither" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: PhhhbbtttEither (Int, Float, String) (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative (undefined :: PhhhbbtttEither (Int, Float, String) (Int, Float, String)))
      it "Monad" $
        property
          (quickBatch $ monad (undefined :: PhhhbbtttEither (Int, Float, String) (Int, Float, String)))
    describe "Identity" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: Identity (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative (undefined :: Identity (Int, Float, String)))
      it "Monad" $
        property
          (quickBatch $ monad (undefined :: Identity (Int, Float, String)))
    describe "List" $ do
      it "Functor" $
        property (quickBatch $ functor (undefined :: List (Int, Float, String)))
      it "Applicative" $
        property
          (quickBatch $ applicative (undefined :: List (Int, Float, String)))
      it "Monad" $
        property
          (quickBatch $ monad (undefined :: List (Int, Float, String)))