import Control.Applicative (liftA3)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Chapter Exercises

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure $ Constant x

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (3, Yep <$> arbitrary)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (3, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big a b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Big a b b'

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger a b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    b'' <- arbitrary
    return $ Bigger a b b' b''

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  deriving (Eq, Show)
instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)
instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node left a right) = foldMap f left <> f a <> foldMap f right
instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node left a right) =
    Node <$> traverse f left <*> f a <*> traverse f right
instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary =
    frequency
      [ (1, return Empty),
        (1, Leaf <$> arbitrary),
        (1, liftA3 Node arbitrary arbitrary arbitrary)
      ]
instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

main :: IO ()
main =
  hspec $ do
    describe "Identity" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Identity (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $ foldable (undefined :: Identity (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: Identity (Int, Float, String)))

    describe "Constant" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Constant Int (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $ foldable (undefined :: Constant Int (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: Constant Int (Int, Float, String)))

    describe "Optional" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Optional (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $ foldable (undefined :: Optional (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: Optional (Int, Float, String)))

    describe "List" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: List (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $ foldable (undefined :: List (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: List (Int, Float, String)))

    describe "Three" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Three Int String (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $ foldable (undefined :: Three Int String (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: Three Int String (Int, Float, String)))

    describe "Pair" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Pair Int (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $ foldable (undefined :: Pair Int (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: Pair Int (Int, Float, String)))

    describe "Big" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Big Int (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $ foldable (undefined :: Big Int (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: Big Int (Int, Float, String)))

    describe "Bigger" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Bigger Int (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $ foldable (undefined :: Bigger Int (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: Bigger Int (Int, Float, String)))

    describe "Tree" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: Tree (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $ foldable (undefined :: Tree (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: Tree (Int, Float, String)))
