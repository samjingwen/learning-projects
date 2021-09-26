module SkiFree where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)
instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary
instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq
instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)
instance (Foldable n, Functor n) => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a 
instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

main :: IO ()
main = 
  hspec $ do  
    describe "S" $ do
      it "Functor" $
        property
          (quickBatch $ functor (undefined :: S [] (Int, Float, String)))
      it "Foldable" $
        property
          (quickBatch $ foldable (undefined :: S [] (Int, Float, String, Int, String)))
      it "Traversable" $
        property
          (quickBatch $ traversable (undefined :: S [] (Int, Float, String)))
