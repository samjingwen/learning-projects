import Data.Monoid

-- Exercises: Library Functions
sum' :: (Foldable t, Num a) => t a -> a
sum' x = getSum $ foldMap Sum x

product' :: (Foldable t, Num a) => t a -> a
product' x = getProduct $ foldMap Product x

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\a b -> b || a == x) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr go Nothing
  where go a Nothing = Just a
        go a (Just b) = if a > b then Just b else Just a

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr go Nothing
  where go a Nothing = Just a
        go a (Just b) = if a > b then Just a else Just b

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ b -> b + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> f a <> b) mempty

-- Chapter Exercises
data Constant a b = Constant b
instance Foldable (Constant a) where
  foldr f z (Constant x) = f x z

data Two a b = Two a b
instance Foldable (Two a) where
  foldr f z (Two a b) = f b z

data Three a b c = Three a b c
instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z

data Three' a b = Three' a b b
instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b'
  foldr f z (Three' a b b') = (f b . f b') z

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
  foldMap f (Four' a b b' b'') = f b <> f b' <> f b''
  foldr f z (Four' a b b' b'') = (f b . f b' . f b'') z

filterF :: (Applicative f, Foldable t , Monoid (f a))
  => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then mempty else pure x) 


