{-# LANGUAGE FlexibleInstances #-}
import GHC.Arr
-- Exercises: Be Kind

-- 1) *
-- 2) b :: * -> *, T :: * -> *
-- 3) c :: * -> * -> *

-- Exercises: Heavy Lifting
a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (*2) (\x -> x - 2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed

-- 16.10 Exercises: Instances of Func
newtype Identity a = Identity a
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

data Two a b = Two a b
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

data Three a b c = Three a b c
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

data Three' a b = Three' a b b
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b
instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

data Trivial = Trivial
-- not possible, Trivial :: *

-- Exercise: Possibly
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers (f a)
  fmap f LolNope = LolNope

-- Short Exercise
data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

-- f :: * -> * 

-- 16.17 Chapter exercises

data Bool = False | True
-- no

data BoolAndSomethingElse a = False' a | True' a
-- yes

data BoolAndMaybeSomethingElse a = Falsish | Truish a
-- yes

newtype Mu f = Inf { outF :: f (Mu f)}
-- no

data D = D (Array Word Word) Int Int
-- no

data Sum0 a b = First0 b | Second0 a
instance Functor (Sum0 e) where
  fmap f (First0 a) = First0 (f a)
  fmap f (Second0 b) = Second0 b

data Company a b c = DeepBlue a b | Something c
instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More a b = L b a b | R a b a deriving (Eq, Show)
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


data Quant a b =  Finance | Desk a | Bloor b
instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ (Desk a) = Desk a
  fmap _ Finance = Finance

data K' a b = K' a
instance Functor (K' a) where
  fmap _ (K' a) = K' a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype K a b = K a
instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a = Nil | Cons a (List a)
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data GoatLord a = 
  NoGoat 
  | OneGoat a 
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats ga ga' ga'') = 
    MoreGoats (fmap f ga) (fmap f ga') (fmap f ga'')

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read (f . g)