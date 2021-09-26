import Data.Monoid
import Test.QuickCheck

-- Exercise: Optional Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  (Only a) <> (Only a') = Only (a <> a')
  Nada <> Nada = Nada

-- 15.11 Madness
type Verb = String

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbin' ::
  Exclamation ->
  Adverb ->
  Noun ->
  Adjective ->
  String
madlibbin' e adv noun adj =
  e <> "! he said "
    <> adv
    <> " as he jumped into his car "
    <> noun
    <> " and drove off with his "
    <> adj
    <> " wife."

madlibbinBetter' ::
  Exclamation ->
  Adverb ->
  Noun ->
  Adjective ->
  String
madlibbinBetter' e adv noun adj =
  mconcat
    [ e,
      "! he said ",
      adv,
      " as he jumped into his car ",
      noun,
      " and drove off with his ",
      adj,
      " wife."
    ]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Exercise: Maybe Another Monoid

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada

instance Semigroup (First' a) where
  (First' (Only a)) <> _ = First' (Only a)
  _ <> (First' (Only a)) = First' (Only a)
  _ <> _ = First' Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(5, Only <$> arbitrary), (1, return Nada)]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
  First' String ->
  First' String ->
  First' String ->
  Bool

type FstId = First' String -> Bool

-- 15.15 Chapter exercises

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

--

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type TrivLeftIdentity = Trivial -> Bool

type TrivRightIdentity = Trivial -> Bool

--

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

type IdentityLeftIdentity a = Identity a -> Bool

type IdentityRightIdentity a = Identity a -> Bool

--

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

type TwoLeftIdentity a b = Two a b -> Bool

type TwoRightIdentity a b = Two a b -> Bool

--

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

--

data Four a b c d = Four a b c d deriving (Eq, Show)

instance
  (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d)
  where
  (Four a b c d) <> (Four a' b' c' d') =
    Four (a <> a') (b <> b') (c <> c') (d <> d')

instance
  (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d)
  where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

--

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolConjLeftIdentity = BoolConj -> Bool

type BoolConjRightIdentity = BoolConj -> Bool

--

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type BoolDisjLeftIdentity = BoolDisj -> Bool

type BoolDisjRightIdentity = BoolDisj -> Bool

--

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (Snd b) <> _ = Snd b
  (Fst a) <> (Snd b) = Snd b
  (Fst a) <> (Fst b) = Fst b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return (Fst a), return (Snd b)]

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

--

newtype Combine a b = Combine {unCombine :: a -> b}

instance Show (Combine a b) where
  show (Combine f) = "Combine stuff"

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (const mempty)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return (Combine f)

combineSemigroupAssoc ::
  (Eq b, Semigroup b) =>
  Combine a b ->
  Combine a b ->
  Combine a b ->
  a ->
  Bool
combineSemigroupAssoc f g h x =
  unCombine (f <> (g <> h)) x == unCombine ((f <> g) <> h) x

combineLeftIdentity ::
  (Eq b, Monoid b) =>
  Combine a b ->
  a ->
  Bool
combineLeftIdentity f x = unCombine (mempty <> f) x == (unCombine f) x

combineRightIdentity ::
  (Eq b, Monoid b) =>
  Combine a b ->
  a ->
  Bool
combineRightIdentity f x = unCombine (f <> mempty) x == (unCombine f) x

type CombineAssoc a b =
  Combine a b -> Combine a b -> Combine a b -> a -> Bool

type CombineLeftIdentity a b = Combine a b -> a -> Bool

type CombineRightIdentity a b = Combine a b -> a -> Bool

--

newtype Comp a = Comp {unComp :: a -> a}

instance Show (Comp a) where
  show (Comp a) = "Comp stuff"

instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (\x -> f x <> g x)

instance Monoid a => Monoid (Comp a) where
  mempty = Comp (const mempty)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return (Comp f)

compSemigroupAssoc ::
  (Eq a, Semigroup a) =>
  Comp a ->
  Comp a ->
  Comp a ->
  a ->
  Bool
compSemigroupAssoc f g h x = unComp (f <> (g <> h)) x == unComp ((f <> g) <> h) x

compLeftIdentity ::
  (Eq a, Monoid a) =>
  Comp a ->
  a ->
  Bool
compLeftIdentity f x = unComp (mempty <> f) x == (unComp f) x

compRightIdentity ::
  (Eq a, Monoid a) =>
  Comp a ->
  a ->
  Bool
compRightIdentity f x = unComp (f <> mempty) x == (unComp f) x

type CompAssoc a = Comp a -> Comp a -> Comp a -> a -> Bool

type CompLeftIdentity a = Comp a -> a -> Bool

type CompRightIdentity a = Comp a -> a -> Bool

--

data Validation a b = Failed a | Succeeded b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Failed a) <> (Failed a') = Failed (a <> a')
  (Succeeded b) <> _ = Succeeded b
  _ <> (Succeeded b) = Succeeded b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return (Failed a), return (Succeeded b)]

type ValidationAssoc a b =
  Validation a b -> Validation a b -> Validation a b -> Bool

--

newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance Show (Mem s a) where
  show _ = "Mem Mem Mem"

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\x -> (mempty, x))

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) =
    Mem
      ( \x ->
          ( (fst . f) x <> (fst . g) x,
            (snd . g . snd . f) x
          )
      )

instance
  (CoArbitrary s, Arbitrary s, Arbitrary a) =>
  Arbitrary (Mem s a)
  where
  arbitrary = do
    f <- arbitrary
    return (Mem f)

memSemigroupAssoc ::
  (Eq a, Eq s, Semigroup a) =>
  Mem s a ->
  Mem s a ->
  Mem s a ->
  s ->
  Bool
memSemigroupAssoc f g h x =
  runMem (f <> (g <> h)) x == runMem ((f <> g) <> h) x

memLeftIdentity ::
  (Eq a, Eq s, Monoid a) =>
  Mem s a ->
  s ->
  Bool
memLeftIdentity f x =
  runMem (mempty <> f) x == (runMem f) x

memRightIdentity ::
  (Eq a, Eq s, Monoid a) =>
  Mem s a ->
  s ->
  Bool
memRightIdentity f x =
  runMem (f <> mempty) x == (runMem f) x

type MemAssoc s a = Mem s a -> Mem s a -> Mem s a -> s -> Bool

type MemLeftIdentity s a = Mem s a -> s -> Bool

type MemRightIdentity s a = Mem s a -> s -> Bool

--

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: TrivLeftIdentity)
  quickCheck (monoidRightIdentity :: TrivRightIdentity)

  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (semigroupAssoc :: IdentityAssoc (Sum Int))
  quickCheck (semigroupAssoc :: IdentityAssoc (Product Int))
  quickCheck (monoidLeftIdentity :: IdentityLeftIdentity String)
  quickCheck (monoidRightIdentity :: IdentityRightIdentity (Sum Double))

  quickCheck (semigroupAssoc :: TwoAssoc String (Sum Int))
  quickCheck (monoidLeftIdentity :: TwoLeftIdentity String (Product Int))
  quickCheck (monoidRightIdentity :: TwoRightIdentity String (Product Int))

  quickCheck (semigroupAssoc :: ThreeAssoc String (Sum Int) (Product Int))

  quickCheck (semigroupAssoc :: FourAssoc String (Sum Int) (Product Int) String)

  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConjLeftIdentity)
  quickCheck (monoidRightIdentity :: BoolConjRightIdentity)

  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisjLeftIdentity)
  quickCheck (monoidRightIdentity :: BoolDisjRightIdentity)

  quickCheck (semigroupAssoc :: OrAssoc (Sum Int) (Product Int))

  quickCheck (combineSemigroupAssoc :: CombineAssoc (Sum Int) (Product Int))
  quickCheck (combineLeftIdentity :: CombineLeftIdentity (Sum Int) String)
  quickCheck (combineRightIdentity :: CombineRightIdentity (Product Int) String)

  quickCheck (compSemigroupAssoc :: CompAssoc (Sum Int))
  quickCheck (compLeftIdentity :: CompLeftIdentity (Sum Int))
  quickCheck (compLeftIdentity :: CompRightIdentity (Sum Int))

  quickCheck (semigroupAssoc :: ValidationAssoc (Sum Int) Float)
  let failure :: String -> Validation String Int
      failure = Failed
      success :: Int -> Validation String Int
      success = Succeeded
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

  let f' = Mem $ \s -> ("hi", s + 1)
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
  quickCheck (memSemigroupAssoc :: MemAssoc String (Sum Int))
  quickCheck (memLeftIdentity :: MemLeftIdentity String (Sum Int))
  quickCheck (memRightIdentity :: MemRightIdentity String (Sum Int))
