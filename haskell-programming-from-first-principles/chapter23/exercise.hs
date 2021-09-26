{-# LANGUAGE InstanceSigs #-}

import System.Random
import Control.Monad
import Control.Monad.Trans.State

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x ->
      error $
        "intToDie got non 1-6 integer: "
          ++ show x

-- Exercises: Roll Your Own
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum res gen
      | sum >= n = res
      | otherwise =
        let (die, nextGen) = randomR (1, 5) gen
        in go (sum + die) (fst res + 1, intToDie die : snd res) nextGen

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi
      ( \s ->
          let (a, gs) = g s
          in (f a, gs)
      )

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi
      ( \s ->
          let (ab, fs) = f s
              (a, gs) = g fs
          in (ab a, gs)
      )

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g =
    Moi
      ( \s ->
          let (a, fs) = f s
          in runMoi (g a) fs
      )

-- Fizzbuzz Differently
fizzBuzz :: Integer -> String
fizzBuzz n 
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo = undefined

main :: IO ()
main =
  mapM_ putStrLn $
    fizzbuzzList  [100, 99..1]

-- Chapter exercises
get' :: Moi s s
get' = Moi (\s -> (s, s))

put' :: s -> Moi s ()
put' s = Moi (\_ -> ((), s))

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

modify' :: (s -> s) -> Moi s ()
modify' f = Moi (\s -> ((), f s))

