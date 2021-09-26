module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

myMultiply :: (Integral a) => a -> a -> a
myMultiply = go
  where go sum count
          | sum == 0 = 0
          | count == 0 = 0
          | count == 1 = sum
          | otherwise = go sum (count - 1) + sum

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
    it "0 * 5 is 0" $ do
      myMultiply 0 5 `shouldBe` 0
    it "5 * 0 is 0" $ do
      myMultiply 5 0 `shouldBe` 0
    it "3 * 4 is 12" $ do
      myMultiply 3 4 `shouldBe` 12
    it "56 * 78 is 4368" $ do
      myMultiply 56 78 `shouldBe` 4368
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

