module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import Data.List (sort)

import qualified WordNumberTest as WNT
import Exercise

genFool :: Gen Fool
genFool = oneof [return Fulse, return Frue]

chanceFool :: Gen Fool
chanceFool = frequency [(2, return Fulse), (1, return Frue)]

instance Arbitrary Fool where
  arbitrary = genFool

main :: IO ()
main = do
  WNT.main

  hspec $ do
    describe "halfIdentity" $ do
      it "halfIdentity returns itself" $ do
        property (\x -> halfIdentity x == x)

    describe "listOrdered" $ do
      it "listOrdered returns True if list is sorted" $ do
        property (\xs -> listOrdered (sort xs :: [Int]))

    describe "plusAssociative" $ do
      it "plusAssociative returns True" $ do
        property (plusAssociative :: Int -> Int -> Int -> Bool)

    describe "plusCommutative" $ do
      it "plusCommutative returns True" $ do
        property (plusCommutative :: Int -> Int -> Bool) 

    describe "multiplyAssociative" $ do
      it "multiplyAssociative returns True" $ do
        property (multiplyAssociative :: Int -> Int -> Int -> Bool)

    describe "multiplyCommutative" $ do
      it "multiplyCommutative returns True" $ do
        property (multiplyCommutative :: Int -> Int -> Bool)

    describe "quotRem" $ do
      it "quotRem returns True" $ do
        property (multiplyAssociative :: Int -> Int -> Int -> Bool)

    describe "divMod" $ do
      it "divMod returns True" $ do
        property (multiplyCommutative :: Int -> Int -> Bool)

    -- describe "powerAssociative" $ do
    --   it "powerAssociative test" $ do
    --     property (powerAssociative :: Int -> Int -> Int -> Bool)

    -- describe "powerCommutative" $ do
    --   it "powerCommutative test" $ do
    --     property (powerCommutative :: Int -> Int -> Bool)

    describe "reverseTwice" $ do
      it "reverseTwice returns True" $ do
        property (reverseTwice :: [Int] -> Bool)

    describe "dollarFn" $ do
      it "dollarFn returns True" $ do
        property (\(Fn f) x -> 
          (dollarFn :: (Int -> Int) -> Int -> Bool) f x)

    describe "dotFn" $ do
      it "dotFn returns True" $ do
        property (\(Fn f) (Fn g) x -> 
          (dotFn :: (Int -> Int) -> (Int -> Int) -> Int -> Bool) f g x)

    describe "constructorCheck" $ do
      it "constructorCheck" $ do
        property (constructorCheck :: String -> String -> Bool)
    
    describe "appendCheck" $ do
      it "appendCheck" $ do
        property (appendCheck :: [[String]] -> Bool)

    -- describe "isThatSo" $ do
    --   it "isThatSo" $ do
    --     property (isThatSo :: Int -> [String] -> Bool)

    describe "readShow" $ do
      it "readShow" $ do
        property (readShow :: Int -> Bool)

    -- describe "squareIdentity" $ do
    --   it "squareIdentity" $ do
    --     property (squareIdentity :: Double -> Bool)

    describe "capitalizeWordCheck" $ do
      it "capitalizeWordCheck" $ do
        property capitalizeWordCheck


