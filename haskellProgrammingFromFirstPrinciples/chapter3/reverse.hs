module Reverse where

rvrs :: String -> String
rvrs x = third ++ " " ++ second ++ " " ++ first 
  where first = take 5 x
        second = take 2 (drop 6 x )
        third = take 7 (drop 9 x)


main :: IO ()
main = print $ rvrs "Curry is awesome"