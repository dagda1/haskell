module Hacker where

fibonacci :: Int -> Int
fibonacci n
  | n < 2 = n
  | otherwise = fibonacci(n - 1) + fibonacci(n - 2)

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]
