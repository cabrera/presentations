module Factorial
(
  equationalFactorial
, guardedFactorial
) where

equationalFactorial :: Integer -> Integer
equationalFactorial 0 = 1
equationalFactorial n = n * equationalFactorial (n - 1)

guardedFactorial :: Integer -> Integer
guardedFactorial n
  | n <= 0 = 0
  | otherwise = n * guardedFactorial (n - 1)