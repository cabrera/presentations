module Misc
(
  describeList
, validArea  
) where

describeList :: [a] -> String
describeList xs = case xs of
  [] -> "empty"
  [_] -> "singleton"
  _ -> "longer list"

validArea :: (Ord a, Num a) => a -> a -> Bool
validArea x y
  | area x y >= 0 = True
  | otherwise = False
  where area x' y' = x' * y'
