module Misc
(
  describeList
, validArea
, analyzeNumber
, starter
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

analyzeNumber :: (Ord a, Num a) => a -> Bool
analyzeNumber n =
  let analyze n' = (n' * n')
      reasonable n' = analyze n' > 2
  in
    reasonable n

starter :: String -> String
starter "" = "empty"
starter all_xs@(x:_) = all_xs ++ " starts with " ++ [x]
