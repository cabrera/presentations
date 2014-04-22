module Person
(
  Person
, age
, name
) where

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show, Eq)

