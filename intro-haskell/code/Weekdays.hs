module Weekdays
(
  Weekday
, next
) where

-- usually provided in own module/file
class Eq' a where
    -- point-free default impls.
    -- provide one of (==') or (/=')
    (===) :: a -> a -> Bool
    (/==) :: a -> a -> Bool
    l === r = not $ l /== r
    l /== r = not $ l === r

data Weekday =
    Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday deriving (Show, Eq)

instance Eq' Weekday where
    Monday === Monday = True
    Tuesday === Tuesday = True
    Wednesday === Wednesday = True
    Thursday === Thursday = True
    Friday === Friday = True
    Saturday === Saturday = True
    Sunday === Sunday = True
    _ === _ = False

next :: Weekday -> Weekday
next day = case day of
  Monday -> Tuesday
  Tuesday -> Wednesday
  Wednesday -> Thursday
  Thursday -> Friday
  Friday -> Saturday
  Saturday -> Sunday
  Sunday -> Monday
