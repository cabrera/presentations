module AltEq
(
  Eq'
) where

class Eq' a where
    -- point-free default impls.
    -- provide one of (==') or (/=')
    (===) :: a -> a -> Bool
    (/==) :: a -> a -> Bool
    l === r = not $ l /== r
    l /== r = not $ l === r
