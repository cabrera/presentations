module Geometry.Circle
( area
, perimeter
) where

-- the most accurate; more accurate than Prelude.pi
pi' :: Float
pi' = 3.1415926

area :: Float -> Float
area r = pi' * r**2

perimeter :: Float -> Float
perimeter r = 2 * pi' * r
