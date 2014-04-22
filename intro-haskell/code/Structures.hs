module Structures
(
  List'
, head'
, Tree
, insert
) where

data List' a =
  Nil
  | Cons a (List' a) deriving (Show, Eq)

head' :: List' a -> Maybe a
head' Nil = Nothing
head' (Cons x _) = Just x

data Tree a =
  EmptyTree
  | Node a (Tree a) (Tree a) deriving (Show, Eq)

insert :: Ord a => a -> Tree a -> Tree a
insert v EmptyTree = Node v EmptyTree EmptyTree
insert v (Node n l r)
  | v == n = Node v l r  -- create identical node in place
  | v < n = Node n (insert v l) r
  | v > n = Node n l (insert v r)
