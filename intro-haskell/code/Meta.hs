{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T

meta :: [(T.Text, T.Text)]
meta = [
  ("Author", "Allele Dev")
  , ("Email", "allele.dev@gmail.com")
  , ("Objectives", "Introduce: Haskell, Types, FP")
  ]

main :: IO ()
main = print meta
