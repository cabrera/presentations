{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T

meta :: [(T.Text, T.Text)]
meta = [
  ("Author", "Alejandro Cabrera")
  , ("Email", "cpp.cabrera@gmail.com")
  , ("Objectives", "Introduce: Haskell, Types, FP")
  ]

main :: IO ()
main = print meta
