module Main where

import qualified Day01

main = do
  input <- getContents

  Day01.solve (lines input)
