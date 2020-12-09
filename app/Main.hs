module Main where

import System.Environment

import qualified Day01
import qualified Day02
import qualified Day03

solve day input =
  case day of
    "1" -> Day01.solve input
    "2" -> Day02.solve input
    "3" -> Day03.solve input
    _ -> error $ "Invalid argument: " ++ day

main = do
  [arg] <- getArgs
  input <- getContents

  solve arg (lines input)
