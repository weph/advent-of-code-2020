module Main where

import System.Environment
import System.Exit
import System.IO

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05

solve day input =
  case day of
    "1" -> Day01.solve input
    "2" -> Day02.solve input
    "3" -> Day03.solve input
    "4" -> Day04.solve input
    "5" -> Day05.solve input
    _ -> error $ "Invalid day: " ++ day

usage = do
  name <- getProgName
  putStrLn ("Usage: " ++ name ++ " day input")
  exitWith ExitSuccess

run [] = usage
run [day] = usage
run [day, filename] = do
  handle <- openFile filename ReadMode
  input <- hGetContents handle
  solve day input

main = do
  args <- getArgs
  run args
