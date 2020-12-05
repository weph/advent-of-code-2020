module Main where

import Data.List

combinations 0 lst = [[]]
combinations n lst = do
    (x:xs) <- tails lst
    rest   <- combinations (n-1) xs
    return $ x : rest

value x = if sum x == 2020 then product x else 0

findValue lst = filter (/=0) (map value lst)

main = do
  input <- getContents

  let numbers = map read (lines input) :: [Integer]

  print (findValue (combinations 2 numbers))
  print (findValue (combinations 3 numbers))
