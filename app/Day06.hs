module Day06 where

import Data.Char
import Data.List
import Data.List.Split

countAny group = length (nub (filter isAlpha group))
countAll group = length (foldl1 (\acc x -> acc `intersect` x) (lines group))

solve input = do
  let groups = splitOn "\n\n" input

  putStrLn ("Part 1: " ++ (show (sum (map countAny groups))))
  putStrLn ("Part 2: " ++ (show (sum (map countAll groups))))
