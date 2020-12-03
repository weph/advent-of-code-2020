module Main where

toTuple x = (x, x)

listToTuple x = map toTuple x

combine a b = (fst a + fst b, snd a * snd b)

combineTwo [] = []
combineTwo (x:xs) = (map (combine x) xs) ++ combineTwo xs

combineThree [] = []
combineThree (x:xs) = (map (combine x) (combineTwo xs)) ++ combineThree xs

withSum x y = map snd (filter ((==x).fst) y)

main = do
  input <- getContents

  let numbers = map read (lines input) :: [Integer]

  print (withSum 2020 (combineTwo (listToTuple numbers)))
  print (withSum 2020 (combineThree (listToTuple numbers)))
