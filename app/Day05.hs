module Day05 where

charToBit x | x `elem` "FL" = 0 | x `elem` "BR" = 1

seatId input = foldl (\acc x -> acc * 2 + (charToBit x)) 0 input

emptySeat seatIds = (head (filter (\x -> ((x + 1) `notElem` seatIds) && ((x + 2) `elem` seatIds)) seatIds)) + 1

solve input = do
  let seatIds = map seatId (lines input)

  putStrLn ("Part 1: " ++ (show (maximum seatIds)))
  putStrLn ("Part 2: " ++ (show (emptySeat seatIds)))
