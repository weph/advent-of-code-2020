module Day03 where

tree x map = if (map !! (x `mod` (length map))) == '#' then 1 else 0

traverseMap :: [String] -> Int -> Int -> Int -> Int
traverseMap [] px right down = 0
traverseMap (x:xs) px right down = tree px x + traverseMap (drop (down - 1) xs) (px + right) right down

solve input = do
  let traverse = traverseMap (lines input) 0

  putStrLn ("Part 1: " ++ (show (traverse 3 1)))
  putStrLn ("Part 2: " ++ (show ((traverse 1 1) * (traverse 3 1) * (traverse 5 1) * (traverse 7 1) * (traverse 1 2))))
