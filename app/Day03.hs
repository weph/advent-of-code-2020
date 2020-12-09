module Day03 where

tree x map = if (map !! (x `mod` (length map))) == '#' then 1 else 0

traverseMap :: Int -> Int -> Int -> [String] -> Int
traverseMap right down px [] = 0
traverseMap right down px (x:xs) = tree px x + traverseMap right down (px + right) (drop (down - 1) xs)

solve input = do
  print (traverseMap 3 1 0 input)
  print ((traverseMap 1 1 0 input) * (traverseMap 3 1 0 input) * (traverseMap 5 1 0 input) * (traverseMap 7 1 0 input) * (traverseMap 1 2 0 input))
