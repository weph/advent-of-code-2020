module Day03 where

tree x map = if (map !! (x `mod` (length map))) == '#' then 1 else 0

traverseMap :: Int -> Int -> Int -> [String] -> Int
traverseMap right down px [] = 0
traverseMap right down px (x:xs) = tree px x + traverseMap right down (px + right) (drop (down - 1) xs)

part1 mapData = traverseMap 3 1 0 mapData

part2 mapData = (traverseMap 1 1 0 mapData) *
  (traverseMap 3 1 0 mapData) *
  (traverseMap 5 1 0 mapData) *
  (traverseMap 7 1 0 mapData) *
  (traverseMap 1 2 0 mapData)

solve input = do
  let mapData = lines input

  putStrLn ("Part 1: " ++ (show (part1 mapData)))
  putStrLn ("Part 2: " ++ (show (part2 mapData)))
