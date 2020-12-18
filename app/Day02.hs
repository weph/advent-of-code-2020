module Day02 where

import Text.Regex.PCRE

policy1 min max char str = cnt >= min && cnt <= max
  where cnt = length (filter (==char) str)

charAt p str = if length str > p then str !! p else ' '
policy2 p1 p2 char str = (charAt (p1 - 1) str == char) /= (charAt (p2 - 1) str == char)

isValid policy [min, max, char, str] = policy (read min :: Int) (read max :: Int) (head char) str

validCount xs = length (filter (==True) xs)

parse :: String -> [String]
parse input = drop 1 (getAllTextSubmatches $ input =~ "(.+)-(.+) (.): (.+)" :: [String])

solve input = do
  let pwData = map parse (lines input)

  putStrLn ("Part 1: " ++ (show (validCount (map (isValid policy1) pwData))))
  putStrLn ("Part 2: " ++ (show (validCount (map (isValid policy2) pwData))))
