module Day02 where

import Text.Regex.PCRE

pattern = "(.+)-(.+) (.): (.+)"

occurences char str = length (filter (==char) str)
policy1 min max char str = (occurences char str) >= min && (occurences char str) <= max

charAt p str = if length str > p then str !! p else ' '
policy2 p1 p2 char str = (charAt (p1 - 1) str == char) /= (charAt (p2 - 1) str == char)

applyPolicy1 [min, max, char, str] = policy1 (read min :: Int) (read max :: Int) (head char) str
applyPolicy2 [min, max, char, str] = policy2 (read min :: Int) (read max :: Int) (head char) str

validCount xs = length (filter (==True) xs)

parse :: String -> [String]
parse input = drop 1 (getAllTextSubmatches $ input =~ pattern :: [String])

solve input = do
  let pwData = map parse input

  print (validCount (map applyPolicy1 pwData))
  print (validCount (map applyPolicy2 pwData))
