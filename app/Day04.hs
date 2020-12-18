module Day04 where

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as Map

passportSchema = Map.fromList [
  ("byr", between 1920 2002),
  ("iyr", between 2010 2020),
  ("eyr", between 2020 2030),
  ("hgt", isValidHeight),
  ("hcl", isHexColor),
  ("ecl", isEyeColor),
  ("pid", isPid)
  ]

toPassport x = Map.fromList (map (\x -> (head x, last x)) (map (splitOn ":") (splitOneOf " \n" x)))
toPassports x = map toPassport x

between min max v = vi >= min && vi <= max
  where vi = (read v :: Int)

isHexColor v = hash == '#' && length values == 6 && all isHexDigit values
  where (hash:values) = v

isEyeColor v = v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isPid v = length v == 9 && all isDigit v

isValidHeight v =
  let unit = dropWhile isDigit v
      value = takeWhile isDigit v
  in (unit == "cm" && between 150 193 value) || (unit == "in" && between 59 76 value)

isFieldValid passport field isValid = case Map.lookup field passport of
  Nothing -> False
  Just x -> isValid x

countValid l = length (filter (==True) l)

hasAllRequiredFields schema passport = length ((Map.keys schema) \\ (Map.keys passport)) == 0

allFieldsValid schema passport = Map.foldrWithKey (\field validator acc -> acc && (isFieldValid passport field validator)) True schema

solve :: String -> IO ()
solve input = do
  let passports = toPassports (splitOn "\n\n" input)

  putStrLn ("Part 1: " ++ (show (countValid (map (hasAllRequiredFields passportSchema) passports))))
  putStrLn ("Part 2: " ++ (show (countValid (map (allFieldsValid passportSchema) passports))))
