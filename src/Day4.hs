module Day4
  ( meetsCriteria
  , passwordsMeetingCriteria
  , meetsExtendedCriteria
  , passwordsMeetingExtendedCriteria
  ) where

passwordsMeetingExtendedCriteria :: String -> Int
passwordsMeetingExtendedCriteria range = do
  let parts = wordsWhen (== '-') range
  let x1 = parts !! 0
  let x2 = parts !! 1
  let p1 = read x1 :: Int
  let p2 = read x2 :: Int
  length [password | password <- [p1 .. p2], meetsExtendedCriteria (show password)]

meetsExtendedCriteria :: String -> Bool
meetsExtendedCriteria password =
  length password == 6 &&
  neverDecreases password &&
  twoAdjacentDigitsAreSameAndNotPartOfBiggerGroup password

twoAdjacentDigitsAreSameAndNotPartOfBiggerGroup :: String -> Bool
twoAdjacentDigitsAreSameAndNotPartOfBiggerGroup password = do
  let curried = adjacentDigitsAndNotPartOfBiggerGroup password
  any curried (pairs password)

adjacentDigitsAndNotPartOfBiggerGroup :: String -> (Char, Char) -> Bool
adjacentDigitsAndNotPartOfBiggerGroup password pair =
  adjacentDigits pair && 2 == groupSize (fst pair) password

groupSize :: Char -> String -> Int
groupSize digit password = length $ filter (== digit) password

containsPartOfLargerGroup :: String -> Bool
containsPartOfLargerGroup password = True

passwordsMeetingCriteria :: String -> Int
passwordsMeetingCriteria range = do
  let parts = wordsWhen (== '-') range
  let x1 = parts !! 0
  let x2 = parts !! 1
  let p1 = read x1 :: Int
  let p2 = read x2 :: Int
  length [password | password <- [p1 .. p2], meetsCriteria (show password)]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

meetsCriteria :: String -> Bool
meetsCriteria password =
  length password == 6 &&
  neverDecreases password && twoAdjacentDigitsAreSame password

twoAdjacentDigitsAreSame :: String -> Bool
twoAdjacentDigitsAreSame password = any adjacentDigits (pairs password)

adjacentDigits :: (Char, Char) -> Bool
adjacentDigits = uncurry (==)

neverDecreases :: String -> Bool
neverDecreases password = all nonDescendingPair (pairs password)

nonDescendingPair :: (Char, Char) -> Bool
nonDescendingPair = uncurry (<=)

pairs :: String -> [(Char, Char)]
pairs password = zip password (drop 1 password)
