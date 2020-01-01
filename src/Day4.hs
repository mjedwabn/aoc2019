module Day4
  ( meetsCriteria
  , passwordsMeetingCriteria
  , meetsExtendedCriteria
  , passwordsMeetingExtendedCriteria
  ) where

passwordsMeetingExtendedCriteria :: String -> Int
passwordsMeetingExtendedCriteria range =
  length [password | password <- passwordsInRange range, meetsExtendedCriteria password]

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
passwordsMeetingCriteria range =
  length [password | password <- passwordsInRange range, meetsCriteria password]

passwordsInRange :: String -> [String]
passwordsInRange range = do
  let (from, to) = parseRange range
  [show password | password <- [from .. to]]

parseRange :: String -> (Int, Int)
parseRange range = do
  let parts = wordsWhen (== '-') range
  (read (head parts) :: Int, read (parts !! 1) :: Int)

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
