module Day3
  ( closestCross
  , closestCrossDistance
  ) where

import           Data.List (sortBy)

closestCrossDistance :: String -> String -> Int
closestCrossDistance w1 w2 = do
  let intersection = closestCross w1 w2
  manhattanDistance (0, 0) intersection

closestCross :: String -> String -> (Int, Int)
closestCross w1 w2 = do
  let wire1 = parseWire w1
  let wire2 = parseWire w2
  let path1 = expandPath (0, 0) wire1
  let path2 = expandPath (0, 0) wire2
  let intersections = findIntersections path1 path2
  findClosestIntersection intersections

findIntersections :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
findIntersections path1 path2 =
  [p1 | p1 <- path1, p2 <- path2, p1 == p2, p1 /= (0, 0)]

findClosestIntersection :: [(Int, Int)] -> (Int, Int)
findClosestIntersection intersections = do
  let sorted = sortBy compareManhattanDistance intersections
  head sorted

compareManhattanDistance a b = do
  let fromCentral = manhattanDistance (0, 0)
  if fromCentral a < fromCentral b
    then LT
    else GT

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance p1 p2 = abs (fst p1 - fst p2) + abs (snd p1 - snd p2)

expandPath :: (Int, Int) -> [(String, Int)] -> [(Int, Int)]
expandPath start path = do
  let unified = start : map unifyPart path
  let zipped = zip unified [0 ..]
  let partialEval = evalPath unified
  let joined = map partialEval zipped
  foldl (\x y -> x ++ drop 1 (expandEdge (last x) y)) [(0, 0)] joined

expandEdge :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
expandEdge from to = do
  let fromX = fst from
  let fromY = snd from
  let toX = fst to
  let toY = snd to
  if fromX == toX
    then [(fromX, y) | y <- expandDimension fromY toY]
    else [(x, fromY) | x <- expandDimension fromX toX]

expandDimension :: Int -> Int -> [Int]
expandDimension from to =
  if to >= from
    then [from .. to]
    else [from,from - 1 .. to]

evalPath :: [(Int, Int)] -> ((Int, Int), Int) -> (Int, Int)
evalPath path part =
  ( sum (map fst (take (snd part + 1) path))
  , sum (map snd (take (snd part + 1) path)))

unifyPart :: (String, Int) -> (Int, Int)
unifyPart part
  | fst part == "R" = (snd part, 0)
  | fst part == "L" = (-(snd part), 0)
  | fst part == "U" = (0, snd part)
  | otherwise = (0, -(snd part))

parseWire :: String -> [(String, Int)]
parseWire wire = do
  let parts = wordsWhen (== ',') wire
  map parsePathPart parts

parsePathPart :: String -> (String, Int)
parsePathPart part = (take 1 part, read (substring 1 (length part) part) :: Int)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)
