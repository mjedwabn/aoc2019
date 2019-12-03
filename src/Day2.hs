module Day2
  ( runProgram,
  restoreAndRun,
  findInputs,
  puzzleAnswer
  ) where

runProgram :: [Int] -> [Int]
runProgram program = runOpcodeAtPosition program 0

runOpcodeAtPosition :: [Int] -> Int -> [Int]
runOpcodeAtPosition program position
  | program !! position == 99 = program
  | program !! position == 1 = runAdd program position
  | program !! position == 2 = runMul program position
  | otherwise = runOpcodeAtPosition program (position + 4)

runAdd :: [Int] -> Int -> [Int]
runAdd program position = do
  let a = program !! (program !! (position + 1))
  let b = program !! (program !! (position + 2))
  let newProgram = replace program (program !! (position + 3)) (a + b)
  runOpcodeAtPosition newProgram (position + 4)

runMul :: [Int] -> Int -> [Int]
runMul program position = do
  let a = program !! (program !! (position + 1))
  let b = program !! (program !! (position + 2))
  let newProgram = replace program (program !! (position + 3)) (a * b)
  runOpcodeAtPosition newProgram (position + 4)

replace :: [a] -> Int -> a -> [a]
replace xs i e =
  case splitAt i xs of
    (before, _:after) -> before ++ e : after
    _                 -> xs


restoreAndRun :: [Int] -> Int -> Int -> Int
restoreAndRun program i1 i2 = do
  let restoredProgram1 = replace program 1 i1
  let restoredProgram2 = replace restoredProgram1 2 i2
  let afterRun = runProgram restoredProgram2
  head afterRun

findInputs :: [Int] -> Int -> (Int, Int)
findInputs program output =
  snd
    (head
       (filter
          ((== 19690720) . fst)
          [ (restoreAndRun program i1 i2, (i1, i2))
          | i1 <- [0 .. 99]
          , i2 <- [0 .. 99]
          ]))

puzzleAnswer :: Int -> Int -> Int
puzzleAnswer noun verb = 100 * noun + verb
