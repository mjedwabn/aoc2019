import Test.HUnit
import Day1

day1Examples = TestCase (do
  assertEqual "Example1" 2 (fuel 12)
  assertEqual "Example2" 2 (fuel 14)
  assertEqual "Example3" 654 (fuel 1969)
  assertEqual "Example4" 33583 (fuel 100756)
  )

day1 = TestCase (do
  content <- readFile "test/day1.txt"
  let modules = lines content
  assertEqual "Day 1" 3255932 (totalFuel (map read modules))
  )

day1Part2Examples = TestCase (do
  assertEqual "Example 1" 2 (actualFuel 14)
  assertEqual "Example 2" 966 (actualFuel 1969)
  assertEqual "Example 3" 50346 (actualFuel 100756)
  )

day1Part2 = TestCase (do
  content <- readFile "test/day1.txt"
  let modules = lines content
  assertEqual "Day 1" 4881041 (totalActualFuel (map read modules))
  )

testList = TestList [day1Examples, day1, day1Part2Examples, day1Part2]

main :: IO ()
main = do
  runTestTT testList
  return ()
