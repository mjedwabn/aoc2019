import Test.HUnit
import Lib

day1Examples = TestCase (do
  assertEqual "Example1" 2 (day1FuelRequirement 12)
  assertEqual "Example2" 2 (day1FuelRequirement 14)
  assertEqual "Example3" 654 (day1FuelRequirement 1969)
  assertEqual "Example4" 33583 (day1FuelRequirement 100756)
  )

day1 = TestCase (do
  content <- readFile "test/day1.txt"
  let modules = lines content
  assertEqual "Day 1" 3255932 (day1TotalFuelRequirement (map read modules))
  )

day1Part2Examples = TestCase (do
  assertEqual "Example 1" 2 (day1ActualFuelRequirement 14)
  assertEqual "Example 2" 966 (day1ActualFuelRequirement 1969)
  assertEqual "Example 3" 50346 (day1ActualFuelRequirement 100756)
  )

day1Part2 = TestCase (do
  content <- readFile "test/day1.txt"
  let modules = lines content
  assertEqual "Day 1" 0 (day1TotalActualFuelRequirement (map read modules))
  )

testList = TestList [day1Examples, day1, day1Part2Examples, day1Part2]

main :: IO ()
main = do
  runTestTT testList
  return ()
