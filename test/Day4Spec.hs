import Test.HUnit
import Day4

day4Examples = TestCase (do
  assertEqual "6 digits" False (meetsCriteria "12345")
  assertEqual "never decrease" False (meetsCriteria "123454")
  assertEqual "two adjacent digits are same" False (meetsCriteria "123456")

  assertEqual "Example1" True (meetsCriteria "111111")
  assertEqual "Example2" False (meetsCriteria "223450")
  assertEqual "Example3" False (meetsCriteria "123789")

  assertEqual "Total" 1 (passwordsMeetingCriteria "123454-123457")
  )

day4 = TestCase (assertEqual "Day4" 1650 (passwordsMeetingCriteria "178416-676461"))

day4Part2Examples = TestCase (do
  assertEqual "Part 2 Example 1" True (meetsExtendedCriteria "112233")
  assertEqual "Part 2 Example 2" False (meetsExtendedCriteria "123444")
  assertEqual "Part 2 Example 3" True (meetsExtendedCriteria "111122")
  )
  
day4Part2 = TestCase (assertEqual "Day4Part2" 0 (passwordsMeetingExtendedCriteria "178416-676461"))

testList = TestList [day4Examples, day4, day4Part2Examples, day4Part2]

main :: IO ()
main = do
  runTestTT testList
  return ()
