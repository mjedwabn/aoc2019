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

testList = TestList [day4Examples, day4]

main :: IO ()
main = do
  runTestTT testList
  return ()
