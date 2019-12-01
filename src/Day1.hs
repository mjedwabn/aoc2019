module Day1
    ( fuel,
    totalFuel,
    actualFuel,
    totalActualFuel
    ) where

fuel :: Double -> Int
fuel mass = subtract 2 (floor ((/) mass 3))

totalFuel :: [Double] -> Int
totalFuel modules = sum (map fuel modules)

actualFuel :: Double -> Int
actualFuel mass = do
  let part = fuel mass
  if part <= 0
    then 0
    else sum [part, actualFuel (fromIntegral part)]

totalActualFuel :: [Double] -> Int
totalActualFuel modules = sum (map actualFuel modules)
