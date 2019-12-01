module Lib
    ( day1FuelRequirement,
    day1TotalFuelRequirement,
    day1ActualFuelRequirement,
    day1TotalActualFuelRequirement
    ) where

day1FuelRequirement :: Double -> Int
day1FuelRequirement mass = subtract 2 (floor ((/) mass 3))

day1TotalFuelRequirement :: [Double] -> Int
day1TotalFuelRequirement modules = sum (map day1FuelRequirement modules)

day1ActualFuelRequirement :: Double -> Int
day1ActualFuelRequirement mass = do
  let part = day1FuelRequirement mass
  if part <= 0
    then 0
    else sum [part, day1ActualFuelRequirement (fromIntegral part)]

day1TotalActualFuelRequirement :: [Double] -> Int
day1TotalActualFuelRequirement modules = sum (map day1ActualFuelRequirement modules)
