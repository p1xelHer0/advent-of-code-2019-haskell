module Day1
  ( day01a
  , day01b
  )
where

input :: String -> [Int]
input = fmap read . lines

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

totalFuel :: [Int] -> Int
totalFuel = foldr ((+) . fuel) 0

day01a :: String -> String
day01a = show . totalFuel . input

fuel' :: Int -> Int
fuel' = sum . tail . takeWhile (> 0) . iterate fuel

totalFuel' :: [Int] -> Int
totalFuel' = foldr ((+) . fuel') 0

day01b :: String -> String
day01b = show . totalFuel' . input
