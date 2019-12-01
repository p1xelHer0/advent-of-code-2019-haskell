module Day1
  ( day01a
  , day01b
  )
where

fuelRequired :: Int -> Int
fuelRequired n = floor (fromIntegral n / 3) - 2

fuelSum :: [Int] -> Int
fuelSum = sum . fmap fuelRequired

day01a :: String -> String
day01a = show . fuelSum . fmap read . lines

fuelRequired' :: Int -> Int -> Int
fuelRequired' result x = if nextFuel > 0
  then fuelRequired' (result + nextFuel) nextFuel
  else result
  where nextFuel = fuelRequired x

fuelSum' :: [Int] -> Int
fuelSum' = sum . map (fuelRequired' 0)

day01b :: String -> String
day01b = show . fuelSum' . fmap read . lines
