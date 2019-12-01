-- stack --resolver lts-14.16 script

main = print . solve . map read . lines =<< readFile "input.txt"

fuelRequired :: Int -> Int
fuelRequired n = floor (fromIntegral n / 3) - 2

fuelRequired' :: Int -> Int -> Int
fuelRequired' result x = if nextFuel > 0
  then fuelRequired' (result + nextFuel) nextFuel
  else result
  where nextFuel = fuelRequired x

solve :: [Int] -> Int
solve = sum . map (fuelRequired' 0)
