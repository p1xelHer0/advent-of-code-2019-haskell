-- stack --resolver lts-14.16 script

main = print . solve . map read . lines =<< readFile "input.txt"

fuelRequired :: Int -> Int
fuelRequired n = floor (fromIntegral n / 3) - 2

solve :: [Int] -> Int
solve = sum . map fuelRequired
