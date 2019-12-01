-- stack --resolver lts-14.16 script

main = print . solve . map read . lines =<< readFile "input.txt"

solve :: [Int] -> Int
solve = sum
