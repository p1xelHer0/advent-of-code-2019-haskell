module Day2
  ( day02a
  , day02b
  )
where

import           Data.List.Split
import qualified Data.IntMap.Strict            as IM
import           Data.Maybe                    as Mb

type Position = Int
type Program = IM.IntMap Int
type Operation = Int -> Int -> Int

input :: String -> [Int]
input = fmap read . endBy ","

mappedInput :: String -> IM.IntMap Int
mappedInput x =
  IM.insert 2 2 $ IM.insert 1 12 $ IM.fromList . zip [0 ..] $ input x

data Opcode = Add | Mult | Halt | Error deriving Show

opcode :: Int -> Opcode
opcode x = case x of
  1  -> Add
  2  -> Mult
  99 -> Halt
  _  -> Error

run :: Program -> Maybe Program
run = run' 0 where
  run' instr prog = case opcode (valueFromPos instr prog) of
    Add   -> run' (instr + 4) $ operate instr (+) prog
    Mult  -> run' (instr + 4) $ operate instr (*) prog
    Halt  -> Just prog
    Error -> Nothing

valueFromPos :: Int -> IM.IntMap Int -> Int
valueFromPos pos im = Mb.fromMaybe 0 (IM.lookup pos im)

operate :: Int -> Operation -> Program -> Program
operate instr op prog = IM.insert pos (op x y) prog
  where (x, y, pos) = getInstructions instr prog

getInstructions :: Int -> Program -> (Int, Int, Position)
getInstructions instr prog = (x', y', pos)
 where
  x   = valueFromPos (instr + 1) prog
  y   = valueFromPos (instr + 2) prog
  x'  = valueFromPos x prog
  y'  = valueFromPos y prog
  pos = valueFromPos (instr + 3) prog


day02a :: String -> String
day02a s = show $ head $ fmap snd $ IM.toList $ fromJust $ run (mappedInput s)

day02b :: String -> String
day02b = show . id . mappedInput
