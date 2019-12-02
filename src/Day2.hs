module Day2
  ( day02a
  , day02b
  )
where

import           Data.List.Split
import           Data.List
import qualified Data.IntMap.Strict            as IM
import           Data.Maybe                    as Mb

type Position = Int
type Program = IM.IntMap Int
type Operation = Int -> Int -> Int

type Noun = Int
type Verb = Int

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

run1 :: Program -> Maybe Program
run1 = run1' 0 where
  run1' instr prog = case opcode (valueFromPos instr prog) of
    Add   -> run1' (instr + 4) $ operate instr (+) prog
    Mult  -> run1' (instr + 4) $ operate instr (*) prog
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

extractResult :: Maybe Program -> Int
extractResult prog = head $ fmap snd $ IM.toList $ fromJust prog

day02a :: String -> String
day02a s = show $ extractResult $ run1 (mappedInput s)

outputToProduce = 19690720

nouns :: [Noun]
nouns = [0 .. 99]

verbs :: [Verb]
verbs = [0 .. 99]

run2 :: Program -> [((Noun, Verb), Int)]
run2 prog = catMaybes $ do
  noun <- nouns
  verb <- verbs
  let test = IM.insert 2 verb $ IM.insert 1 noun prog
  pure $ sequenceA ((noun, verb), Just (extractResult $ run1 test))

findIt :: [((Noun, Verb), Int)] -> (Noun, Verb)
findIt = fst . head . filter (\(_, result) -> result == outputToProduce)

doIt s = findIt $ run2 (mappedInput s)

day02b :: String -> String
day02b s = show $ 100 * fst nounAndVerb + snd nounAndVerb
  where nounAndVerb = doIt s
