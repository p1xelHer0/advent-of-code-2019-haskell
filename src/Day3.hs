module Day3
  ( day03a
  , day03b
  )
where

import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Map.Strict               as M

-- Direction and Distance - Movement
data Movement = U Distance | R Distance | D Distance | L Distance deriving (Eq, Show)

-- Position
type Coordinate = (Int, Int)
type Distance = Int

-- We will store all the moves in a Map
-- Cooridnates being the Key
-- Value being the times said Coordinate has been visited
type Paths = M.Map Coordinate Int

-- Start Coordinate
centralPort :: Coordinate
centralPort = (0, 0)

-- This is the function which reads the input from ./input-data/3a.txt
input :: String -> [String]
input = lines

mkMovements :: String -> [Movement]
mkMovements s = catMaybes $ mkMovement <$> endBy "," s

mkMovement :: String -> Maybe Movement
mkMovement ('U' : n) = Just (U (read n :: Int))
mkMovement ('R' : n) = Just (R (read n :: Int))
mkMovement ('D' : n) = Just (D (read n :: Int))
mkMovement ('L' : n) = Just (L (read n :: Int))
mkMovement _         = Nothing

move :: [Movement] -> Paths
move ms = move' ms centralPort M.empty

move' :: [Movement] -> Coordinate -> Paths -> Paths
move' (U n : ds) c@(x, y) ps = move' ds (x, y + n) (move'' c (U n) ps)
move' (R n : ds) c@(x, y) ps = move' ds (x + n, y) (move'' c (R n) ps)
move' (D n : ds) c@(x, y) ps = move' ds (x, y - n) (move'' c (D n) ps)
move' (L n : ds) c@(x, y) ps = move' ds (x - n, y) (move'' c (L n) ps)
move' []         _        ps = ps

move'' :: Coordinate -> Movement -> Paths -> Paths
move'' coord d = visitf (mkLine coord d)

visitf :: [Coordinate] -> Paths -> Paths
visitf cs p = foldr visit p cs

visit :: Coordinate -> Paths -> Paths
visit c = M.insert c 1

-- Make a "line" of Coordinates
mkLine :: Coordinate -> Movement -> [Coordinate]
mkLine c (U n) = tail $ take (succ n) (iterate (\(x, y) -> (x, y + 1)) c)
mkLine c (R n) = tail $ take (succ n) (iterate (\(x, y) -> (x + 1, y)) c)
mkLine c (D n) = tail $ take (succ n) (iterate (\(x, y) -> (x, y - 1)) c)
mkLine c (L n) = tail $ take (succ n) (iterate (\(x, y) -> (x - 1, y)) c)

manhattan :: Coordinate -> Int
manhattan (x, y) = abs x + abs y

-- Every element with a value higher than 1 has wires crossing
solveManhattan :: Paths -> Int
solveManhattan = minimum . fmap manhattan . M.keys . M.filter (> 1)

joinPaths :: [Paths] -> Paths
joinPaths = foldr (M.unionWith (+)) M.empty

day03a :: String -> String
day03a = show . solveManhattan . joinPaths . fmap (move . mkMovements) . input

day03b :: String -> String
day03b = id
