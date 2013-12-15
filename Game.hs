module Game where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Control.Monad 
import Test.HUnit 

type Point = (Int, Int)
data Line = L { getPoint       :: Point
              , getOrientation :: Orientation
              , getDirection   :: Direction } 
              deriving (Show, Ord)
data Orientation = H  -- horizontal
                 | V  -- vertical
                 | A  -- ascending
                 | D  -- descending
                 deriving (Show, Eq, Ord, Enum, Bounded)
data Direction = Positive | Negative deriving (Show, Eq, Ord, Enum, Bounded)
data Board = B { lineState :: [Line]
               , pointState :: Map Point [Line] }
               deriving (Show, Eq)

instance Eq Line where
  (==) l1 l2 = Set.fromList (linePoints l1) == Set.fromList (linePoints l2)

lineLength :: Int
lineLength = 5

orientationMultiplier :: Orientation -> (Int, Int)
orientationMultiplier H = (1, 0)
orientationMultiplier V = (0, 1)
orientationMultiplier A = (1, -1)
orientationMultiplier D = (1, 1)

directionMultiplier :: Direction -> Int
directionMultiplier Positive = 1
directionMultiplier Negative = -1

game5 :: [Point]
game5 = [
                    (3,0),(4,0),(5,0),(6,0),
                    (3,1),            (6,1),
                    (3,2),            (6,2),
  (0,3),(1,3),(2,3),(3,3),            (6,3),(7,3),(8,3),(9,3),
  (0,4),                                                (9,4),
  (0,5),                                                (9,5),
  (0,6),(1,6),(2,6),(3,6),            (6,6),(7,6),(8,6),(9,6),
                    (3,7),            (6,7),
                    (3,8),            (6,8),
                    (3,9),(4,9),(5,9),(6,9)
  ]

makeBoard :: [Point] -> Board
makeBoard points = B [] (Map.fromList $ map (\p -> (p, [])) points)

played :: Board -> Point -> Bool
played board point = Map.member point (pointState board)

validLines :: Board -> Point -> [Line]
validLines board p = do
    orientation <- [minBound..]
    let behindPoints = adjacentPoints board p orientation Negative
    let aheadPoints = adjacentPoints board p orientation Positive
    offset <- [lineLength - 1 - aheadPoints..behindPoints]
    let p' = movePoint offset p orientation Negative
    return $ L p' orientation Positive

adjacentPoints :: Board -> Point -> Orientation -> Direction -> Int
adjacentPoints board point orientation direction = aux 0 where
  aux i | i >= lineLength - 1 = i
        | played board (movePoint (i + 1) point orientation direction) = aux (i + 1)
        | otherwise = i

movePoint :: Int -> Point -> Orientation -> Direction -> Point
movePoint n (x, y) orientation direction = (x + dx * n, y + dy * n) where
  dirMul = directionMultiplier direction
  (oriMulX, oriMulY) = orientationMultiplier orientation
  (dx, dy) = (dirMul * oriMulX, dirMul * oriMulY)

possibleMovePoints :: Board -> [Point]
possibleMovePoints board = do
    x <- [x1..x2]
    y <- [y1..y2]
    guard $ not $ Map.member (x, y) (pointState board)
    return (x, y)
  where
    points = Map.keys $ pointState board
    xs = map fst points
    ys = map snd points
    x1 = minimum xs - 1
    x2 = maximum xs + 1
    y1 = minimum ys - 1
    y2 = maximum ys + 1

validMoves :: Board -> [Line]
validMoves board = concat [ validLines board point | point <- possibleMovePoints board ]

test_validMoves :: Test
test_validMoves = "validMoves" ~: TestList [
  "initBoard" ~: Set.fromList (validMoves (makeBoard game5)) ~?= 
    Set.fromList [
      L (2,0) H Positive, L (3,-1) V Positive, L (6,-1) V Positive,
      L (3,0) H Positive, L (0,4) A  Positive, L (5,0) D Positive,
      L (0,3) H Positive, L (5,3) H Positive, L (-1,3) H Positive,
      L (0,2) V Positive, L (3,0) V Positive, L (6,0) V Positive,
      L (9,2) V Positive, L (6,3) H Positive, L (-1,6) H Positive,
      L (6,6) H Positive, L (0,3) V Positive, L (0,5) D Positive,
      L (3,5) V Positive, L (0,6) H Positive, L (5,6) H Positive,
      L (6,5) V Positive, L (5,9) A Positive, L (9,3) V Positive,
      L (2,9) H Positive, L (3,9) H Positive, L (3,6) V Positive,
      L (6,6) V Positive
    ]
  ]

makeMove :: Board -> Line -> Maybe Board
makeMove board line = do
    guard $ line `elem` (validMoves board)
    insertPoint (insertLine board)
  where
    insertLine b = b { lineState = line : lineState b }
    insertPoint b = do 
      let point = filter (not.played b) (linePoints line)
      case point of
        (p:ps) -> return b { pointState = newPointState p }
        _      -> Nothing
      where
        newPointState p = Map.insertWith (++) p [line] (pointState board)

linePoints :: Line -> [Point]
linePoints l = aux 0 [] where
  aux i acc | i == lineLength = acc
            | otherwise       = aux (i + 1) (newPoint:acc)
    where newPoint = movePoint i (getPoint l) (getOrientation l) (getDirection l)