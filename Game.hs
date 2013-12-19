module Game where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad 
import Test.QuickCheck hiding (Positive, Negative, replay)
import Test.HUnit 
import Text.ParserCombinators.Parsec hiding (Line)

type Point = (Int, Int)
data Line = VL -- "void" (fake) line, used to indicating initial marks
          | L { getPoint       :: Point
              , getOrientation :: Orientation
              , getDirection   :: Direction } 
          deriving (Ord)
data Orientation = H  -- horizontal
                 | V  -- vertical
                 | A  -- ascending
                 | D  -- descending
                 deriving (Eq, Ord, Enum, Bounded)
data Direction = Positive | Negative deriving (Eq, Ord, Enum, Bounded)
data Board = B { lineState :: [Line]
               , pointState :: Map Point [Line] }
               deriving (Show, Eq)

instance Show Line where
  show VL = "VL"
  show (L point orientation direction) =
    unwords [show point, show orientation, show direction]

instance Show Orientation where
  show H = "-"
  show V = "|"
  show A = "/"
  show D = "\\"

instance Show Direction where
  show Positive = "+"
  show Negative = "-"

instance Eq Line where
  (==) VL VL = True
  (==) VL _  = False
  (==) _  VL = False
  (==) l1 l2 = Set.fromList (linePoints l1) == Set.fromList (linePoints l2)

lineLength :: Int
lineLength = 5

orientationMultiplier :: Orientation -> (Int, Int)
orientationMultiplier H = (0, 1)
orientationMultiplier V = (1, 0)
orientationMultiplier A = (-1, 1)
orientationMultiplier D = (1, 1)

directionMultiplier :: Direction -> Int
directionMultiplier Positive = 1
directionMultiplier Negative = -1

game5 :: [Point]
game5 = [
                    (0,3),(0,4),(0,5),(0,6),
                    (1,3),            (1,6),
                    (2,3),            (2,6),
  (3,0),(3,1),(3,2),(3,3),            (3,6),(3,7),(3,8),(3,9),
  (4,0),                                                (4,9),
  (5,0),                                                (5,9),
  (6,0),(6,1),(6,2),(6,3),            (6,6),(6,7),(6,8),(6,9),
                    (7,3),            (7,6),
                    (8,3),            (8,6),
                    (9,3),(9,4),(9,5),(9,6)
  ]

makeBoard :: [Point] -> Board
makeBoard points = B [] (Map.fromList $ map (\p -> (p, [VL])) points)

played :: Board -> Point -> Bool
played board point = Map.member point (pointState board)

playable :: Board -> Bool
playable b = length (validMoves b) > 0

score :: Board -> Int
score board = length (Map.keys (pointState board)) - length game5

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
  aux i =
    let newPoint = (movePoint (i + 1) point orientation direction) in 
    case () of 
      _ | i >= lineLength - 1 -> i
        | played board newPoint ->
            if overlapped newPoint then i + 1
                                   else aux (i + 1)
        | otherwise -> i
  overlapped :: Point -> Bool
  overlapped point = 
    case Map.lookup point (pointState board) of 
      Just x -> orientation `elem` (map getOrientation (filter (/= VL) x))
      Nothing -> undefined

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

validMovePoints :: Board -> [Point]
validMovePoints board = filter (not.null.(validLines board)) (possibleMovePoints board)

test_validMoves :: Test
test_validMoves = "validMoves" ~: TestList [
  "initBoard" ~: Set.fromList (validMoves (makeBoard game5)) ~?= 
    Set.fromList [
      L (0,2) H Positive, L (-1,3) V Positive, L (-1,6) V Positive,
      L (0,3) H Positive, L (4,0) A Positive, L (0,5) D Positive,
      L (3,0) H Positive, L (3,5) H Positive, L (3,-1) H Positive,
      L (2,0) V Positive, L (0,3) V Positive, L (0,6) V Positive,
      L (2,9) V Positive, L (3,6) H Positive, L (6,-1) H Positive,
      L (6,6) H Positive, L (3,0) V Positive, L (5,0) D Positive,
      L (5,3) V Positive, L (6,0) H Positive, L (6,5) H Positive,
      L (5,6) V Positive, L (9,5) A Positive, L (3,9) V Positive,
      L (9,2) H Positive, L (9,3) H Positive, L (6,3) V Positive,
      L (6,6) V Positive
    ]
  ]

test_score :: Test
test_score = "score" ~: TestList [
  "initBoard" ~: score (makeBoard game5) ~?= 0
  ]

makeMove :: Board -> Line -> Maybe Board
makeMove board line = do
    guard $ line `elem` (validMoves board)
    Just $ insertPoints (insertLine board) (linePoints line)
  where
    insertLine b = b { lineState = line : lineState b }
    insertPoints b points = do 
      case points of
        (p:ps) -> insertPoints (b { pointState = newPointState b p }) ps
        _      -> b
      where
        newPointState b p = Map.insertWith (++) p [line] (pointState b)

undoMove :: Board -> Maybe Board
undoMove board = do
    case lineState board of
      (l:ls) -> return $ removePoint l (board { lineState = ls })
      _      -> Nothing
  where
    removePoint l b = b { pointState = cleanEmpty (foldr (removeLineFromMap l) (pointState b) (linePoints l)) }
    removeLineFromMap l p m = Map.adjust (filter (/= l)) p m      
    cleanEmpty = Map.filter (not.null)

tryMakeFirstMove :: Board -> Board
tryMakeFirstMove board =
  case validMoves board of
    (l:_) -> fromMaybe board (makeMove board l)
    _     -> board

tryMakeMove :: Board -> Line -> Board
tryMakeMove board l = fromMaybe board (makeMove board l)

tryUndoMove :: Board -> Board
tryUndoMove board =
  case lineState board of 
    (_:_) -> fromMaybe board (undoMove board)
    _     -> board

instance Arbitrary Orientation where
  arbitrary = elements [minBound..]

instance Arbitrary Direction where
  arbitrary = elements [minBound..]

instance Arbitrary Board where
  arbitrary = frequency [ (1, return $ makeBoard game5)
                        , (5, suchThat (tryMakeRandomMove arbitrary) playable) ] where
    tryMakeRandomMove board = do
      b <- board
      let moves = validMoves b
      case moves of
        (_:_) -> liftM2 tryMakeMove (return b) (elements moves)
        _     -> return b

  shrink b = case undoMove b of 
               Just b' -> [b']
               Nothing -> []  

validBoardLineOverlap :: Board -> Bool
validBoardLineOverlap b = and $ map (\p -> overlap p <= 1) pairs where
  pairs = [ (l1, l2) | l1 <- lineState b, l2 <- lineState b, l1 /= l2 ]
  overlap (l1, l2) = length $ linePoints l1 `intersect` linePoints l2

validBoard :: Board -> Bool
validBoard board = replayLines (makeBoard game5) (lineState board) where
  replayLines _ [] = True
  replayLines b ls = 
    case playableLine b ls of
      Just l  -> replayLines (tryMakeMove b l) (filter (/= l) ls)
      Nothing -> False
  playableLine b ls = find (\l -> l `elem` validMoves b) ls

prop_makeMove_score :: Board -> Property
prop_makeMove_score board = 
  playable board ==> score (tryMakeFirstMove board) == score board + 1

prop_makeMove_noOverlap :: Board -> Property
prop_makeMove_noOverlap board = 
  playable board ==> validBoardLineOverlap

prop_makeMove_validity :: Board -> Bool
prop_makeMove_validity = validBoard

prop_undoMove_rt :: Board -> Property
prop_undoMove_rt board =
  playable board ==> tryUndoMove (tryMakeFirstMove board) == board

prop_undoMove_validity :: Board -> Bool
prop_undoMove_validity = validBoard.tryUndoMove.tryMakeFirstMove

linePoints :: Line -> [Point]
linePoints l = aux 0 [] where
  aux i acc | i == lineLength = acc
            | otherwise       = aux (i + 1) (newPoint:acc)
    where newPoint = movePoint i (getPoint l) (getOrientation l) (getDirection l)

intP :: GenParser Char st Int
intP = do
  n <- string "-" <|> return []
  s <- many1 digit
  return $ (read (n ++ s) :: Int)

eolP :: GenParser Char st String
eolP = try (string "\r\n") <|> string "\n" <|> string "\r"

pointP :: GenParser Char st Point
pointP = do
  char '('
  x <- intP
  char ','
  y <- intP
  char ')'
  return (x, y)

orientationP :: GenParser Char st Orientation
orientationP = do
  c <- oneOf "-|/\\"
  return $ case c of 
          '-' -> H
          '|' -> V
          '/' -> A
          '\\' -> D

directionP :: GenParser Char st Direction
directionP = do
  c <- oneOf "+-"
  return $ case c of 
          '+' -> Positive
          '-' -> Negative

lineP :: GenParser Char st Line
lineP = do
  point <- pointP
  spaces
  orientation <- orientationP
  spaces
  direction <- directionP
  eolP
  return $ L point orientation direction

linesP :: GenParser Char st [Line]
linesP = do
  line <- many lineP
  eof
  return line

loadBoard :: String -> Either String Board
loadBoard input =
  case parse linesP "Data Format Error" input of
    Left err -> Left $ show err
    Right ls -> replay ls

replay :: [Line] -> Either String Board
replay previousLines = playNext (makeBoard game5) previousLines where
  playNext b [] = Right b
  playNext b (l:ls) =
    case makeMove b l of
      Just b' -> playNext b' ls
      Nothing -> Left $ "Invalid Move: " ++ show l

serializeBoard :: Board -> String
serializeBoard b = foldr appendLine "" (lineState b) where
  appendLine l s = s ++ show l ++ "\n"

prop_parseBoard_rt :: Board -> Bool
prop_parseBoard_rt b =
  case loadBoard (serializeBoard b) of
    Left _ -> False 
    Right b' -> b == b'
