module BoardPrinter where

import GameLogic

import Data.Map (Map)
import qualified Data.Map as Map

drawPoint :: Board -> Map Point String
drawPoint board = Map.fromList $ map (\x -> (transform x, "O")) points
                where points = Map.keys $ pointState board

transform :: Point -> Point
transform (x, y) = (1 + 2 * x, 1 + 2 * y)

drawLinks :: Board -> Map Point String -> Map Point String
drawLinks board m = updatePointMap (links ls) m where
  ls = lineState board

updatePointMap :: [((Point, Point), String)] 
                    -> Map Point String -> Map Point String
updatePointMap [] m = m
updatePointMap ((p, orientation):ls) m = 
  updatePointMap ls (Map.insertWith (++) (findNewIndex p) orientation m)

updatePointMap' :: [(Point, String)] -> Map Point String -> Map Point String
updatePointMap' [] m = m
updatePointMap' ((p, orientation):ls) m =
  updatePointMap' ls (Map.insertWith (++) p orientation m)

links :: [Line] -> [((Point, Point), String)]
links ls = do
  l <- ls
  constructPair l

constructPair :: Line -> [((Point, Point), String)]
constructPair l = map orientationPair (zip (tail points) (init points)) where
  orientationPair x = (x, show (getOrientation l))
  points = linePoints l

findNewIndex :: (Point, Point) -> Point
findNewIndex ((x, y), (a, b)) | x == a    = (1 + 2 * x, 1 + y + b)
                              | y == b    = (1 + x + a, 1 + 2 * y)
                              | otherwise = (1 + x + a, 1 + y + b)

showBoard :: Board -> Map Point String
showBoard board = updatePointMap' (hintPoints board) (showBoardWOHints board)

showBoardWOHints :: Board -> Map Point String 
showBoardWOHints board = drawLinks board $ drawPoint board

generatePoints :: Board -> [[(Int, Int)]]
generatePoints b = [[(x, y) | y <- [y1..y2]] | x <- [x1..x2]]
                  where points = Map.keys $ pointState b
                        xs = map fst points
                        ys = map snd points
                        x1 = 1 + 2 * (minimum xs - 1)
                        x2 = 1 + 2 * (maximum xs + 1)
                        y1 = 1 + 2 * (minimum ys - 1)
                        y2 = 1 + 2 * (maximum ys + 1 )                

colorMapString :: String -> String
colorMapString str = concat (map (\c -> getColor c ++ [c]) str) where
        getColor c = if c `elem` ['a'..'z'] ++ ['A'..'N']
                        then "\ESC[90m"
                        else "\ESC[39m"

mapStringWithHints :: Board -> String
mapStringWithHints b = colorMapString $ m where
  m = unlines $ map (unwords.map (showPointMap (showBoard b))) ps
  ps = generatePoints b

mapStringWithOutHints :: Board -> String
mapStringWithOutHints b = unlines $ map (unwords.map (showPointMap m)) ps
          where m = showBoardWOHints b
                ps = generatePoints b

showPointMap :: Map Point String -> Point -> String
showPointMap m p = 
  case Map.findWithDefault " " p m of 
    [c] -> [c]
    s@(_:_) | '/' `elem` s && '\\' `elem` s -> "X"
            | otherwise -> s

hintPoints :: Board -> [(Point, String)]
hintPoints b = zip (map transform points) $ take len alphabets
            where points = validMovePoints b
                  len = length points
                  alphabets = map (:[]) (['a'..'z'] ++ ['A'..'Z'])
