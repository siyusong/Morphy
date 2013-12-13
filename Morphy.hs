{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
import Prelude 
--import Test.QuickCheck hiding (elements)
--import Control.Monad 
import qualified Data.Set as Set
import Data.List
 

type Cross = (Int, Int)
data Line = Line Cross Cross deriving (Eq, Ord)
data Board = Board (Set.Set Cross) (Set.Set Line) Moves
type Moves = [Line]



isLineValid :: Line -> Bool
isLineValid (Line (x, y) (x', y')) | x' == (x + 4) && y == y'       = True
                                   | x' == x && y' == (y + 4)       = True
                                   | x' == (x + 4) && y' == (y + 4) = True
                                   | otherwise                      = False


canMakeMove :: Line -> Board -> Bool
canMakeMove l (Board cset lset _) = not $ any (isOverlapped l) (Set.toList lset)

isOverlapped :: Line -> Line -> Bool
isOverlapped l1 l2 = length (Data.List.intersect (makeCrossList l1) (makeCrossList l2)) > 1


makeCrossList :: Line -> [Cross]
makeCrossList (Line (x, y) (x', y')) | x' == x + 4 && y == y'       = [(x,y), (x+1, y), (x+2, y), (x+3, y), (x+4, y)]
                                   | x' == x && y' == y + 4       = [(x,y), (x,y+1),(x, y+2), (x,y+3), (x,y+4)]
                                   | x' == x + 4 && y' == y + 4   = [(x,y),(x+1,y+1),(x+2,y+2),(x+3,y+3),(x+4, y+4)]
                                   | otherwise                     = []


makeMove :: Line -> Board -> Maybe Board
makeMove l (Board cset lset lst) 
        | not (isLineValid l) || canMakeMove l (Board cset lset lst) = Nothing
        | otherwise = Just (Board cset (Set.insert l lset) (l:lst))                                

undoMove :: Board -> Maybe Board
undoMove (Board _ _ []) = Nothing
undoMove (Board cset lset (l:ls)) = Just $ Board cset (Set.delete l lset) ls