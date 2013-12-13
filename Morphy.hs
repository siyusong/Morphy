{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
import Prelude 
--import Test.QuickCheck hiding (elements)
--import Control.Monad 
import qualified Data.Set as Set
import Data.List
 

type Cross = (Int, Int)
data Line = Line Cross Cross deriving (Eq, Ord)
data Board = Board (Set.Set Cross) (Set.Set Line) Moves [Cross]
type Moves = [Line]



isLineValid :: Line -> Bool
isLineValid (Line (x, y) (x', y')) | x' == (x + 4) && y == y'       = True
                                   | x' == x && y' == (y + 4)       = True
                                   | x' == (x + 4) && y' == (y + 4) = True
                                   | otherwise                      = False


canMakeMove :: Line -> Board -> Bool
canMakeMove l (Board _ lset _ _) = not $ any (isOverlapped l) (Set.toList lset)

isOverlapped :: Line -> Line -> Bool
isOverlapped l1 l2 = length (Data.List.intersect (makeCrossList l1) (makeCrossList l2)) > 1


makeCrossList :: Line -> [Cross]
makeCrossList (Line (x, y) (x', y')) | x' == x + 4 && y == y'       = [(x,y), (x+1, y), (x+2, y), (x+3, y), (x+4, y)]
                                   | x' == x && y' == y + 4       = [(x,y), (x,y+1),(x, y+2), (x,y+3), (x,y+4)]
                                   | x' == x + 4 && y' == y + 4   = [(x,y),(x+1,y+1),(x+2,y+2),(x+3,y+3),(x+4, y+4)]
                                   | otherwise                     = []

newValidCross :: Line -> Board -> [Cross]
newValidCross l (Board cset _ _ _) = filter (`Set.notMember` cset) lst
                                    where lst = makeCrossList l
                                          


makeMove :: Line -> Board -> Maybe Board
makeMove l (Board cset lset lst clist) = if not (isLineValid l) || (len > 1) || canMakeMove l (Board cset lset lst clist) then Nothing
                                                  else Just $ makeNewBoard l (Board cset lset lst clist) cValid

                                        where cValid = newValidCross l (Board cset lset lst clist)
                                              len = length cValid

makeNewBoard :: Line -> Board -> [Cross] -> Board
makeNewBoard l (Board cset lset lst clist) [] = Board cset (Set.insert l lset) (l:lst) clist
makeNewBoard l (Board cset lset lst clist) (c:cst) = Board (Set.insert c cset) (Set.insert l lset) (l:lst) (c:clist)   


undoMove :: Board -> Maybe Board
undoMove (Board _ _ [] _) = Nothing
undoMove (Board cset lset (l:ls) []) = Just $ Board cset (Set.delete l lset) ls []
undoMove (Board cset lset (l:ls) (c:clist)) = Just $ Board cset (Set.delete l lset) ls clist
