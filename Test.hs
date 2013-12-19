{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-}

module Test where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad 
import Test.HUnit
import Test.QuickCheck hiding (Positive, replay)
import GameLogic
import BoardPrinter

main :: IO ()
main = do
  _ <- runTestTT unitTests
  quickCheck boardProperties

-- HUnit tests
unitTests :: Test
unitTests = TestList [ test_drawPoint
                     , test_findNewIndex
                     , test_validMoves
                     , test_score
                     ]

test_drawPoint :: Test
test_drawPoint = "draw1" ~: TestList [
    "draw points" ~: drawPoint (makeBoard game5) ~?=
    Map.fromList [((1,7),"O"),((1,9),"O"),((1,11),"O"),((1,13),"O"),
    ((3,7),"O"),((3,13),"O"),((5,7),"O"),((5,13),"O"),((7,1),"O"),((7,3),"O"),
    ((7,5),"O"),((7,7),"O"),((7,13),"O"),((7,15),"O"),((7,17),"O"),((7,19),"O"),
    ((9,1),"O"),((9,19),"O"),((11,1),"O"),((11,19),"O"),((13,1),"O"),
    ((13,3),"O"),((13,5),"O"),((13,7),"O"),((13,13),"O"),((13,15),"O"),
    ((13,17),"O"),((13,19),"O"),((15,7),"O"),((15,13),"O"),((17,7),"O"),
    ((17,13),"O"),((19,7),"O"),((19,9),"O"),((19,11),"O"),((19,13),"O")]
    ]

test_findNewIndex :: Test
test_findNewIndex = "test" ~: TestList [
    "test H" ~: findNewIndex ((0,0),(0,1)) ~?= (1,2),
    "test V" ~: findNewIndex ((0,0), (1,0)) ~=? (2,1),
    "test A" ~: findNewIndex ((0,0), (1,1)) ~=? (2,2)
    ]

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
  "init board" ~: score (makeBoard game5) ~?= 0,
  "one move" ~: score (tryMakeFirstMove (makeBoard game5)) ~?= 1,
  "undo" ~: score (tryUndoMove (tryMakeFirstMove (makeBoard game5))) ~?= 0
  ]

-- QuickCheck Tests
boardProperties :: Property
boardProperties = 
  printTestCase "score" prop_makeMove_score .&&.
  printTestCase "noOverlap" prop_makeMove_noOverlap .&&.
  printTestCase "makeMove validity" prop_makeMove_validity .&&.
  printTestCase "undoMove rt" prop_undoMove_rt .&&.
  printTestCase "undoMove validity" prop_undoMove_validity .&&.
  printTestCase "parsing and printing" prop_parseBoard_rt 

instance Arbitrary Orientation where
  arbitrary = elements [minBound..]

instance Arbitrary Direction where
  arbitrary = elements [minBound..]

instance Arbitrary Board where
  arbitrary = 
    frequency [ (1, return $ makeBoard game5)
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

prop_parseBoard_rt :: Board -> Bool
prop_parseBoard_rt b =
  case loadBoard (serializeBoard b) of
    Left _ -> False 
    Right b' -> b == b'
