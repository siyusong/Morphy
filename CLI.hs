import Game
import Data.Map (Map)
import qualified Data.Map as Map
import Test.HUnit 

drawPoint :: Board -> Map Point String
drawPoint board = Map.fromList $ map (\x -> (transform x, "O")) points
                where points = Map.keys $ pointState board

transform :: Point -> Point
transform (x,y) = (1 + 2 * x, 1+ 2 * y)

testDrawPoint :: Test
testDrawPoint = "draw1" ~: TestList [
    "draw points" ~: drawPoint (makeBoard game5) ~?=
    Map.fromList [((1,7),"O"),((1,9),"O"),((1,11),"O"),((1,13),"O"),((3,7),"O"),
    ((3,13),"O"),((5,7),"O"),((5,13),"O"),((7,1),"O"),((7,3),"O"),((7,5),"O"),
    ((7,7),"O"),((7,13),"O"),((7,15),"O"),((7,17),"O"),((7,19),"O"),((9,1),"O"),
    ((9,19),"O"),((11,1),"O"),((11,19),"O"),((13,1),"O"),((13,3),"O"),((13,5),"O"),
    ((13,7),"O"),((13,13),"O"),((13,15),"O"),((13,17),"O"),((13,19),"O"),((15,7),"O"),
    ((15,13),"O"),((17,7),"O"),((17,13),"O"),((19,7),"O"),((19,9),"O"),
    ((19,11),"O"),((19,13),"O")]
    ]

drawLinks :: Board -> Map Point String -> Map Point String
drawLinks board m = updatePointMap lk m
                    where ls = lineState board
                          lk = links ls

updatePointMap :: [((Point, Point), String)] -> Map Point String -> Map Point String
updatePointMap [] m = m
updatePointMap (l:ls) m = updatePointMap ls (Map.insertWith (++) (findNewIndex $ fst l) (snd l) m)

links :: [Line] -> [((Point, Point), String)]
links ls = do 
                l <- ls
                constructPair l

constructPair :: Line -> [((Point, Point), String)]
constructPair l = map (\x -> (x, getLink (getOrientation l))) $ zip (tail points) points2
        where points = linePoints l
              points2 = init points

getLink :: Orientation -> String
getLink H = "-"
getLink V = "|"
getLink A = "/"
getLink D = "\\"

findNewIndex :: (Point, Point) -> Point
findNewIndex ((x, y), (a, b)) | x == a    = (1 + 2 * x, 1 + y + b)
                              | y == b    = (1 + x + a, 1 + 2 * y)
                              | otherwise = (1 + x + a, 1 + y + b)

testFindNewIndex :: Test
testFindNewIndex = "test" ~: TestList [
        "test H" ~: findNewIndex ((0,0),(0,1)) ~?= (1,2),
        "test V" ~: findNewIndex ((0,0), (1,0)) ~=? (2,1),
        "test A" ~: findNewIndex ((0,0), (1,1)) ~=? (2,2)
        ]

showBoard :: Board -> Map Point String
showBoard board = drawLinks board $ drawPoint board 



