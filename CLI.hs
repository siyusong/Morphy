import Game
import Data.Map (Map)
import qualified Data.Map as Map
import Test.HUnit 
import qualified Data.List as List

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
showBoard board = updatePointMap2 (hintPoints board) m
                where m = drawLinks board $ drawPoint board

printMap :: Board -> String
printMap b = unlines $ map (unwords . map (showM m)) indices
          where points = Map.keys $ pointState b
                xs = map fst points
                ys = map snd points
                x1 = 1 + 2 * (minimum xs - 1)
                x2 = 1 + 2 * (maximum xs + 1)
                y1 = 1 + 2 * (minimum ys - 1)
                y2 = 1 + 2 * (maximum ys + 1 )
                indices = [[(x, y) | y <- [y1..y2]] | x <- [x1..x2]]
                m = showBoard b 

showM :: Map Point String -> Point -> String
showM m p = aux len s
            where s = Map.findWithDefault " " p m
                  len = length s
                  aux 1 s = s
                  aux _ s | (List.isInfixOf "/") s && (List.isInfixOf "\\" s) = "X"
                          | otherwise = s

hintPoints :: Board -> [(Point, String)]
hintPoints b = zip (map transform points) $ take len $ map (:[]) (['a'..'z'] ++ ['A'..'Z'])
            where points = validMovePoints b
                  len = length points

updatePointMap2 ::[(Point, String)] -> Map Point String -> Map Point String
updatePointMap2 [] m = m
updatePointMap2 (l:ls) m = updatePointMap2 ls (Map.insertWith (++) (fst l) (snd l) m)

gameTurn :: Board -> IO()
gameTurn b = do
        putStr $ printMap b
        let s = score b
        putStr "The Current Score is "
        putStr $ show s
        putStr "\n"
        putStr "All the hints are shown\n"
        putStr "Please indicate which point you are going to play? or Type '?' for MENU\n"
        x <- getLine
        --putStr x
        --putStr "\n"
        case x of 
            "?" -> menu b
            r   -> gameflow b r


invalidMove :: Board -> IO()
invalidMove b = do
                 putStr "Invalid Move"
                 gameTurn b

makeHashMap :: [a] ->[(String, a)]
makeHashMap [] = []
makeHashMap ls = zip (take len $ map (:[]) (['a'..'z'] ++ ['A'..'Z'])) ls
                where len = length ls


gameflow :: Board -> String -> IO()
gameflow b x = do
                let points = validMovePoints b
                let m = Map.fromList $ makeHashMap points
                let p = Map.findWithDefault (100,100) x m
                --putStrLn $ show p
                let ls = validLines b p
                --putStrLn $ show ls
                case (length ls) of 
                    0 -> invalidMove b
                    1 -> gameTurn $ tryMakeMove b (head ls)
                    l -> lineSelection ls b

lineSelection :: [Line] -> Board -> IO()
lineSelection ls b = do
                        let lines = makeHashMap ls
                        let m = Map.fromList lines
                        putStrLn "You have following line selections: "
                        showLineChoices lines
                        x <- getLine
                        putStr x
                        gameTurn $ tryMakeMove b $ Map.findWithDefault (L (100,100) H Positive) x m


showLineChoices :: [(String, Line)] -> IO()
showLineChoices [] = do
                        putStrLn "Please make a choice using the letter indicator."
showLineChoices (l:ls) = do
                            putStr (fst l)
                            putStr "-> "
                            putStr $ show $ getPoint (snd l)

                            putStrLn $ drawDirection $ getOrientation (snd l)
                            showLineChoices ls

main :: IO()
main = gameTurn $ makeBoard game5


drawDirection :: Orientation -> String
drawDirection H = " -"
drawDirection V = " |"
drawDirection A = " /"
drawDirection D = " \\"


menu :: Board -> IO()
menu b = do
            putStrLn "Quit, Save, Load, Undo, Continue or Replay?"
            x <- getLine
            putStrLn ""
            case x of 
                "C" -> gameTurn b
                "Q" -> return ()
                "U" -> menuUndo b
                --"S" -> 

menuUndo :: Board -> IO()
menuUndo b = do
                case undoMove b of
                    Just x -> do
                                putStrLn "Undo Succuess."
                                gameTurn x
                    Nothing -> do
                                putStrLn "Undo Failed"
                                menu b  






