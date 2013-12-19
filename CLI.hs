module CLI where

import Game
import BoardPrinter

import Control.Concurrent (threadDelay)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Char (isAlpha, toLower)
import System.IO
import System.Console.ANSI
import System.Random (randomRIO)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  gameTurn $ makeBoard game5

gameTurn :: Board -> IO ()
gameTurn b = do
  clearScreen
  setCursorPosition 0 0 
  hideCursor
  putStr $ mapStringWithHints b
  showCursor
  putStrLn $ "Score: " ++ show (score b)
  putStrLn "Please indicate which point you are going to play?"
  putStrLn "(Type '.' for a random move or '?' for more options)"
  putStr ": " >> hFlush stdout
  x <- getChar
  case x of 
    '?' -> menu b
    '.' -> randomMove b
    c   -> gameFlow b c

invalidMove :: Board -> IO ()
invalidMove b = do
  putStrLn "Invalid Move"
  gameTurn b

randomMove :: Board -> IO ()
randomMove b = do 
  let points = validMovePoints b
  case points of
    [] -> invalidMove b
    _  -> do 
      p <- pickRandomElement points
      case validLines b p of 
          []    -> invalidMove b
          (l:_) -> gameTurn $ tryMakeMove b l

pickRandomElement :: [a] -> IO a
pickRandomElement l = do
  i <- randomRIO (0, (length l)-1)
  return $ l !! i

makeAlphabetMap :: [a] -> Map Char a
makeAlphabetMap ls = Map.fromList $ zip (take len alphabets) ls where
  len = length ls
  alphabets = ['a'..'z'] ++ ['A'..'Z']

gameFlow :: Board -> Char -> IO ()
gameFlow b x = do
  let points = validMovePoints b
  case Map.lookup x (makeAlphabetMap points) of 
    Just p -> do
      case validLines b p of 
        []  -> invalidMove b
        [l] -> gameTurn $ tryMakeMove b l 
        ls  -> lineSelection b ls
    Nothing -> invalidMove b

lineSelection :: Board -> [Line] -> IO ()
lineSelection b ls = do
  putStrLn "You have following line selections: "
  let m = makeAlphabetMap ls
  showLineChoices m
  x <- getChar
  case Map.lookup x m of
    Just l  -> gameTurn $ tryMakeMove b l
    Nothing -> invalidMove b

showLineChoices :: Map Char Line -> IO ()
showLineChoices m = do
  putStrLn "Please make a choice using the letter indicator."
  mapM_ showLineChoice (Map.toList m)
  where
    showLineChoice (alphabet, line) = 
      putStrLn $ [alphabet] ++ " -> " ++ show line

menu :: Board -> IO ()
menu b = do
  putStrLn ""
  mapM_ putStrUnderlineFirst [ "Continue, "
                             , "Save, "
                             , "Load, "
                             , "Undo, "
                             , "Replay, or "
                             , "Quit?"
                             ]
  putStr "\n: " >> hFlush stdout
  x <- getChar
  cursorUp 3
  clearFromCursorToScreenEnd
  case toLower x of 
    'c' -> gameTurn b
    'q' -> return ()
    'u' ->
      case undoMove b of
        Just  x -> putStrLn "Undo Succuess." >> gameTurn x
        Nothing -> putStrLn "Undo Failed"    >> menu b
    's' -> do
      putStrLn "Saving game to save.txt..."
      writeFile "save.txt" (serializeBoard b)
      menu b
    'l' -> do
      x <- readFile "save.txt"
      case loadBoard x of
        Left err -> putStrLn err             >> menu b
        Right s  -> putStrLn "Game Loaded."  >> gameTurn s
    'r' -> do
      let ls = reverse $ lineState b
      replayMoves (makeBoard game5) ls 
      menu b
    _ -> menu b

replayMoves :: Board -> [Line] -> IO ()
replayMoves b lns = do 
    clearScreen
    setCursorPosition 0 0
    hideCursor
    putStr $ mapStringWithOutHints b
    showCursor
    case lns of
      (l:ls) -> do
        threadDelay 300000
        replayMoves (tryMakeMove b l) ls
      _ -> return ()                    

putStrUnderlineFirst :: String -> IO ()
putStrUnderlineFirst [] = return ()
putStrUnderlineFirst (c:cs) = do
    setSGR [SetUnderlining SingleUnderline]
    putChar c
    setSGR []
    putStr cs
