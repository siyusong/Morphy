{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module CLI where

import GameLogic
import BoardPrinter

import Control.Concurrent (threadDelay)
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)
import System.IO
import System.Console.ANSI
import System.Random (randomRIO)

type Game a = StateT Board IO a

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout LineBuffering
  _ <- runStateT gameTurn (makeBoard game5)
  return ()

io :: IO a -> Game a
io = lift

gameTurn :: Game ()
gameTurn = do
  printBoard
  b <- get
  io $ putStrLn $ "Score: " ++ show (score b)
  io $ putStrLn "Please indicate which point you are going to play?"
  io $ putStrLn "(Type '.' for a random move or '?' for more options)"
  io $ putStr ": " >> hFlush stdout
  x <- io getChar
  case x of 
    '?' -> menu
    '.' -> randomMove
    c   -> gameFlow c

printBoard :: Game ()
printBoard = do
  b <- get
  io $ clearScreen
  io $ setCursorPosition 0 0 
  io $ hideCursor
  io $ putStr $ mapStringWithHints b
  io $ showCursor

invalidMove :: Game ()
invalidMove = do
  io $ putStrLn "Invalid Move"
  gameTurn

randomMove :: Game ()
randomMove = do 
  b <- get
  let points = validMovePoints b
  case points of
    [] -> invalidMove
    _  -> do 
      p <- io $ pickRandomElement points
      case validLines b p of 
          []    -> invalidMove
          (l:_) -> do
            put $ tryMakeMove b l
            gameTurn

pickRandomElement :: [a] -> IO a
pickRandomElement l = do
  i <- randomRIO (0, (length l)-1)
  return $ l !! i

makeAlphabetMap :: [a] -> Map Char a
makeAlphabetMap ls = Map.fromList $ zip (take len alphabets) ls where
  len = length ls
  alphabets = ['a'..'z'] ++ ['A'..'Z']

gameFlow :: Char -> Game ()
gameFlow x = do
  b <- get
  let points = validMovePoints b
  case Map.lookup x (makeAlphabetMap points) of 
    Just p -> do
      case validLines b p of 
        []  -> invalidMove
        [l] -> do
          put $ tryMakeMove b l
          gameTurn
        ls  -> lineSelection ls
    Nothing -> invalidMove

lineSelection :: [Line] -> Game ()
lineSelection ls = do
  io $ putStrLn "You have following line selections: "
  let m = makeAlphabetMap ls
  io $ showLineChoices m

  b <- get
  x <- io getChar
  case Map.lookup x m of
    Just l  -> do 
      put $ tryMakeMove b l
      gameTurn
    Nothing -> invalidMove
  where
  showLineChoices m = do
    putStrLn "Please make a choice using the letter indicator."
    mapM_ showLineChoice (Map.toList m)
  showLineChoice (alphabet, line) = 
    putStrLn $ [alphabet] ++ " -> " ++ show line

menu :: Game ()
menu = do
  io $ putStrLn ""
  io $ mapM_ putStrUnderlineFirst [ "Continue, "
                             , "Save, "
                             , "Load, "
                             , "Undo, "
                             , "Replay, "
                             , "New game or "
                             , "Quit?"
                             ]
  io $ putStr "\n: " >> hFlush stdout
  c <- io getChar
  b <- get
  case toLower c of 
    'c' -> gameTurn
    'q' -> return ()
    'u' ->
      case undoMove b of
        Just b' -> do 
          io $ putStrLn "Undo Succuess." 
          put b'
          gameTurn
        Nothing -> do
          clearAbove 3
          io $ putStrLn "Cannot Undo"
          menu
    's' -> do
      clearAbove 3
      io $ putStrLn "Saving game to save.txt..."
      io $ writeFile "save.txt" (serializeBoard b)
      menu
    'l' -> do
      f <- io $ readFile "save.txt"
      case loadBoard f of
        Left err -> do
          io $ putStrLn err
          menu
        Right s -> do
          io $ putStrLn "Game Loaded."
          put s
          gameTurn
    'r' -> do
      let ls = reverse $ lineState b
      put $ makeBoard game5
      replayMoves ls 
      menu
    'n' -> do
      put $ makeBoard game5
      gameTurn
    _ -> clearAbove 2 >> menu
    where 
      clearAbove i = do
        io $ cursorUp i
        io $ setCursorColumn 0
        io $ clearLine
        io $ clearFromCursorToScreenEnd

replayMoves :: [Line] -> Game ()
replayMoves lns = do 
    io $ clearScreen
    io $ setCursorPosition 0 0
    io $ hideCursor
    b <- get
    io $ putStr $ mapStringWithOutHints b
    io $ showCursor
    case lns of
      (l:ls) -> do
        io $ threadDelay 300000
        put $ tryMakeMove b l
        replayMoves ls
      _ -> return ()                    

putStrUnderlineFirst :: String -> IO ()
putStrUnderlineFirst [] = return ()
putStrUnderlineFirst (c:cs) = do
    setSGR [SetUnderlining SingleUnderline]
    putChar c
    setSGR []
    putStr cs
