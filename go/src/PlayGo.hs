-- | Name: Vinh Duong
-- | Date: 
-- | Final Project
-- | This file contains the execution of haskeGo game 
module PlayGo (playGo) where

import Lib

playGo :: Int -> IO ()
playGo size = do
  -- | Intro 
  putStrLn ""
  putStrLn "Welcome to haskGo!"
  putStrLn $ "This is a " ++ show size ++ "x" ++ show size ++ " gameboard."
  putStrLn ""
  putStrLn "Instructions:"
  putStrLn "Please enter your move in form of 'x y', where x and y are integers"
  putStrLn ("Valid values for x & y: [0 - " ++ show (size - 1) ++ "]")
  putStrLn "Enter 'pass' to skip your turn"

  -- | Start game loop
  let state = initGame size
      loop state' = do
        let board = getBoard state'
            color = currentPlayer state'
        putStrLn $ "Current player: " ++ show color
        putStrLn $ printBoard board
        putStr "Your move: "
        line <- getLine
        print line
  loop state

