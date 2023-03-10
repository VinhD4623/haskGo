-- | Name: Vinh Duong
-- | Date: 
-- | Final Project

module Lib (
    Board, Pos, Stone,
    Color(Black,White),
    Move(Pass, Place),
    GameState,  
    switchColor,
    getStoneColor,
    getStone,
    showStone,
    emptyGameBoard,
    printBoard,
    isEmpty,
    getStoneFromMove,
    getPos,
    currentPlayer,
    getEmptyPos,
    initGame,
    getBoard
)where


import qualified Data.Set as Set
import Data.String

type Board = [[Stone]]
type Pos = (Int, Int)
type Stone = Maybe Color

-- | Color can be compared (Eq), ordered (Ord), and converted to a string (Show).
data Color = Black | White | Empty
             deriving (Show, Eq, Ord)

-- | Move can be converted to a string (Show).
data Move = Pass | Place Color Pos
            deriving Show

-- | Switching color
switchColor :: Color -> Color
switchColor Black = White
switchColor White = Black

-- | Get color from stone
getStoneColor :: Stone -> Maybe Color
getStoneColor Nothing = Nothing
getStoneColor (Just Black) = Just Black
getStoneColor (Just White) = Just White

-- | Get string representation for stone to display
showStone :: Stone -> String
showStone Nothing = "-"
showStone (Just Black) = "X"
showStone (Just White) = "O"

-- | Check if given position is empty
isEmpty :: Board -> Pos -> Bool
isEmpty board pos = getStone board pos == Nothing

-- | Create an empty game board
-- | Game board is represented by a two-dimensional list of Nothing
emptyGameBoard :: Int -> Board
emptyGameBoard n = replicate n (replicate n Nothing)


-- | Get stone at given position
-- | Validates the position (x, y) inputs 
-- | Upon valid Pos returns a stone value at (x, y)
getStone :: Board -> Pos -> Stone
getStone board (x, y)
  | x >= 0 && x < length board && y >= 0 && y < length (head board) =
    board !! x !! y
  | otherwise = Nothing


-- | Print Go board
-- | Returns a string representation of the board
printBoard :: Board -> String
printBoard board =
  -- Convert ['0', '1', .. n -1] to single string
  let columnHeader = unwords $ map show [0 .. length board - 1] 
      rowContent x =
        -- Access the row at index x on board
        let row       = board !! x
            -- Traverse through row and get string representation of index
            stones    = map showStone row
            -- Combine the list of strings to a single string
            rowString = unwords stones
        in rowString
  -- Turn list of strings into single string seperated by line breaks
  -- List of strings created by mapping rowContent over a list from 0 to the length of the board-1 
  in unlines $ columnHeader : map (\x -> show x ++ " " ++ rowContent x) [0 .. length board - 1]


-- | Get stone from move
getStoneFromMove :: Move -> Stone
getStoneFromMove Pass = Nothing
getStoneFromMove (Place color _) = Just color

-- | Get position from move
getPos :: Move -> Pos
getPos Pass = (-1, -1)   -- Return invalid Pos for Pass
getPos (Place _ pos) = pos




----------------------------------------------------
-- | Keep current game status
-- | Board: gameboard, Color: color of players turn, Set Pos: unique pos on the board
data GameState = GameState Board Color (Set.Set Pos)

-- | Get board from game state
getBoard :: GameState -> Board
getBoard (GameState board _ _) = board

-- | Initialize game state
-- | Takes in a size argument and initializes a game board
initGame :: Int -> GameState
initGame size = GameState (emptyGameBoard size) Empty Set.empty

-- | Get current player
currentPlayer :: GameState -> Color
currentPlayer (GameState _ color _) = color

-- | Get list of empty positions
getEmptyPos :: Board -> [Pos]
getEmptyPos board = 
    let positions = [(x,y) | x <- [0 .. length board - 1], y <- [0 .. length (head board) -1]]
    in filter (isEmpty board) positions
