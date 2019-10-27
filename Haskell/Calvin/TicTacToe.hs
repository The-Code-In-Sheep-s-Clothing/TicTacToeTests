module BoardGame where

import Data.Maybe

-- A board is a list of spaces.
-- Each space has a coordinate.
-- A coordinate is a list of ints, size depends on dimension of board.
-- Each space has a piece.
-- A piece is either Blank, O, or X. 

data Piece = Blank | O | X 
    deriving(Eq, Show)

type Coordinate = [Int]

type Space = (Coordinate, Piece)

type Board = [Space]

-- Checks if board has winner
type WinCondition = (Board -> Bool)

-- Rule = Checks if move is valid on board
type Rule = (Board -> Move -> Bool)

type Move = (Coordinate, Piece)


type SearchResult = (Bool, Maybe Board) 

-- Search for space in board and remove it.
-- If successfully found, first part of tuple == True.
removeSpace :: Board -> Coordinate -> SearchResult 
removeSpace ((c, p):ss) t = case (coordinateEquals c t) of
                                    True -> case (remove ss t) of
                                        (_, b) -> (True, b)
                                    False -> case (removeSpace ss t) of
                                        (True, b) -> (True, ((c,p):ss))
                                        (False, b) -> (False, (c,p):ss)) 
removeSpace []  _         = (False, []) 

-- Test if two coordinates are equal
coordinateEquals :: Coordinate -> Coordinate -> Bool
coordinateEquals [] []         = True
coordinateEquals [] _          = False
coordinateEquals _ []          = False
coordinateEquals (x:xs) (y:ys) = if (x == y)
                                    then coordinateEquals xs ys
                                    else False

-- Place a piece on a board, returns Nothing if coordinate does not exist
move :: Board -> Coordinate -> Piece -> Maybe Board
move b c p = case (removeSpace b c) of 
                    Just (False, _) -> Nothing
                    Just (True, b) -> Just (b ++ (c, p))


-- Creates board with all blank squares
createBoard :: [Int] -> [Int] -> Board
createBoard (d:[]) c = boardize c (arrayize d)
createBoard (d:ds) c = boardize c (arrayize d) 

arrayize :: Int -> [Int]
iterate 1 = [0]
iterate n = [n-1] ++ arrayize n-1 

boardize :: [Int] -> [Int] -> Board
boardize c (x:[]) = [(c ++ x, Blank)]
boardize c (x:xs) = [(c ++ x, Blank)] ++ boardize c xs

-- First check for win conditions
-- Next check if valid 
play :: Board -> Move -> [WinCondition] -> [Rule] -> Either Piece Board
play b m w r = case (evaluateBoard b w) of
                True -> case (validMove b m r) of 
                    True -> Right (move b 
                False -> Left Blank


evaluateBoard :: Board -> [WinCondition] -> Bool
evaluateBoard b [] = True
evaluateBoard b (w:ws) = (evaluateBoard b ws) && (w b)  


