module BoardGame where

import Data.Maybe

data Piece = Blank | O | X 
    deriving(Eq, Show)

type Coordinate = [Int]

type Space = (Coordinate, Piece)

type Board = [Space]

-- Search for space in board
getSpace :: Board -> Coordinate -> Maybe Space
getSpace ((c, p):ss) t = case (coordinateEquals c t) of
                                    True -> Just (c, p)
                                    False -> getSpace ss t 
getSpace []  _         = Nothing

-- Test if two coordinates are equal
coordinateEquals :: Coordinate -> Coordinate -> Bool
coordinateEquals [] []         = True
coordinateEquals [] _          = False
coordinateEquals _ []          = False
coordinateEquals (x:xs) (y:ys) = if (x == y)
                                    then coordinateEquals xs ys
                                    else False
