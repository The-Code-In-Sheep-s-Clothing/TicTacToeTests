-- PlayerID, Piece Type, x, y
type Piece = (Int, Int, Int, Int)

-- pieces
type Tile = ([Piece])

-- Width, Height
type Board = (Int, Int, [Tile])

-- Board, Piece Actions
type BoardLogic = (Board, [Piece -> Board -> Maybe Board])

-- PlayerTurn, Board State, Board State to winner
type Game = (Int, [BoardLogic], [BoardLogic]->Int)

move :: Piece -> Board -> Maybe Board
move (playerID, pieceType, x, y) (width, height, tiles) =
    if x > width || x < 1 || y > height || y < 1 || (playerID, pieceType, x, y) elem tiles
    then Nothing
    else (width, height, [tiles:(playerID, pieceType, x, y)])

wincon :: [BoardLogic]->Int
wincon [((3, 3, []), _)]
wincon _ = -1

tictactoe :: Game
tictactoe = (((3, 3, []), [move]), wincon)

main = putStrLn "asdf"
