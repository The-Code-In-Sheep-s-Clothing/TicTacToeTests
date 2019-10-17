-- TYPES
-- PlayerID, Piece Type, x, y
type Piece = (Int, Int, Int, Int)

-- Width, Height
type Board = (Int, Int, [Piece])

-- Piece, old board, new Board
type Action = Piece -> Board -> Maybe Board

-- Board, Piece Actions
type BoardLogic = (Board, [Action])

data BoardState = Playing [BoardLogic]
                | Finished Int

-- PlayerTurn, Board State, Board State to winner, print board
type Game = (Int, BoardState, [BoardState->Int], BoardState -> [Char])


-- LOGIC
type Turns = [(Piece, Action)]

cleanAction :: Maybe Board -> Board
cleanAction (Just b) = b
cleanAction Nothing = (0, 0, [])

playTurns :: Turns -> Maybe Game -> Maybe Game
playTurns [] g = g
playTurns ((piece, act):xs) (Just (playerturn, Playing [(board, possibleactions)], winfunc, prnt)) =
    if((act piece board) /= Nothing)
    then Just (mod (playerturn + 1) 2, Playing [(cleanAction (act piece board), possibleactions)], winfunc, prnt)
    else Nothing
playTurns _ _ = Nothing

dummyPrint :: BoardState -> [Char]
dummyPrint _ = "Error"

cleanGame :: Maybe Game -> Game
cleanGame (Just g) = g
cleanGame Nothing = (0, (Finished 0), [], dummyPrint)

checkWinstates :: BoardState -> [BoardState->Int] -> Int
checkWinstates b (x:xs) = if x b /= -1
                     then x b
                     else checkWinstates b xs
checkWinstates b [] = -1

gameLoop :: Maybe Game -> IO()
gameLoop (Just (playerturn, Playing [(board, actions)], winstates, printer)) = do
    m <- getLine
    let mi = read m :: Int
    x <- getLine
    let xi = read x :: Int
    y <- getLine
    let yi = read y :: Int

    let newboard = Playing [(cleanAction((actions!!mi) (playerturn, 1, xi, yi) board), actions)]

    if(checkWinstates newboard winstates == -1)
    then putStrLn (printer newboard)
    else putStrLn (printer (Finished (checkWinstates newboard winstates)))

    let newstate = (Just (mod (playerturn + 1) 2, newboard, winstates, printer))
    gameLoop newstate
    return ()
gameLoop _ = do
    putStrLn "gameover"
    return ()

main :: IO()
main = gameLoop (Just tictactoe)

-- GAME DEFINITION
move :: Action
move (playerID, pieceType, x, y) (width, height, tiles) =
    if x > width || x < 1 || y > height || y < 1 || elem (playerID, pieceType, x, y) tiles
    then Nothing
    else Just (width, height, tiles ++ [(playerID, pieceType, x, y)])

wincon :: BoardState -> Int
wincon (Finished w) = w
wincon (Playing [((sx, sy, ((pid, pt, x, y):xs)), funcs)]) =
    if  (elem (pid, pt, x + 1, y) xs && elem (pid, pt, x + 2, y) xs) ||
        (elem (pid, pt, x, y + 1) xs && elem (pid, pt, x, y + 2) xs) ||
        (elem (pid, pt, x + 1, y + 1) xs && elem (pid, pt, x + 2, y + 1) xs)
    then pid
    else wincon (Playing [((sx, sy, xs), funcs)])
wincon _ = -1

printBoard :: BoardState -> [Char]
printBoard (Playing [((bx, by, []), _)]) = "end\n"
printBoard (Playing [((bx, by, ((pid, t, x, y):xs)), _)]) = "pid: " ++ show pid ++ "  x: " ++ show x ++ "  y: " ++ show y ++ "\n" ++ printBoard (Playing [((bx, by, xs), [])])
printBoard (Finished w) = "player " ++ show w ++ " won the game!"

tictactoe :: Game
tictactoe = (1, Playing [((3, 3, []), [move])], [wincon], printBoard)
