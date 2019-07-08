type Var = (String, Maybe Int)
type Func = (String, (GameState -> IO GameState))

data Tile = Tile {tile_pieces :: [Piece],
                  tile_vars   :: [Var]} 
    deriving (Read, Show, Eq, Ord)

data Board = Board {board       :: [[Tile]],
                    board_vars  :: [Var]}
    deriving (Read, Show, Eq, Ord)

data Piece = Piece {rep        :: Char,
                    piece_vars :: [Var]}
    deriving (Read, Show, Eq, Ord)

data Player = Player {id            :: Int,
                      player_pieces :: [Piece],
                      player_vars   :: [Var]}
    deriving (Read, Show, Eq)

data GameState = GameState {current_board  :: Board,
                            current_player :: Int,
                            next_player    :: Int,
                            player_list    :: [Player],
                            player_actions :: [Func]}

-- Builtins
init_board :: Int -> Int -> Tile -> [Var] -> Board
init_board x y t v = Board [[t | i <- [1..x]] | j <- [1..y]] v

init_players :: Int -> [[Piece]] -> [Var] -> [Player]
init_players 0 [] v = []
init_players n (p:ps) v = (init_players (n-1) ps v) ++ [(Player n p v)]

call_function :: [Func] -> String -> GameState -> IO GameState
call_function ((n, f):xs) s g 
        | n == s = f g 
        | otherwise = call_function xs s g

get_var :: [Var] -> String -> Maybe Int
get_var [] _ = Nothing
get_var ((n, f):xs) s 
        | n == s = f 
        | otherwise = get_var xs s

set_var :: [Var] -> String -> Maybe Int -> [Var]
set_var [] _ _ = []
set_var ((n, f):xs) s i 
        | s == n = (n, i) : tl
        | otherwise = (n, f) : tl
        where tl = set_var xs s i

get_next_player :: GameState -> GameState
get_next_player (GameState b c n l a) = (GameState b n (mod (n+1) (length l)) l a)

input_int :: String -> IO Int
input_int out = do
                print_string out
                inp <- getLine
                return(read inp :: Int)

input_string :: String -> IO String
input_string out = do
                   print_string out
                   inp <- getLine
                   return(read inp)

print_string :: String -> IO ()
print_string s = do putStrLn s

get_tile :: Int -> Int -> GameState -> Tile
get_tile x y g = (((board . current_board) g)!!x)!!y

set_tiles_helper :: GameState -> Tile -> Int -> Int -> [[Tile]]
set_tiles_helper m x r c =
    take r ((board . current_board) m) ++
    [take c (((board . current_board) m) !! r) ++ [x] ++ drop (c + 1) (((board . current_board) m) !! r)] ++
    drop (r + 1) ((board . current_board) m)

set_tile_var :: Int -> Int -> String -> Maybe Int -> GameState -> GameState
set_tile_var x y s i g = (GameState (Board 
                             (set_tiles_helper g (Tile (tile_pieces (get_tile x y g))
                                 (set_var (tile_vars (get_tile x y g)) s i)) x y)
                             ((board_vars . current_board) g)) 
                         (current_player g) (next_player g) (player_list g) (player_actions g))

-- TODO: Expand to work with other var types
all_tile :: [Tile] -> String -> Maybe Int -> Bool
all_tile [] s i = True
all_tile (h:t) s i = if (get_var (tile_vars h) s) == i then (all_tile t s i) else False

board_row :: Int -> GameState -> [Tile]
board_row i g = ((board . current_board) g)!!i

board_col :: Int -> GameState -> [Tile]
board_col i g = map (head . drop i) ((board . current_board) g)

main :: IO ()
main = do 
       let initial_state = game_init
       game_loop =<< initial_state
       return () 

-- Written by developer
-- Required
game_init :: IO GameState
game_init = do 
            let board = (init_board 3 3 (Tile [] [("owned", Nothing)]) []) 
            let players = (init_players 2 [[Piece 'x' []], [Piece 'o' []]] [])
            return (GameState board 0 1 players [("move", player_move)])

-- Required
game_loop :: GameState -> IO ()
game_loop g = do 
              new_g <- call_function (player_actions g) "move" g
              game_over <- game_end new_g
              if (game_over) then return () 
              else game_loop (get_next_player new_g)

-- Required
game_end :: GameState -> IO Bool
game_end g = do
             -- Currently does not support loops, we check every condition by hand. Very tedious. FIXME
             if (all_tile (board_row 0 g) "owned" (Just 0)) then do 
                print_string "Player 0 wins"
                return True
             else if (all_tile (board_row 1 g) "owned" (Just 0)) then do 
                print_string "Player 0 wins"
                return True
             else if (all_tile (board_row 2 g) "owned" (Just 0)) then do 
                print_string "Player 0 wins"
                return True
             else if (all_tile (board_col 0 g) "owned" (Just 0)) then do 
                print_string "Player 0 wins"
                return True
             else if (all_tile (board_col 1 g) "owned" (Just 0)) then do 
                print_string "Player 0 wins"
                return True
             else if (all_tile (board_col 2 g) "owned" (Just 0)) then do 
                print_string "Player 0 wins"
                return True
             else if (all_tile (board_row 0 g) "owned" (Just 1)) then do 
                print_string "Player 1 wins"
                return True
             else if (all_tile (board_row 1 g) "owned" (Just 1)) then do 
                print_string "Player 1 wins"
                return True
             else if (all_tile (board_row 2 g) "owned" (Just 1)) then do 
                print_string "Player 1 wins"
                return True
             else if (all_tile (board_col 0 g) "owned" (Just 1)) then do 
                print_string "Player 1 wins"
                return True
             else if (all_tile (board_col 1 g) "owned" (Just 1)) then do 
                print_string "Player 1 wins"
                return True
             else if (all_tile (board_col 2 g) "owned" (Just 1)) then do 
                print_string "Player 1 wins"
                return True
             else return False

player_move :: GameState -> IO GameState
player_move g = do
                print_string ("Move for player: " ++ show (current_player g))
                x <- input_int "Input x coord"
                y <- input_int "Input y coord"
                let tile = get_tile x y g
                if (get_var (tile_vars tile) "owned") == Nothing then do
                    let new_state = set_tile_var x y "owned" (Just (current_player g)) g
                    return new_state
                else do
                    print_string "Invalid move, try again"
                    player_move g


-- Possible syntax
-- Init (
--     Board(3, 3, Tile(owner))
--     Players(2, ['x', 'o'])
--     PlayerActions(move)
--     // Globals() define global variables
--     // GlobalActions() define non player actions
-- )
-- Loop (
--     PrintBoard // Built in? Print the board with all tile attributes/other information
--     Players
--     End
-- )
-- PlayerActions.move (
--     // Player that is performing the action will be referenced with 'player'    
--     do (
--         Print('Move for player: ' . player)
--         Input(x, 'x coord of move')
--         Input(y, 'y coord of move')
--     ) while Board[x][y].owner is not None
--     Board[x][y].owner = player
--     // Consider returning the next player (ex: UNO needs this)
-- )
-- End (
--     for row in Board.rows (
--         if row.owner are not None and row.owner are equal ( // are keyword will check every element in list
--             ret row[0].owner
--         )
--     )
--     for col in Board.cols (
--         if col.owner are not None and col.owner are equal (
--             ret col[0].owner
--         )
--     )
--     for player in Players (
--         if Board[0][0].owned is player and Board[1][1].owned is player and Board[2][2].owned is player (
--             ret player
--         ) elif Board[2][0].owned is player and Board[1][1].owned is player and Board[2][0].owned is player (
--             ret player
--         )
--     )
--     ret None
-- )
