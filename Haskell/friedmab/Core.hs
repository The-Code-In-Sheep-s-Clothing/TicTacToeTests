--
-- Example Core DSL for Playing TicTacToe (and maybe Connect 4)
--
-- Benjamin Friedman
-- 10/22/2019
--

import Data.Char

-- TODO restate this to make a nice clear summary of what's going on here
-- repesent board as Int
-- represent player as an Int Id
-- pieces represented as coordinates with a player Id
-- rules represented as
  -- GameState -> Piece -> (Position -> Position) -> GameState
  -- rules not listed are NOT allowed (we explicitly list all rules in this naiive implementation)
-- playing a game involves transformations of 'Move's, which are bound by all 'Rule's defined
-- Game End is checked for at end of move, defined by moves which end in
  -- GAME_END (nobody wins)
  -- GAME_WIN (with id of player that wins)
--
-- BREAKDOWN
-- Breaks the problem up into
-- PLAYING a Game which consists of
--  ROUNDS, which are played by 'n' players, for each which has a
--    TURN, where the player attempts to manipulate a piece with the board according to the given rules, and results in a
--      VALID GAMESTATE the reflects the change, and returns to the next turn in the round
--      OR
--      AN INVALID GAMESTATE, which reprompts for this player's turn action before continuing
--

--
-- DSL Components (Semantics)
--

-- Idea of a player as an Id
type Player = Int


-- indicates which player is currently 'up'
type Turn = Int


-- Position on a Board
-- composed of (x,y) coordinates
type Position = (Int,Int)


-- Piece on the Board
-- composed of (position of value & playerId)
type Piece = (Int,Player)


-- unoccupied position on the board
type Space = (Position,Maybe Piece)


-- Represent Board as a list of spaces, with maybe pieces on them
type Board = [Space]


-- Used to mark the possible outcomes from an action
-- Basically describes, VALID, INVALID, Or WIN (for a given player)
data Outcome
  -- valid outcome, game continues (Y)
  = Valid
  -- invalid outcome, likely a bad move being made (N)
  | Invalid
  -- win outcome, current player wins (W)
  | Win Player
  deriving(Eq,Show)


-- Holds the entire game thing
-- as (
--  board dimension (in one direction),
--  list of pieces,
--  list of players,
--  current turn,
--  list of rules
-- )
type GameState = (Board,[Player],Turn,[Rule],Outcome,[Move])


-- 3 types of moves
-- A place, for a new piece at a position on the Board
-- A move, for moving an existing piece on the board
-- And an end, to stop a Move
-- Any move should be at least 1 or more actions before endings
-- (assuming someone doesn't forfeit their turn, which could be symbolized as an 'End')
data Move
  = Place Piece Position
  deriving(Eq,Show)


-- Construct of what rules are in a board game
-- may be a condition for a change from one Piece to another Piece,
-- followed by any number of other rules, ending in a result
-- with a fixed outcome of Y/N/W
-- basically divides rules up into 3 major categories
--    moving rules, placing rules, ending rules (and then adds a result just to cap it off)
-- TODO
data Rule
  -- rule guarding a move, given the current state
--  = MoveRule GameState Move Rule
  -- describes how a piece may be placed
  = PlaceRule Piece Position Rule
  -- rule guarding the end of the game (win,loss,stalemate)
  -- regarded purely by using pieces
  | EndRule Piece Rule
  -- final result of any rule, either Valid/Invalid/Win (loss for other player)
  | Result Outcome
  deriving(Eq,Show)


-- list of rules
type Rules = [Rule]

--
-- ~~~~~~~~~
--


-- count number of items in the list, quickly
--listnumber :: [Int] -> Int
--listnumber [] = 0
--listnumber (l:le) = 1 + listnumber le



-- return whether a space is empty
spaceEmpty :: Board -> Int -> Int -> Bool
-- handle nothing left, which is False
spaceEmpty [] _ _ = False
-- handle a possible space match
spaceEmpty (((x,y),Nothing):board) x2 y2 = if x == x2 && y == y2 then True else spaceEmpty board x2 y2
-- handle occupied spaces
spaceEmpty (_:board) x y = spaceEmpty board x y



-- TODO, convert to, Move -> [Rule] -> Outcome, later one
-- validates a given move, uses coded moves (for now)
checkMove :: Board -> Move -> Outcome
-- handle no moves, this is valid
checkMove [] _ = Invalid
-- for any other move we have to deal with
-- verify move is on the board
-- verify move is on an empty space
checkMove board (Place _ (x,y)) = if x >= 0 && x < getBoardDimen board && spaceEmpty board x y then Valid else Invalid


-- returns board dimension
_getBoardDimen :: Board -> Int
_getBoardDimen [] = 0
_getBoardDimen (_:board) = 1 + _getBoardDimen board


isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

getBoardDimen :: Board -> Int
getBoardDimen [] = 0
getBoardDimen board = isqrt (_getBoardDimen board)


-- applies a move to a given piece, updating the gamestate
applyMove :: Board -> Move -> Board
-- nothing left to apply to
applyMove [] _ = []
applyMove (((x1,y1),_):board) (Place (value,playerId) (x2,y2)) = if x1 == x2 && y1==2 then (((x2,y2),Just (value,playerId)):board) else applyMove board (Place (value,playerId) (x2,y2))


-- check if horizontal victory
-- board, and board dimen, then bool
checkInRow_Horiz :: Board -> Int -> Int -> Bool
-- cheap true case
checkInRow_Horiz [] _ _ = False
checkInRow_Horiz _ _ 3 = True
-- if row valid, keep checking this row, else
checkInRow_Horiz ((_, Just (_,playerId)):board) maxCount count = if playerId == 0 then checkInRow_Horiz board maxCount (count+1) else checkInRow_Horiz (drop 2 board) maxCount 0


_checkInRow_Vert :: Board -> Int -> Int -> Bool
_checkInRow_Vert [] _ _ = False
_checkInRow_Vert _ _ 3 = True
-- if row valid, keep checking this row, else
_checkInRow_Vert ((_, Just (_,playerId)):board) maxCount count = if playerId == 0 then _checkInRow_Vert (drop 2 board) maxCount (count+1) else False

-- check if horizontal victory
-- board, and board dimen, then bool
checkInRow_Vert :: Board -> Int -> Bool
checkInRow_Vert [] _  = False
checkInRow_Vert board maxCount = _checkInRow_Vert board maxCount 0 || _checkInRow_Vert (drop 1 board) maxCount 0 || _checkInRow_Vert (drop 2 board) maxCount 0

-- check diagonal forward
_checkInRow_DiagFwd :: Board -> Int -> Int -> Bool
_checkInRow_DiagFwd [] _ _ = False
_checkInRow_DiagFwd ((_, Just (_,playerId)):board) maxCount count = if playerId == 0 then _checkInRow_DiagFwd (drop 3 board) maxCount (count+1) else False


-- check diagonal backward
_checkInRow_DiagBwd :: Board -> Int -> Int -> Bool
_checkInRow_DiagBwd [] _ _ = False
_checkInRow_DiagBwd ((_, Just (_,playerId)):board) maxCount count = if playerId == 0 then _checkInRow_DiagFwd (drop 1 board) maxCount (count+1) else False

-- check diagonal
checkInRow_Diag :: Board -> Int -> Bool
checkInRow_Diag board maxCount = _checkInRow_DiagFwd board maxCount 0 || _checkInRow_DiagBwd (drop 2 board) maxCount 0

-- check for victory
checkForVictory :: Board -> Outcome -> Turn -> Outcome
-- base case, no pieces on the board, just use our existing outcome
checkForVictory [] outcome _ = outcome
-- TODO check if 'n' in a row, horizontally, vertically, or diagonally
checkForVictory board output turn = if checkInRow_Horiz board (getBoardDimen board) 0 && checkInRow_Vert board (getBoardDimen board) && checkInRow_Diag board (getBoardDimen board) then Win turn else output


-- plays a turn of the game for a player, then switches back
playTurn :: GameState -> GameState
-- valid, play on...                          checkMove(dimen,move)
playTurn (board,p,turn,rules,o,move:moves) = case checkMove board move of
  -- valid move, update and check to apply a victory/end state to the pieces
  Valid  -> (applyMove board move, p,turn,rules,checkForVictory (applyMove board move) o turn,moves)
  -- Bad move
  Invalid -> (board,p,turn,rules,Invalid,moves)
  -- Win case, end the game
  Win winningPlayer -> (applyMove board move,p,turn,rules,Win winningPlayer,moves)


-- starts running rounds of a game, ticking the turns appropriately
playGame :: GameState -> GameState
-- play game reccursively calls playRound until done
-- if that Turn is less than # players, execute a turn for that #
-- else, reset Turn to Zero & execute a turn for that player
playGame gs = case playTurn gs of
  -- good move, go to the next turn
  (b,players,turn,r,Valid,moves) -> playGame (b,players, if length players >= turn+1 then turn+1 else 0,r,Valid,moves)
  -- Bad move or End Game, return
  gsn -> gsn


-- Tries to run a game for a given moveset
play :: GameState -> Outcome
play gs = case playGame gs of
  (b,p,t,r,outcome,m) -> outcome

--main = play ([],[0,1],0,[],Valid,[])
