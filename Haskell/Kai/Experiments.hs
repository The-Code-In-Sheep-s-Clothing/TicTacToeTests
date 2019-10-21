module Experiments where
import Data.Maybe
import Data.List

-- The state is a bunch of ordered pairs.
type State a b = [(a, b)]

-- A piece is one of these ordered pairs
type Piece a b = (a, b)

-- A move is transformation from state to state
type Move a b = (State a b -> State a b)

-- Check what's happening in a game
type StatusPred a b = State a b -> Status

-- A game is always in one of these states:
data Status = Win | Loss | Stalemate | Continue
  deriving Show

-- A successor function is all the possible moves glued together, with all the illegal states removed.
type Successor a b = State a b -> [State a b]

-- A game is an initial state, a successor function, and an end check (could also be Either State Status, maybe better?)
type Game a b = (State a b, Successor a b, StatusPred a b)

-- A rule says if a state is allowed or not.
type Rule a b = State a b -> Bool

-- Lifting and to function types
liftAnd :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
liftAnd f g = \x -> f x && g x

-- Combine all rules into a single rule: if any rule fails, it should return false.
allRules :: [Rule a b] -> Rule a b
allRules rs = foldl (liftAnd) (const True) rs

-- Combine all the possible moves, combine all the rules, and produce a single successor function.
produceSuccessor :: [Move a b] -> [Rule a b] -> Successor a b
produceSuccessor (ms) rs = (\s -> filter (allRules rs) $ glue ms s)

-- Combine the outputs of two functions into a single list
combine :: (a -> b) -> (a -> b) -> a -> [b]
combine f g x = [f x] ++ [g x]

-- Glue together the outputs of a list of functions
glue :: [(a -> b)] -> a -> [b]
glue (f:fs) = \x -> f x:glue fs x
glue [] = \x -> []

-- Rules of nim. You can subtract 1, 2, or 3, but you can't go below zero.
nimMoves = [\[((), x)] -> [((), x-1)], \[((), x)] -> [((), x-2)], \[((), x)] -> [((), x-3)]] :: [Move () Int]
nimRules = [(\[((), x)] -> x >= 0)]

-- Here's (misere) nim. You start with fifteen, and try not to get to zero
nim :: Game () Int
nim = (start, moves, pred)
  where
    start = [((), 15)]
    moves = produceSuccessor nimMoves nimRules
    pred = (\[((), x)] -> if x == 0 then Loss else Continue)




-- mnk in a row games

-- check for same index. should just use `on` instead of this function
sameIndex :: Eq a => Piece a b -> Piece a b -> Bool
sameIndex (i, _) (i', _) = i == i'

-- check for same spot
sameSpot :: Piece (Int, Int) Char -> Piece (Int, Int) Char -> Bool
sameSpot ((x, y), _) ((u, v), _) = x == u && y == v

-- Pieces can't be off board.
offBoard m n ((x, y), _) = x > m && y > n
notOffBoardAny m n st = any (offBoard m n) st

-- No two pieces in the same spot.
boardOK :: State (Int, Int) Char -> Bool
boardOK (p:st) = foldl (&&) True (map (not . sameSpot p) st) && boardOK st
boardOK [] = True

-- Pointwise add two 2-tuples
pwadd (x, y) (z, w) = (x + z, y + w)
countPiece :: State (Int, Int) Char -> (Int, Int)
countPiece ((_, c):st) = pwadd (if c == 'X' then (1, 0) else (0, 1)) $ countPiece st
countPiece [] = (0, 0)

-- Can't have any more than one more 'X' than 'O'
turnCheck :: State (Int, Int) Char -> Bool
turnCheck st = case countPiece st of
  (x, y) -> if (y > x || x > y + 1 ) then False else True

-- List of all of our rules, parameterized by the size of the board
ticTacToeRules :: Int -> Int -> [Rule (Int, Int) Char]
ticTacToeRules m n = [
  boardOK,
  turnCheck,
  not . (notOffBoardAny m n)
                 ]

-- A move is just an addition of another piece to the board
ticTacToeMoves :: Int -> Int -> [Move (Int, Int) Char]
ticTacToeMoves u v = map (\x st -> x:st) [((x, y), c) | x <- [1..u], y <- [1..v], c <- ['X', 'O']]


-- inARow :: State, Direction, Number in a row desired. Returns True or False. Can simplify the function a little if you sort first.
inARow :: State (Int, Int) Char -> State (Int, Int) Char -> (Int, Int) -> Int -> Bool
inARow state (s:st) d n = (inARow' state s d n) || inARow state st d n
  where
    inARow' :: State (Int, Int) Char -> Piece (Int, Int) Char -> (Int, Int) -> Int -> Bool
    inARow' _ _ _ 1 = True
    inARow' st ((x, y), c) (dx, dy) n = case lookup (x+dx, y+dy) st of
      Nothing -> False
      Just c' -> if c == c' then inARow' state ((x+dx, y+dy), c) (dx, dy) (n-1) else False
inARow _ _ _ _ = False


-- Check for the end of the game
ticTacToeEnd :: Int -> StatusPred (Int, Int) Char
ticTacToeEnd k st = if (ticTacToeEnd' st) then Win else Continue
  where
    ticTacToeEnd' [] = False
    ticTacToeEnd' st = foldl (||) False (map (\d -> inARow st st d k) [(1,0), (0,1), (1,1)]) -- Check for horizontal (1,0), vertical (0,1), and diagonal (1,1) victories

-- Here's tic-tac-toe. Start with an empty board.
ticTacToe :: Int -> Int -> Int -> Game (Int, Int) Char
ticTacToe m n k  = (start, moves, pred)
  where
    start = []
    moves = produceSuccessor (ticTacToeMoves m n) (ticTacToeRules m n)
    pred = ticTacToeEnd k

-- Connect four is a specific type of tic-tac-toe.
connectFour :: Game (Int, Int) Char
connectFour = (start, moves, pred)
  where
    start = []
    moves = produceSuccessor (ticTacToeMoves 7 6) ((ticTacToeRules 7 6) ++ [connectFourRule])
    pred = ticTacToeEnd 4

-- the only difference between tic tac toe and connect four is this rule:
connectFourRule :: Rule (Int, Int) Char
connectFourRule = connectFourRule' . sortGrid
connectFourRule' (s:st) = case s of
  ((_, 6), _ ) -> True
  ((x, y), _) -> if (any (sameSpot ((x, y+1), 'O')) st) then True else False

-- You can also do 0-player games like cellular automata. This is unimportant but kind of cool.

toBinary :: Int -> String
toBinary 0 = "0"
toBinary x = (toBinary (x `div` 2)) ++ if x `mod` 2 == 1 then "1" else "0"
padLeft :: String -> String
padLeft s = (concat $ replicate (8 - (length s)) "0") ++ s
ruleN n (p@(x,y):st') st = case (lookup (x-1) st, Just y, lookup (x+1) st) of
    (Just True, Just True, Just True) -> if (bin !! 0) == '1' then (x,True):(ruleN n st' st) else (x,False):(ruleN n st' st)
    (Just True, Just True, Just False) -> if (bin !! 1) == '1' then (x,True):(ruleN n st' st) else (x,False):(ruleN n st' st)
    (Just True, Just False, Just True) -> if (bin !! 2) == '1' then (x,True):(ruleN n st' st) else (x,False):(ruleN n st' st)
    (Just True, Just False, Just False) -> if (bin !! 3) == '1' then (x,True):(ruleN n st' st) else (x,False):(ruleN n st' st)
    (Just False, Just True, Just True) -> if (bin !! 4) == '1' then (x,True):(ruleN n st' st) else (x,False):(ruleN n st' st)
    (Just False, Just True, Just False) -> if (bin !! 5) == '1' then (x,True):(ruleN n st' st) else (x,False):(ruleN n st' st)
    (Just False, Just False, Just True) -> if (bin !! 6) == '1' then (x,True):(ruleN n st' st) else (x,False):(ruleN n st' st)
    (Just False, Just False, Just False) -> if (bin !! 7) == '1' then (x,True):(ruleN n st' st) else (x,False):(ruleN n st' st)
    otherwise -> (x,True):ruleN n st' st
    where
        bin = (padLeft . toBinary) n
-- Cellular automata normally don't have edges. This produces weird behavior.
ruleN _ st' st = st'

-- A 1d cellular automata
rule :: Int -> Game Int Bool
rule n = (start, moves, pred)
  where
    start = [(x,False) | x <- [1..20]] ++ [(21, True)] ++ [(x, False) | x <- [22..40]]
    moves = produceSuccessor [\st -> ruleN n st st] []
    pred = const Continue

-- Grid sorting functions.
sortGrid :: State (Int, Int) b -> State (Int, Int) b
sortGrid st = sortBy (\((x, y), _) ((u, v), _) -> compare y v) $ sortBy (\((x, y), _) ((u, v), _) -> compare x u) st
-- transposed
sortGrid' st = sortBy (\((x, y), _) ((u, v), _) -> compare x u) $ sortBy (\((x, y), _) ((u, v), _) -> compare y v) st


-- Display (uxv grid)... this is pretty bad, but it works.
printGrid :: Show b => (Int, Int) -> State (Int, Int) b -> String
printGrid (u, v) st = printGrid' (u, v) (0,1) $ sortGrid st
  where
-- oh no
    printGrid' (u, v) (px, py) (((x, y), c):st) = (if y > py then (concat . replicate (y - py)) ((empties (u - px)) ++ "\n") else "") {--(if (py /= y && px < u) then empties (u - px) ++ "\n" else if (py /= y) then "\n" else "")-}
                                              ++ (if x > px then empties ((x - px) - 1) else "") ++ "|  " ++ show c ++ "  |" ++ printGrid' (u, v) (x, y) st
    printGrid' (u, v) (px, py) [] = if (py > v) then "" else
      ((if (u > px) then empties (u - px) else "") ++
      if (py < v) then "\n" ++ (printGrid' (u, v) (0, py+1) []) else "")

    empties 0 = ""
    empties x
      | x > 0 = "|     |" ++ empties (x - 1)

-- display a 1d strip
displayStrip :: State Int Bool -> String
displayStrip ((_, s):st) = (if s then "X" else " ") ++ displayStrip st
displayStrip [] = ""

-- play any game
playGame :: (Show a, Show b) => Game a b -> (State a b -> String) -> IO ()
playGame (s, m, f) sh = do
  putStrLn $ show s
  putStrLn $ "State: " ++ "\n" ++  sh s
  putStrLn $ "Moves" ++ "\n" ++ (intercalate "\n\n\n\n" $ map sh (m s))
  move <- getLine
  s' <- return $ ((m s) !! (read move))
  case f s' of
        Win -> putStrLn (sh s') >> putStrLn "You win!"
        Loss -> putStrLn "You lose!"
        Stalemate -> putStrLn "Stalemate!"
        Continue -> playGame (s', m, f) sh
