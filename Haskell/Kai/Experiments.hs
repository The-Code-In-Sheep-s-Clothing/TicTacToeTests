module Experiments where
import Data.Maybe
import Data.List

-- The state is a bunch of ordered pairs.
type State a b = [(a, b)]
type Piece a b = (a, b)
type Description = String
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

{-
data MoveSyntax a b = Moves (MoveSyntax a b) (MoveSyntax a b)   |
                      CheckState (StatePred a b) (MoveSyntax a b) |
                      ModState (State a b -> State a b)

data PredSyntax a b = AnyIndex (a -> Bool) | AnyField (b -> Bool)

compilePred :: PredSyntax a b -> StatePred a b
compilePred (AnyIndex p) = \st -> any p (map fst st)
compilePred (AnyField p) = \st -> any p (map snd st)

nimA :: PredSyntax Integer ()
nimA = AnyIndex (> 2)

nimB = AnyIndex (> 1)

type Successor a b = (State a b -> [State a b])

-}
-- A rule says if a state is allowed or not.
type Rule a b = State a b -> Bool

liftAnd :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
liftAnd f g = \x -> f x && g x

allRules :: [Rule a b] -> Rule a b
allRules rs = foldl (liftAnd) (const True) rs


produceSuccessor :: [Move a b] -> [Rule a b] -> Successor a b
produceSuccessor (ms) rs = (\s -> filter (allRules rs) $ glue ms s)

combine :: (a -> b) -> (a -> b) -> a -> [b]
combine f g x = [f x] ++ [g x]

glue :: [(a -> b)] -> a -> [b]
glue (f:fs) = \x -> f x:glue fs x
glue [] = \x -> []
-- This is all of nim. That's it.
nimMoves = [\[((), x)] -> [((), x-1)], \[((), x)] -> [((), x-2)], \[((), x)] -> [((), x-3)]] :: [Move () Int]
nimRules = [(\[((), x)] -> x >= 0)]

nim :: Game () Int
nim = (start, moves, pred)
  where
    start = [((), 15)]
    moves = produceSuccessor nimMoves nimRules
    pred = (\[((), x)] -> if x == 0 then Loss else Continue)
-- Nim


-- mnk in a row games
sameIndex :: Eq a => Piece a b -> Piece a b -> Bool
sameIndex (i, _) (i', _) = i == i'



sameSpot :: Piece (Int, Int) Char -> Piece (Int, Int) Char -> Bool
sameSpot ((x, y), _) ((u, v), _) = x == u && y == v

offBoard m n ((x, y), _) = x > m && y > n
notOffBoardAny m n st = any (offBoard m n) st
  -- No two pieces in the same spot.
boardOK :: State (Int, Int) Char -> Bool
boardOK (p:st) = foldl (&&) True (map (not . sameSpot p) st) && boardOK st
boardOK [] = True

pwadd (x, y) (z, w) = (x + z, y + w)
countPiece :: State (Int, Int) Char -> (Int, Int)
countPiece ((_, c):st) = pwadd (if c == 'X' then (1, 0) else (0, 1)) $ countPiece st
countPiece [] = (0, 0)


-- Can't have any more than one more 'X' than 'O'
turnCheck :: State (Int, Int) Char -> Bool
turnCheck st = case countPiece st of
  (x, y) -> if (y > x || x > y + 1 ) then False else True

ticTacToeRules :: Int -> Int -> [Rule (Int, Int) Char]
ticTacToeRules m n = [
  boardOK,
  turnCheck,
  not . (notOffBoardAny m n)
                 ]
ticTacToeMoves :: Int -> Int -> [Move (Int, Int) Char]
ticTacToeMoves u v = map (\x st -> x:st) [((x, y), c) | x <- [1..u], y <- [1..v], c <- ['X', 'O']]
-- should be sorted in the typical way
horiz Nothing (s:st) n = horiz (Just s) st n || horiz Nothing st n
horiz Nothing _ _      = False -- I think
horiz _ _ 1 = True
horiz (Just ((x, y), c)) (s@((x',y'), c'):st) n = x' == x + 1 && y == y' && c == c' && horiz (Just s) st (n-1)
horiz _ _ _ = False


horiz2 :: State (Int, Int) Char -> Int -> Bool
horiz2 st n = horiz2' st n
  where
    horiz2' _ 1 = True
    horiz2' [] _ = False
    horiz2' state@(((x, y), c):st') n' = ((case lookup (x+1, y) st' of
                                  Nothing -> False
                                  Just c' -> c' == c
                                    && (horiz2' ([((x+1, y), c)] ++ st')) (n'-1))) || horiz2' st' n

vert2 :: State (Int, Int) Char -> Int -> Bool
vert2 st n = vert2' st n
  where
    vert2' _ 1 = True
    vert2' [] _ = False
    vert2' state@(((x, y), c):st') n' = ((case lookup (x, y+1) st' of
                                  Nothing -> False
                                  Just c' -> c' == c
                                    && (vert2' ([((x, y+1), c)] ++ st')) (n'-1))) || vert2' st' n

diag2 :: State (Int, Int) Char -> Int -> Bool
diag2 st n = diag2' st n
  where
    diag2' _ 1 = True
    diag2' [] _ = False
    diag2' state@(((x, y), c):st') n' = ((case lookup (x+1, y+1) st' of
                                  Nothing -> False
                                  Just c' -> c' == c
                                    && (diag2' ([((x+1, y+1), c)] ++ st')) (n'-1))) || diag2' st' n

-- should be sorted in the other way
vert Nothing (s:st) n = vert (Just s) st n || vert Nothing st n
vert Nothing _ _      = False -- I think
vert _ _ 1 = True
vert (Just ((x, y), c)) (s@((x',y'), c'):st) n = x' == x && y + 1 == y' && c == c' && vert (Just s) st (n-1)
vert _ _ _ = False

ticTacToeEnd :: Int -> StatusPred (Int, Int) Char
ticTacToeEnd k st = if (ticTacToeEnd' st) then Win else Continue
  where
    ticTacToeEnd' [] = False
    ticTacToeEnd' st = horiz2 (sortGrid st) k || vert2 (sortGrid' st) k || diag2 (sortGrid st) k
ticTacToe :: Int -> Int -> Int -> Game (Int, Int) Char
ticTacToe m n k  = (start, moves, pred)
  where
    start = []
    moves = produceSuccessor (ticTacToeMoves m n) (ticTacToeRules m n)
    pred = ticTacToeEnd k

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

-- You can also do 0-player games like cellular automata

toBinary :: Int -> String
toBinary 0 = "0"
toBinary x = (toBinary (x `div` 2)) ++ if x `mod` 2 == 1 then "1" else "0"
padLeft :: String -> String
padLeft s = (concat $ replicate (8 - (length s)) "0") ++ s
-- this should be a zipper
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

ruleN _ st' st = st'

-- Only one move. Rule110 is a zero player game and is thus deterministic.
rule110Move (p@(x, y):st') st = case (lookup (x-1) st, Just y, lookup (x+1) st) of
  (Just False, Just False, Just False) -> (x,False):rule110Move st' st
  (Just True, Just False, Just False) -> (x,False):rule110Move st' st
  (Just True, Just True, Just True) -> (x,False):rule110Move st' st
  (Just False, Just True, Just False) -> (x, True):rule110Move st' st
  (Just False, Just False, Just True) -> (x, True):rule110Move st' st
  otherwise -> rule110Move st' st

 
rule110Move [] _ = []

rule110moves = [
  \state -> rule110Move state state
               ]

rule :: Int -> Game Int Bool
rule n = (start, moves, pred)
  where
    start = [(x,False) | x <- [1..20]] ++ [(21, True)] ++ [(x, False) | x <- [22..40]]
    moves = produceSuccessor [\st -> ruleN n st st] []
    pred = const Continue




sortGrid :: State (Int, Int) b -> State (Int, Int) b
sortGrid st = sortBy (\((x, y), _) ((u, v), _) -> compare y v) $ sortBy (\((x, y), _) ((u, v), _) -> compare x u) st
-- transposed
sortGrid' st = sortBy (\((x, y), _) ((u, v), _) -> compare x u) $ sortBy (\((x, y), _) ((u, v), _) -> compare y v) st


-- Display (uxv grid)
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

displayStrip :: State Int Bool -> String
displayStrip ((_, s):st) = (if s then "X" else " ") ++ displayStrip st
displayStrip [] = ""

playGame :: (Show a, Show b) => Game a b -> (State a b -> String) -> IO ()
playGame (s, m, f) sh = do
  putStrLn $ "State: " ++ "\n" ++  sh s
  putStrLn $ "Moves" ++ "\n" ++ (intercalate "\n\n\n\n" $ map sh (m s))
  move <- getLine
  s' <- return $ ((m s) !! (read move))
  case f s' of
        Win -> putStrLn (sh s') >> putStrLn "You win!"
        Loss -> putStrLn "You lose!"
        Stalemate -> putStrLn "Stalemate!"
        Continue -> playGame (s', m, f) sh
