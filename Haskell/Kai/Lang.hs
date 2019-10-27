-- | my language

module TicTacToeTests.Haskell.Kai.Lang where
import Data.List
import qualified Text.Parsec as Parsec
import Text.ParserCombinators.Parsec
import Control.Applicative ((*>))
type GState a b = [(a, b)]

-- A piece is one of these ordered pairs
type Piece a b = (a, b)

-- A game is always in one of these states:
data Status = Win | Loss | Stalemate | Continue | Invalid | Ended
  deriving Show

data GameStmt = Seq GameStmt GameStmt | DoMove MoveStmt | Loop GameStmt | CheckStatus PredStmt Status | Rule [PredStmt] GameStmt

data MoveStmt = AddP Char

data PredStmt = InARow Int | SameSpot (Int, Int) | Forever | NoSameSpot | BoardSize Int Int

type GridState = [((Int,Int), Char)]

parseSeq :: Parser (GameStmt -> GameStmt -> GameStmt)
parseSeq = do
  optional spaces
  (char ';')
  optional spaces
  optional $ string "\n"
  return Seq

gameParse :: Parser GameStmt
gameParse =      gameParse' `chainl1` (parseSeq)
gameParse' :: Parser GameStmt
gameParse' = do
  try $ string "place"
  spaces
  move <- moveParse
  return $ DoMove move
  <|>
  do
        try $ string "loop"
        spaces
        game <- gameParse
        optional $ char '\n'
        return $ Loop game
  <|>
  do
        try $ string "check"
        spaces
        predicate <- parsePred
        spaces
        status <- parseStatus
        optional $ char '\n'
        return $ CheckStatus predicate status
  <|>
  do
    try $ string "rules:"
    spaces
    rs <- many1 parsePred
    spaces
    string "on"
    spaces
    optional $ char '\n'
    game <- gameParse
    return $ Rule rs game

parseStatus =
  (string "win" *> pure Win) <|> (string "lose" *> pure Loss)
parsePred = do
  string "samespot"
  spaces
  return $ NoSameSpot
  <|>
  do
        string "boardsize"
        spaces
        m <- many1 digit
        spaces
        n <- many1 digit
        optional spaces
        return $ BoardSize (read m) (read n)
  <|>
  do
    string "row"
    spaces
    i <- many1 digit
    optional spaces
    return $ InARow (read i)

moveParse = do
  c <- anyChar
  return $ AddP c

strtic = "rules: \
         \boardsize 3 3 \
         \samespot \
         \on \
         \loop \
         \place x; \
         \place y"

tictactoe = Rule [NoSameSpot, BoardSize 3 3]
    (Loop (Seq
          (DoMove (AddP 'X'))
           (Seq
            (CheckStatus (InARow 3) (Win))
             (Seq
              (DoMove (AddP 'O'))
              (CheckStatus (InARow 3) (Win))))))


evalMove (AddP c) st = \(x,y) -> ((x,y), c):st
-- Should use monad tranformers, yikes (Either would be the main one. Non "Continue" statues fail upwards.)
evalStmts :: [GameStmt] -> (GridState, [PredStmt], Status) -> IO (GridState, [PredStmt], Status)
evalStmts (s:ss) st = evalGame s st >>= evalStmts ss
evalStmts [] st = return (st)


evalGame :: GameStmt -> (GridState, [PredStmt], Status) -> IO (GridState, [PredStmt], Status)
evalGame _ (_, _, Ended) = return ([], [], Ended)
evalGame _ (_, _, Win) = (putStrLn "You Win!") >> return ([], [], Ended)

evalGame h@(Loop gs) st = evalGame gs st >>= evalGame h

evalGame (Seq s1 s2) st = evalGame s1 st >>= evalGame s2

evalGame (CheckStatus p s) (st, r, s') = if (evalPred p st) then  return $ ([], [], s) else return $ (st, r, s')
evalGame (Rule ps gs) (st, r, s) = evalGame gs (st, r ++ ps, s)

evalGame (DoMove m) (st, r, s') = do
  putStrLn $ printGrid (3,3) (sortGrid' st)
  line <- getLine
  p <- return $ (read line :: (Int, Int))
  st' <- return $ evalMove m st p
  if allTrue st' (map evalPred r) then return $ (st', r, s')
  else (putStrLn "You can't do that!") >> evalGame (DoMove m) (st, r, s')

play game = evalGame game ([], [], Continue)
playstr prog = case parse gameParse "" prog of
  Left x -> putStrLn $ "?"  ++ show x
  Right x -> play x >> return ()


evalPred (InARow i) = \st -> inARow st st (1,0) i || inARow st st (1,1) i ||  inARow st st (0,1) i
evalPred (Forever) = const False
evalPred (NoSameSpot) = boardOK
evalPred (BoardSize n m) = not . notOffBoardAny n m
-- mnk in a row games

-- check for same index. should just use `on` instead of this function
sameIndex :: Eq a => Piece a b -> Piece a b -> Bool
sameIndex (i, _) (i', _) = i == i'

-- check for same spot
sameSpot :: Piece (Int, Int) b -> Piece (Int, Int) b -> Bool
sameSpot ((x, y), _) ((u, v), _) = x == u && y == v

-- Pieces can't be off board.
offBoard m n ((x, y), _) = x > m && y > n && x > 0 && y > 0
notOffBoardAny m n st = any (offBoard m n) st

-- No two pieces in the same spot.
boardOK :: GState (Int, Int) Char -> Bool
boardOK (p:st) = foldl (&&) True (map (not . sameSpot p) st) && boardOK st
boardOK [] = True

-- Pointwise add two 2-tuples
pwadd (x, y) (z, w) = (x + z, y + w)
countPiece :: GState (Int, Int) Char -> (Int, Int)
countPiece ((_, c):st) = pwadd (if c == 'X' then (1, 0) else (0, 1)) $ countPiece st
countPiece [] = (0, 0)

-- inARow :: State, Direction, Number in a row desired. Returns True or False. Can simplify the function a little if you sort first.
inARow :: GState (Int, Int) Char -> GState (Int, Int) Char -> (Int, Int) -> Int -> Bool
inARow state (s:st) d n = (inARow' state s d n) || inARow state st d n
  where
    inARow' :: GState (Int, Int) Char -> Piece (Int, Int) Char -> (Int, Int) -> Int -> Bool
    inARow' _ _ _ 1 = True
    inARow' st ((x, y), c) (dx, dy) n = case lookup (x+dx, y+dy) st of
      Nothing -> False
      Just c' -> if c == c' then inARow' state ((x+dx, y+dy), c) (dx, dy) (n-1) else False
inARow _ _ _ _ = False

printGrid :: Show b => (Int, Int) -> GState (Int, Int) b -> String
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

sortGrid :: GState (Int, Int) b -> GState (Int, Int) b
sortGrid st = sortBy (\((x, y), _) ((u, v), _) -> compare y v) $ sortBy (\((x, y), _) ((u, v), _) -> compare x u) st
-- transposed
sortGrid' st = sortBy (\((x, y), _) ((u, v), _) -> compare x u) $ sortBy (\((x, y), _) ((u, v), _) -> compare y v) st

allTrue :: a -> [(a -> Bool)] -> Bool
allTrue x (p:ps) = (p x) && allTrue x ps
allTrue x [] = True
-- all ($ x) y if u wanna be cool.

playFile :: String -> IO ()
playFile file = do
  code <- readFile file
  playstr code
  return ()
