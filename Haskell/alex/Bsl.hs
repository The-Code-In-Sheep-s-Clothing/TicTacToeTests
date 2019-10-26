module Bsl where 

-- configurable tic-tac-toe where players can overwrite previously-placed pieces 
-- I'd like to specify not overwriting as a user-supplied rule 
-- but I haven't fleshed out how rules work for games   

import Data.List 
import Data.List.Split 

-- an ID and a list of characters which represents a user's playable pieces 
data Player = Player Int String
   deriving (Eq)

data Tile = Tile TileType Char Location 

instance Show Tile where 
   show (Tile _ icon _) = ' ' : icon : " " 

data TileType = Occupied Int | Free
   deriving (Eq) 
            
type Tiles = [Tile]

-- 0-indexed location 
type Location = (Int, Int) 

type Width = Int 
type Height = Int 
 
data Board = Board Tiles Width Height  

data Game = Game Board [Player] Status 

-- Invalid is not actually used yet 
data Status = Continue | Stalemate | Victory Player
   deriving (Eq)
     
instance Show Board where
   show (Board t w h) = unlines $ map concat (intersperse [replicate (w * 4) '-'] $ map (intersperse "|") $ chunksOf w (map show t)) 

-- gets the piece number from its 0-indexed coordinates
-- assumes valid indices for the board 
pieceIndex :: Location -> Width -> Int 
pieceIndex (x,y) w = (y * w) + x  

-- unsafe replace, needs to be bounds-checked before use 
replace :: Tile -> Board -> Tiles 
replace t@(Tile _ _ loc@(x, y)) (Board tiles w h) = l ++ t : r where 
                    (l, _:r) = splitAt (pieceIndex loc w) tiles

getStatus :: Int -> Tiles -> Player -> Status 
getStatus i t p@(Player id _) = if (maximum [rowCount, colCount, diagCount]) >= i then Victory p else if not hasFreeTiles then Stalemate else Continue   
   where 
      playersTiles = getSameTiles (Occupied id) t 
      rowCount = countRowAdjacent playersTiles 1  
      colCount = countColAdjacent playersTiles 1
      diagCount = countDiagAdjacent playersTiles 1
      hasFreeTiles = any (sameType Free) t  

sameType :: TileType -> Tile -> Bool 
sameType t (Tile t' _ _) = t == t' 

getSameTiles :: TileType -> Tiles -> Tiles 
getSameTiles t ts = filter (sameType t) ts 

-- A function which determines if two points are related in some way (in a row, diagonally, every other in a col, etc)
type DirectionRelation = (Int, Int) -> (Int, Int) -> Bool 

-- counts adjacent tiles by only considering what is ahead in the list 
-- assumes list contains only one player's pieces and that the list is in row order  
countAdjacent :: Tiles -> Int -> DirectionRelation -> Int 
countAdjacent [] c _ = c 
countAdjacent (_:[]) c _ = c 
countAdjacent ((Tile _ _ (x, y)):(t@(Tile _ _ (x', y'))):ts) c f = if f (x, y) (x', y') then countAdjacent (t:ts) (c + 1) f else max (countAdjacent (t:ts) 1 f) c   

inARow, inACol, inADiag :: DirectionRelation 
inARow (x, y) (x', y') = (y' == y) && (x' - x == 1)
inACol (x, y) (x', y') = (x' == x) && (y' - y == 1) 
inADiag (x, y) (x', y') = (y' - y == 1) && (x' - x == 1) 

countRowAdjacent tiles count = countAdjacent tiles count inARow 
countColAdjacent tiles count = countAdjacent tiles count inACol
countDiagAdjacent tiles count = countAdjacent tiles count inADiag 

p1, p2 :: Player 
p1 = Player 1 "x"
p2 = Player 2 "o" 

-- a smart constructor for the empty board 
emptyBoard :: Width -> Height -> Board 
emptyBoard w h = Board (map (Tile Free ' ') empties) w h  
   where empties = [(x, y) | x <- [0..w-1], y <- [0..h-1]] 

prompt s = do 
   putStr s 
   line <- getLine 
   return line

play :: Game -> IO ()  
play (Game b@(Board t w h) players@(Player id icons : ps) s) = do 
   putStrLn $ "\n" ++ show b
   case s of 
      Continue -> do    
         userX <- prompt $ "Player " ++ show id ++ " x: " 
         userY <- prompt $ "Player " ++ show id ++ " y: " 
         putStrLn "" 
         let x = read userX :: Int 
         let y = read userY :: Int 

         if x < w && x >= 0 && y < h && y >= 0 then do  
            let newBoard = (replace (Tile (Occupied id) (head icons) (x, y)) b)   
            let playersTiles = getSameTiles (Occupied id) newBoard
            let rowCount = countRowAdjacent playersTiles 1  
            let colCount = countColAdjacent playersTiles 1
            let diagCount = countDiagAdjacent playersTiles 1 
            putStr $ "Max row: " ++ show rowCount ++ " | max col: " ++ show colCount ++ " | max diagonal: " ++ show diagCount
            play (Game (Board newBoard w h) (tail players) (getStatus 3 newBoard (head players)))
         else do 
            putStrLn "Out of bounds" 
            play (Game b players s)
      Victory (Player w _) -> putStrLn $ "Player " ++ show w ++ " wins" 
      otherwise -> putStrLn "Stalemate"
         
main = do
   play (Game (emptyBoard 4 4) (cycle [p1, p2]) Continue)
