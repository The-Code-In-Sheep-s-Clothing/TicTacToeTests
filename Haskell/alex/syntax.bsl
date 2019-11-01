game tic-tac-toe
    piece stone 
        move-rules: none
        attack-rules: none 
        replacement-rules: environment

    player Bob 
        pieces: unlimited stone "x"

    player Alice 
        pieces: unlimited stone "o" 

    victory-rules: adjacent-3 
    stalemate-rules: full 

    moves: unlimited Bob, Alice 

    board: width 3, height 3 

game simpleChess 
    piece king
        move-rules: all-1 
        attack-rules: same 
        replacement-rules: environment
        
    piece pawn
        move-rules: forward-1
        attack-rules: lfdiagonal-1, rfdiagonal-2
        replacement-rules: environment
    
    player Bob
        pieces: 1 king "kb", 8 pawn "pb" // for now the display is the literal string, in the future maybe it is the name of a resource?  
        
    player Alice 
        pieces: 1 king "ka", 8 pawn "pa"
        
    victory-rules: threat-king
    
    moves: unlimited Bob, Alice 
    
    board: width 8, height 8
    
    arrange: Bob [d1, a2, b2, c2, d2, e2, f2, g2, h2], // need a less error-prone way to select a player's pieces 
             Alice [d8, a7, b7, c7, d7, e7, f7, h7] 
                  
