#include <iostream>
#include <algorithm>
#include "game.hpp"

using namespace std;

void printBoard(vector<Board> boards){
    for(Board b : boards){
        cout << b.boardName << endl;
        for(int x = 0; x < b.width; x++){
            for(int y = 0; y < b.height; y++){
                char toprint = '-';
                for(Piece p : b.pieces){
                    if(p.x == x && p.y == y){
                        toprint = p.player + '0';
                        break;
                    }
                }
                cout << toprint;
            }
            cout << endl;
        }
    }
}

bool move(ActionMetadata parameters, vector<Board> &boardstate){
    int currentPlayer = parameters.InterpretedGetParameter("current player");
    int x = parameters.InterpretedGetParameter("x");
    int y = parameters.InterpretedGetParameter("y");
    
    if(x >= 0 && x < 3 && y >= 0 && y < 3 && boardstate[0].PiecesAt(x, y).empty()){
        boardstate[0].PlacePiece(Piece(x, y, currentPlayer));
        return true;
    }

    return false;
}

int win(vector<Board> boardstate){
    Board b = boardstate[0];
    for(Piece p : b.pieces){
        int x = p.x;
        int y = p.y;
        int player = p.player;

        if(!(b.PiecesAt(x+1, y).empty()) && !(b.PiecesAt(x+2, y).empty()) &&
            b.PiecesAt(x+1, y)[0].player == player && b.PiecesAt(x+2,y)[0].player == player){
            return player;
        }
        if(!(b.PiecesAt(x, y+1).empty()) && !(b.PiecesAt(x, y+2).empty()) &&
            b.PiecesAt(x, y+1)[0].player == player && b.PiecesAt(x,y+2)[0].player == player){
            return player;
        }
        if(!(b.PiecesAt(x+1, y+1).empty()) && !(b.PiecesAt(x+2, y+2).empty()) &&
            b.PiecesAt(x+1, y+1)[0].player == player && b.PiecesAt(x+2,y+2)[0].player == player){
            return player;
        }
    }

    return -1;
}


int main(){
    Action moveaction(ActionMetadata("move", {Parameter("x", UserInput, 0), Parameter("y", UserInput, 0)}), move);
    Phase p({moveaction});
    Turn t({p});
    Game g(0, 2, {Board("Tic Tac Toe", 3, 3)}, t, {win}, printBoard);
    
    g.gameloop();
}
