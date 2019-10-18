#include <iostream>
#include "game.hpp"

using namespace std;

Game::Game(int pt, int np, vector<Board> boards, Turn turn, vector<int (*) (vector<Board>)> winConditions, void (*printBoard) (vector<Board>)){ 
    this->playerTurn = pt;
    this->numberPlayers = np;
    this->boards = boards;
    this->turn = turn;
    this->winConditions = winConditions;
    this->printBoard = printBoard;
    this->winner = -1;
}

vector<Piece> Board::PiecesAt(int x, int y){
    vector<Piece> pieces;

    for(Piece p : this->pieces){
        if(p.x == x && p.y == y){
            pieces.push_back(p);
        }
    }

    return pieces;
}

Piece::Piece(){
}
Piece::Piece(int x, int y, int player){
    this->x = x;
    this->y = y;
    this->player = player;
}

void Board::PlacePiece(Piece p){
    this->pieces.push_back(p);
}

Action::Action(ActionMetadata metadata, bool (*action) (ActionMetadata, vector<Board> &)){
    this->metadata = metadata;
    this->action = action;
}

Phase::Phase(){
}
Phase::Phase(vector<Action> options){
    this->options = options;
}

Turn::Turn(){
}
Turn::Turn(vector<Phase> phases){
    this->phases = phases;
}

Parameter::Parameter(string name, ParameterStatus status, int value){
    this->name = name;
    this->status = status;
    this->value = value;
}
ActionMetadata::ActionMetadata(){
        
}
ActionMetadata::ActionMetadata(string name, vector<Parameter> parameters){
    this->name = name;
    this->parameters = parameters;
}

void ActionMetadata::UpdateParameter(string name, int value){
    for(Parameter p : this->parameters){
        if(p.name == name){
            p.value = value;
            return;
        }
    }
    this->parameters.push_back(Parameter(name, Defined, value));
}

Parameter ActionMetadata::GetParameter(string name){
    for(Parameter p : this->parameters){
        if(p.name == name){
            return p;
        }
    }
    return Parameter("ERROR", UserInput, 0);
}

int ActionMetadata::InterpretedGetParameter(string name){
    Parameter p = this->GetParameter(name);
    
    int x = p.value;

    if(p.status == UserInput){
        cout << "please enter " << name << ": ";
        cin >> x; 
    }

    return x;
}

void Game::updateGlobalParameters(ActionMetadata &metadata){
    metadata.UpdateParameter("current player", this->playerTurn);
}

Action::Action(){
    action = NULL;
}

void Game::gameloop(){
    while(this->winner == -1){
        //turn
        for(Phase p : this->turn.phases){
            //phase make choice
            Action pieceaction;
            bool found = false;
            while(!found){
                for(Action act : p.options){
                    cout << act.metadata.name << endl;
                }
                string name = "";
                cin >> name;
                for(Action act : p.options){
                    if(act.metadata.name == name){
                        pieceaction = act;
                        found = true;
                    }
                }
           
                
                if(found){
                    this->updateGlobalParameters(pieceaction.metadata);            

                    if(!(pieceaction.action(pieceaction.metadata, this->boards))){
                        found = false;
                        cout << "invalid move" << endl;
                    }
                }
            }
        }

        //wincheck
        for(int (*func) (vector<Board>) : this->winConditions){
            if(this->winner == -1){
                this->winner = func(this->boards);
            }
        }

        //print board state
        this->printBoard(this->boards);


        this->playerTurn = (this->playerTurn + 1) % this->numberPlayers;    
    }
}

Board::Board(string name, int width, int height){
    this->boardName = name;
    this->width = width;
    this->height = height;
}
