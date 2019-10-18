#include <vector>
#include <string>
#include <tuple>

using namespace std;

class Piece{
    private:
    public: 
        Piece();
        Piece(int x, int y, int player);
        int x;
        int y;
        int player;
};

class Board{
    private:
    public:
        Board(string name, int width, int height);
        string boardName;
        int width;
        int height;
        vector<Piece> pieces;
        vector<Piece> PiecesAt(int x, int y);
        void PlacePiece(Piece);
};

enum ParameterStatus {Defined = 0, UserInput = 1};
class Parameter{
    private:
    public:
        Parameter(string, ParameterStatus, int);
        string name;
        ParameterStatus status;
        int value;
};

class ActionMetadata{
    private:
    public:
        ActionMetadata();
        ActionMetadata(string, vector<Parameter>);
        string name;
        vector<Parameter> parameters;
        void UpdateParameter(string name, int value);
        Parameter GetParameter(string name);
        int InterpretedGetParameter(string);
};

class Action{
    private:
    public:
        Action();
        Action(ActionMetadata, bool (*) (ActionMetadata, vector<Board> &));
        ActionMetadata metadata;
        bool(*action) (ActionMetadata, vector<Board> &);
};

class Phase{
    private:
    public:
        Phase();
        Phase(vector<Action>);
        vector<Action> options;      
};

class Turn{
    private:
    public:
        Turn();
        Turn(vector<Phase>);
        vector<Phase> phases;
};

class Game{
    private:
        void updateGlobalParameters(ActionMetadata &);
    public:
        Game(int pt, int np, vector<Board> boards, Turn turn, vector<int (*) (vector<Board>)> winConditions, void (*printBoard) (vector<Board>));
        int playerTurn;
        int numberPlayers;
        int winner;
        vector<Board> boards;
        Turn turn;
        vector<int (*) (vector<Board>)> winConditions;
        void (*printBoard) (vector<Board>);


        void gameloop();
};
