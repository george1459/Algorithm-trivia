#ifndef _LOGIC_H
#define _LOGIC_H

#include "board.h"


enum turn {
	BLACK_NEXT,
	WHITE_NEXT
};

typedef enum turn turn;


enum outcome {
	BLACK_WIN,
	WHITE_WIN,
	DRAW
};

typedef enum outcome outcome;


enum quadrant {
	NW, NE, SW, SE
};

typedef enum quadrant quadrant;

enum direction {
	CW, CCW
};

typedef enum direction direction;


struct game {
	board* b;
	turn next;
};

typedef struct game game;

/* initialize a new game of a given side and type */
game* new_game(unsigned int side, enum type type);

/* free a game entirely */
void game_free(game* g);

/* place a marble at a given position at a given game, do not flip the turn after placing a marble */
int place_marble(game* g, pos p);

/* twist a specific quadrant of a given game, it can be either twisted CW or CCW, after the twist, flip the turn */
void twist_quadrant(game* g, quadrant q, direction d);

/* return 1 if the game is over according the rules */
int game_over(game* g);

/* determing if white wins, black wins, or it is a draw */
outcome game_outcome(game* g);

#endif /* _LOGIC_H */
