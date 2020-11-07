#ifndef _BOARD_H
#define _BOARD_H

#include "pos.h"


enum square {
	    EMPTY,
	        BLACK,
		    WHITE
};

typedef enum square square;


union board_rep {
	    enum square** cells;
	        unsigned int* bits;
};

typedef union board_rep board_rep;

enum type {
	    CELLS, BITS
};


struct board {
	    unsigned int side;
	        enum type type;
		    board_rep u;
};

typedef struct board board;

/* Makes a new epty board using the given type and the given side */
board* board_new(unsigned int side, enum type type);

/* free an existing board completely */
void board_free(board* b);

/* Show a given board using the specific criteria on Piazza */
void board_show(board* b);

/* get and return the square representation at the specified location */
square board_get(board* b, pos p);

/* set the square representation at the sepcified location */
void board_set(board* b, pos p, square s);

/* This sets the desired update at the desired location in board, using BITS representation */
void set(board* b, unsigned int i, unsigned int j, unsigned int update);

/* This returns the BITS representation of empty, white, or black fulfillment at a slot */
char back(board* b, unsigned int i, unsigned int j);

#endif /* _BOARD_H */
