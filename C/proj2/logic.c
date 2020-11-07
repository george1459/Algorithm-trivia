#include <stdlib.h>
#include <stdio.h>
#include "logic.h"
#include "board.h"

/* note on this n_oucome:
 * this special type recorded whether white has won (with white)
 * and whether black has won (with black) */
struct n_outcome {
	int white;
	int black;
};

typedef struct n_outcome n_outcome;

game* new_game(unsigned int side, enum type type) {
	game* res = (game*)malloc(sizeof(game));
	res -> b = board_new(side, type);
	res -> next = WHITE_NEXT;
        return res;
}

void game_free(game* g) {
	board_free(g -> b);
	free(g);
}


int place_marble(game* g, pos p) {
	if (g -> b -> type == BITS) {
		if (back(g -> b, p.r, p.c) != 0) {
			return 0;
		}
		else {
			if (g -> next == WHITE_NEXT) {
				set(g -> b, p.r, p.c, 2);
				return 1;
			}
			else {
				set(g -> b, p.r, p.c, 1);
				return 1;
			}
		}
	
	}
	else if (g -> b -> type == CELLS) {
		if (g -> b -> u.cells[p.r][p.c] != EMPTY) {
			return 0;
		}
		else {
			if (g -> next == WHITE_NEXT) {
				g -> b -> u.cells[p.r][p.c] = WHITE;
				return 1;
			}
			else {
				g -> b -> u.cells[p.r][p.c] = BLACK;
				return 1;
			}
		}
	}
	else {
		fprintf(stderr, "place_marble: incorrect type");
		exit(1);
	}
}

void twist_quadrant(game* g, quadrant q, direction d) {
	if (g -> b -> type == BITS) {
		fprintf(stderr, "twist_quadrant: BITS type requested");
		exit(1);
	}
	else if (g -> b -> type == CELLS) {
		unsigned int mid = (g -> b -> side) / 2;
		/* x_offset & y_offset represents the necessary differene between the desired quadrant in th original board and 0 */
		unsigned int x_offset = 0, y_offset = 0, side = mid;
		/* If (q == NW), then no change to either offset or the new side value*/
		if (q == NE) {
			x_offset = mid;
			side = (g -> b -> side) - mid;
		}
		else if (q == SW) {
			y_offset = mid;
			side = (g -> b -> side) - mid;
		}
		else if (q == SE) {
			x_offset = mid;
			y_offset = mid;
			side = (g -> b -> side) - mid;
		}
		square** res = (square**)malloc(sizeof(square*) * side);
		unsigned int i, j;
		for (i = 0; i < side; i ++) {
			res[i] = (square*)malloc(sizeof(square) * side);
			for (j = 0; j < side; j ++) {
				if (d == CCW) {
					res[i][j] = (g -> b -> u.cells)[j + y_offset][side - 1 - i + x_offset];
				}
				else {
					res[i][j] = (g -> b -> u.cells)[side - 1 - j + y_offset][i + x_offset];
				}
			}
		}
		for (i = 0; i < side; i ++) {
			for (j = 0; j < side; j ++) {
				(g -> b -> u.cells)[i + y_offset][j + x_offset] = res[i][j];
			}
		}
		/* Now freeing the newly allocated res */
		for (i = 0 ; i < side; i ++) {
			free(res[i]);
		}
		free(res);
		/* finally, making a change in turn*/ 
		if (g -> next == BLACK_NEXT) {
			g -> next = WHITE_NEXT;
		}
		else if (g -> next == WHITE_NEXT) {
			g -> next = BLACK_NEXT;
		}

	}
	else {
		fprintf(stderr, "twist_quadrant: incorrect type");
		exit(1);
	}
}

/* Now comes the most complicated functions in this code structure */

int search_hor(game* g) {
	unsigned int i, j, res = 0;
	unsigned int side = g -> b -> side;
	for (i = 0; i < side; i ++) {
		/* initialize previous to be the first element in this row */
		square previous = (g -> b -> u.cells)[i][0];
		for (j = 0; j < side; j ++) {
			square existing = (g -> b -> u.cells)[i][j];
			if ((existing == previous) && existing != EMPTY) {
				res ++;
			}
			else {
				res = 0;
			}
			previous = existing;
			if (res >= (side) - 1) {
				return 1;
			}
		}
		/* set the res value back to 0 again */
		res = 0;
	}
	return 0;
}

int search_ver(game* g) {
	unsigned int i, j, res = 0;
	unsigned int side = g -> b -> side;
	for (i = 0; i < side; i ++) {
		/* initialize previous to be the first element in this row */
		square previous = (g -> b -> u.cells)[0][i];
		for (j = 0; j < side; j ++) {
			square existing = (g -> b -> u.cells)[j][i];
			if ((existing == previous) && existing != EMPTY) {
				res ++;
			}
			else {
				res = 0;
			}
			previous = existing;
			if (res >= side - 1) {
				return 1;
			}
		}
		/* set the res value back to 0 again */
		res = 0;
	}
	return 0;
}

int search_dia_helper(game* g, int side_offset, int begin_x, int begin_y, int y_inc /* This final y_inc can be either -1 or 1*/) {
	unsigned int i, res = 0;
	unsigned int side = g -> b -> side;
	square previous = (g -> b -> u.cells)[begin_x][begin_y];
	for (i = 0; i < side - side_offset; i ++) {
		square existing = (g -> b -> u.cells)[begin_x][begin_y];
		if ((existing == previous) && existing != EMPTY) {
			res ++;
		}
		else {
			res = 0;
		}
		previous = existing;
		if (res >= (side - 1)) {
			return 1;
		}
		begin_x ++;
		begin_y = begin_y + (y_inc);
	}
	return 0;
}


int search_dia(game* g) {
	unsigned int side = g -> b -> side;
	if (
	/* finally search diagonally top-left to bottom-right, there are three lines to search */
	(search_dia_helper(g, 0, 0, 0, 1) == 1) || 
	(search_dia_helper(g, 1, 1, 0, 1) == 1) ||
	(search_dia_helper(g, 1, 0, 1, 1) == 1) ||
	/* Now search for the top-right to bottom-left lines */
	(search_dia_helper(g, 0, 0, side - 1, -1) == 1) ||
	(search_dia_helper(g, 1, 1, side - 1, -1) == 1) ||
	(search_dia_helper(g, 1, 0, side - 2, -1) == 1) ) {
		return 1;
	}
	return 0;
}


int game_over(game* g) {
	if ((search_hor(g) == 1) || (search_ver(g) == 1) || (search_dia(g) == 1)) {
		return 1;
	}
	else {
		return 0;
	}
}

/* The functions above only needs to worry about whether the game has finished or not,
 * they return as soon as they are able to make this decision.
 * the function below also needs to record the apperances of different squares.
 * There will be a "c_" attached to all following functions */

n_outcome c_search(game* g) {
	unsigned int i, j, res = 0;
	unsigned int side = g -> b -> side;
	n_outcome n_res; 
	n_res.white = 0;
	n_res.black = 0;
	/* first search horizontally */
	for (i = 0; i < side; i ++) {
		/* initialize previous to be the first element in this row */
		square previous = (g -> b -> u.cells)[i][0];
		for (j = 0; j < side; j ++) {
			square existing = (g -> b -> u.cells)[i][j];
			if ((existing == previous) && existing != EMPTY) {
				res ++;
			}
			else {
				res = 0;
			}
			previous = existing;
			if (res >= (side) - 1) {
				if (previous == WHITE) {
					n_res.white = 1;
					if (n_res.black == 1) {
						return n_res;
					}
				}
				else if (previous == BLACK) {
					n_res.black = 1;
					if (n_res.white == 1) {
						return n_res;
					}
				}
			}
		}
		/* set the res value back to 0 again */
		res = 0;
	}
	/* next search vertically */
	for (i = 0; i < side; i ++) {
		/* initialize previous to be the first element in this row */
		square previous = (g -> b -> u.cells)[0][i];
		for (j = 0; j < side; j ++) {
			square existing = (g -> b -> u.cells)[j][i];
			if ((existing == previous) && existing != EMPTY) {
				res ++;
			}
			else {
				res = 0;
			}
			previous = existing;
			if (res >= (side) - 1) {
				if (previous == WHITE) {
					n_res.white = 1;
					if (n_res.black == 1) {
						return n_res;
					}
				}
				else if (previous == BLACK) {
					n_res.black = 1;
					if (n_res.white == 1) {
						return n_res;
					}
				}
			}
		}
		/* set the res value back to 0 again */
		res = 0;
	}

	return n_res;
}

n_outcome c_search_dia_helper(game* g, int side_offset, int begin_x, int begin_y, int y_inc /* This final y_inc can be either -1 or 1*/) {
	unsigned int i, res = 0;
	unsigned int side = g -> b -> side;
	n_outcome n_res;
        n_res.white = 0;
	n_res.black = 0;
	square previous = (g -> b -> u.cells)[begin_x][begin_y];
	for (i = 0; i < side - side_offset; i ++) {
		square existing = (g -> b -> u.cells)[begin_x][begin_y];
		if ((existing == previous) && existing != EMPTY) {
			res ++;
		}
		else {
			res = 0;
		}
		previous = existing;
		if (res >= (side - 1)) {
			/* we don't have to do the double check here because one call can only generate one possible result*/
			if (previous == WHITE) {
				n_res.white = 1;
				return n_res;
			}
			else if (previous == BLACK) {
				n_res.black = 1;
				return n_res;
			}
		}
		begin_x ++;
		begin_y = begin_y + y_inc;
	}
	return n_res;
}

/* Notice it's clear that if one party has (side - 1) mabbles placed on a diagnol, 
 * It's impossible for the other party to have the same amount at the same time
 * However, for code simplicity, we simply add all results together */
outcome game_outcome(game* g) {
	unsigned int side = g -> b -> side;
	n_outcome n_res0 = c_search(g);
	n_outcome n_res1 = c_search_dia_helper(g, 0, 0, 0, 1);
	n_outcome n_res2 = c_search_dia_helper(g, 1, 1, 0, 1);
	n_outcome n_res3 = c_search_dia_helper(g, 1, 0, 1, 1);
	n_outcome n_res4 = c_search_dia_helper(g, 0, 0, side - 1, -1);
	n_outcome n_res5 = c_search_dia_helper(g, 1, 1, side - 1, -1);
	n_outcome n_res6 = c_search_dia_helper(g, 1, 0, side - 2, -1);
	n_outcome n_final;
	n_final.white = n_res0.white + n_res1.white + n_res2.white + n_res3.white + n_res4.white + n_res5.white + n_res6.white;
	n_final.black = n_res0.black + n_res1.black + n_res2.black + n_res3.black + n_res4.black + n_res5.black + n_res6.black;
	if (n_final.white > 0 && n_final.black == 0) {
		return WHITE_WIN;
	}
	if (n_final.black > 0 && n_final.white == 0) {
		return BLACK_WIN;
	}
	return DRAW;
}	
