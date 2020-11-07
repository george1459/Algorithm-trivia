# include <stdio.h>
# include <stdlib.h>
# include "board.h"
# include "pos.h"

board* board_new(unsigned int side, enum type type) {
	if (side % 2 != 0) {
		fprintf(stderr, "board_new: side length not an even number");
		exit(1);
	}
	if (type == BITS) {
		board* res = (board*)malloc(sizeof(board));
		res -> side = side;
		res -> type = type;
		unsigned int length = (side * side % 16 == 0) ? (side * side / 16) : (side * side / 16 + 1);
		unsigned int i;
		unsigned int* bits = (unsigned int*)malloc(sizeof(unsigned int) * length);
		if (side * side % 16 == 0) {
			for (i = 0; i < length; i ++) {
				bits[i] = 0 << 31;
			}
		}
		else {
			for (i = 0; i < length - 1; i ++) {
				bits[i] = 0 << 31;
			}
			bits[i] = 0 << ((side * side - (length - 1) * 16) / 2 - 1);
		}
		res -> u.bits = bits;
		return res;
	}
	else if (type == CELLS) {
		board* res = (board*)malloc(sizeof(board));
		res -> side = side;
		res -> type = type;
		square** cells = (square**)malloc(sizeof(square*) * side);
		unsigned int i, j;
		for (i = 0; i < side; i ++) {
			cells[i] = (square*)malloc(sizeof(square) * side);
			for (j = 0; j < side; j ++) {
				cells[i][j] = EMPTY;
			}
		}
		res -> u.cells = cells;
		return res;
	}
	else {
		fprintf(stderr, "board_new: incorrect type given");
		exit(1);

	}
}

void board_free(board* b) {
	if (b -> type == BITS) {
		free(b -> u.bits);
		free(b);
	}
	else if (b -> type == CELLS) {
		unsigned int i;
		for (i = 0; i < b -> side; i ++) {
			free(b -> u.cells[i]);
		}
		free(b -> u.cells);
		free(b);
	}
	else {
		fprintf(stderr, "board_free: incorrect type given");
		exit(1);
	}
}

void print_horizontal_index(unsigned int side) {
	unsigned int i;
	for (i = 0; i < side; i ++) {
		if (i == side / 2) {
			printf(" ");
		}
		if (i < 10) {
			printf("%d", i);
		}
		else if (i < 36) {
			char s = 'A' + (i - 10);
			printf("%c", s);
		}
		else if (i < 62) {
			char s = 'a' + (i - 36);
			printf("%c", s);
		}
		else {
			printf("?");
		}
	}
}

void print_vertical_index(unsigned int i, unsigned int side) {
	if (i == side / 2) {
		printf("\n");
	}
	if (i < 10) {
		printf("%d", i);
	}
	else if (i < 36) {
		char s = 'A' + (i - 10);
		printf("%c", s);
	}
	else if (i < 62) {
		char s = 'a' + (i - 36);
		printf("%c", s);
	}
	else {
		printf("?");
	}
}

/* This function take sin a board, a row number i and a column number j, it returns 0, 1, 2 according to 00, 01, or 10 */
char back(board* b, unsigned int i, unsigned int j) {
	unsigned int side = b -> side;
	unsigned int current = (i * side * 2 + j * 2) / 32;
	unsigned int retrieve = (i * side * 2 + j * 2) - current * 32;
	return (((b -> u.bits)[current]) & ((3 /* representing 11 */ << retrieve) + (0 << 31))) >> retrieve;
}

void board_show(board* b) {
	if (b -> type == BITS) {
		printf("  ");
		print_horizontal_index(b -> side);
		printf("\n\n");
		/* The first two line has been completely printed */
		unsigned int i, j;
		for (i = 0; i < b -> side; i ++) {
			print_vertical_index(i, b -> side);
			printf(" ");
			/* Now loop over the current row */
			for (j = 0; j < b -> side; j ++) {
				if (j ==  (b -> side) / 2) {
					printf(" ");
				}
				char n = back(b, i, j);
				if (n == 0) {
					printf(".");
				}
				else if (n == 1) {
					printf("*");
				}
				else if (n == 2) {
					printf("o");
				}	
			}
		printf("\n");
		}
	}
	else if (b -> type == CELLS) {
		printf("  ");
		print_horizontal_index(b -> side);
		printf("\n\n");
		/* The first two line has been completely printed */
		unsigned int i, j;
		for (i = 0; i < b -> side; i ++) {
			print_vertical_index(i, b -> side);
			printf(" ");
			/* Now loop over the current row */
			for (j = 0; j < b -> side; j ++) {
				if (j ==  (b -> side) / 2) {
					printf(" ");
				}
				if ((b -> u.cells)[i][j] == EMPTY) {
					printf(".");
				}
				else if ((b -> u.cells)[i][j] == BLACK) {
					printf("*");
				}
				else if ((b -> u.cells)[i][j] == WHITE) {
					printf("o");
				}
			}
		printf("\n");
		}
	}
	else {
		fprintf(stderr, "board_show: incorrect type given\n");
		exit(1);
	}
}

square board_get(board* b, pos p) {
	if (b -> type == BITS) {
		if ((p.r > b -> side) || (p.c > b -> side)) {
			fprintf(stderr, "board_get: given position exceeds matrix dimention\n");
			exit(1);
		}
		char n = back(b, p.r, p.c);
		if (n == 0) {
			return EMPTY;
		}
		else if (n == 1) {
			return BLACK;
		}
		else {
			return WHITE;
		}	
	}
	else if (b -> type == CELLS) {
		if ((p.r > b -> side) || (p.c > b -> side)) {
			fprintf(stderr, "board_get: given position exceeds matrix dimention\n");
			exit(1);
		}
		return b -> u.cells[p.r][p.c];
	}
	else {
		fprintf(stderr, "board_get: incorrect type given\n");
		exit(1);
	}
}

/* This function takes in a board, a row number i and a column number j, and a number representing 00, 01, or 10
It then updates the board by placing the marble at the specified location */
void set(board* b, unsigned int i, unsigned int j, unsigned int update) {
	unsigned int side = b -> side;
	unsigned int current = (i * side * 2 + j * 2) / 32;
	unsigned int retrieve = (i * side * 2 + j * 2) - current * 32;
	(b -> u.bits)[current] = ((b -> u.bits)[current]) | (update << retrieve);
}


void board_set(board* b, pos p, square s) {
	if (b -> type == BITS) {
		if ((p.r > b -> side) || (p.c > b -> side)) {
			fprintf(stderr, "board_set: given position exceeds matrix dimention\n");
			exit(1);
		}
		if (s == EMPTY) {
			set(b, p.r, p.c, 0);
		}
		else if (s == BLACK) {
			set(b, p.r, p.c, 1);
		}
		else {
			set(b, p.r, p.c, 2);
		}
	}
	else if (b -> type == CELLS) {
		if ((p.r > b -> side) || (p.c > b -> side)) {
			fprintf(stderr, "board_set: given position exceeds matrix dimention\n");
			exit(1);
		}
		b -> u.cells[p.r][p.c] = s;
	}
	else {
		fprintf(stderr, "board_set: incorrect type given\n");
		exit(1);
	}
}






