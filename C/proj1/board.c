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
		fprintf(stderr, "board_new: incorrect type given");
		exit(1);
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
		fprintf(stderr, "board_free: incorrect type given");
		exit(1);
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


void board_show(board* b) {
	if (b -> type == BITS) {
		fprintf(stderr, "board_show: incorrect type given");
		exit(1);
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
		fprintf(stderr, "board_get: incorrect type given\n");
		exit(1);
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

void board_set(board* b, pos p, square s) {
	if (b -> type == BITS) {
		fprintf(stderr, "board_set: incorrect type given\n");
		exit(1);
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






