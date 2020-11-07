# include <stdlib.h>
# include <stdio.h>
# include "board.h"
# include "pos.h"
# include "logic.h"


void pos_show(pos a) {
	printf("(%u,", a.r);
	printf("%u)\n", a.c);
}

void evidence_make_pos() {
	printf("***testing make_pos\n");
	printf("expecting (4,10): ");
	pos_show(make_pos(4, 10));
}

void evidence_board_new() {
	unsigned int a = 4;
	printf("\n\n***testing board_new\n");
	printf("expecting an empty board with side with 4 \n");
	board* b = board_new(a, BITS);
	board_show(b);
	board_free(b);

	a = 80;
	printf("\n\nexpecting an empty board with side with 80 \n");
	b = board_new(a, BITS);
	board_show(b);
	board_free(b);
}

void show_square(square a) {
	if (a == EMPTY) {
		printf("EMPTY\n");
	}
	else if (a == BLACK) {
		printf("BLACK\n");
	}
	else if (a == WHITE) {
		printf("WHITE\n");
	}
}

void evidence_board_get() {
	printf("\n\n***testing board_get\n");
	printf("expecting EMPTY:");
	board* a = board_new(4, BITS);
	show_square(board_get(a, make_pos(1,2)));
	board_free(a);

	a = board_new(10, BITS);
	board_set(a, make_pos(2, 4), BLACK);
	board_set(a, make_pos(2, 5), BLACK);
	board_set(a, make_pos(2, 6), WHITE);
	board_set(a, make_pos(2, 7), BLACK);
	printf("at (2,4): ");
	show_square(board_get(a, make_pos(2, 4)));
	printf("at (2,5): ");
	show_square(board_get(a, make_pos(2, 5)));
	printf("at (2,6): ");
	show_square(board_get(a, make_pos(2, 6)));
	printf("at (2,7): ");
	show_square(board_get(a, make_pos(2, 7)));
	board_show(a);
	board_free(a);
}


void evidence_board_set() {
	printf("\n\n***testing board_set\n");
	printf("expecting a 10*10 board with (3,4) set to be black and (9, 7) set to be white:\n");
	board* a = board_new(10, BITS);
	board_set(a, make_pos(3, 4), BLACK);
	board_set(a, make_pos(9, 7), WHITE);
	board_show(a);
	board_free(a);
}

void evidence_new_game() {
	printf("\n\n***testing new_game\n");
	game* g = new_game(6, BITS);
	printf("-expecting a game of a board of parameter 6:\n");
	board_show(g -> b);
	printf("\n expecting to print 1 representing white_next:\n");
	if (g -> next == WHITE_NEXT) {
	       printf("1\n");
	}
	game_free(g);
}

void evidence_place_marble() {
	printf("\n\n***testing place_marble\n");
	game* g = new_game(6, BITS);
	printf("-expecting a game of a board of parameter 6, a white marble will be placed at (3,4):\n");
	int a = place_marble(g, make_pos(3,4));
	board_show(g -> b);
	printf("\n-expecting to print 1 representing white_next:\n");
	if (g -> next == WHITE_NEXT) {
	       printf("1\n");
	}
	printf("\nexpecting the output below to be 1 to represent correct placement:\n");
	printf("%d", a);
	printf("\n-now attempting to place a marble at the same place, expecting the output below to be zerp:\n");
	printf("%d", place_marble(g, make_pos(3,4)));
	game_free(g);
}

void evidence_twist_quadrant() {
	printf("\n\n***testing twist_quadrant\n");
	game* g = new_game(6, BITS);
	printf("-expecting a game of a board of parameter 6, a white marble will be placed at (3,4):\n");
	place_marble(g, make_pos(3,4));
	printf("-and now twist the SE quadrant CW:\n");
	twist_quadrant(g, SE, CW);
	board_show(g -> b);
	printf("\n-expecting to print 0 representing black_next:\n");
	if (g -> next == BLACK_NEXT) {
	       printf("0\n");
	}

	printf("\n-and now place a black marble at (0,1) and (1,2) and a white marble at (2,2): \n");
	place_marble(g, make_pos(0, 1));
	place_marble(g, make_pos(1, 2));
	g -> next = WHITE_NEXT;
	place_marble(g, make_pos(2, 2));
	g -> next = BLACK_NEXT;
	board_show(g -> b);
	printf("-and now twist the NW quadrant CCW;\n");
	twist_quadrant(g, NW, CCW);
	board_show(g -> b);
	printf("\n expecting to print 1 representing white_next:\n");
	if (g -> next == WHITE_NEXT) {
	       printf("1\n");
	}

	g = new_game(8, CELLS);
	printf("\n-Now make a new board of size 8, place white marbles at (0,7); (1,6); (3,6); (2,5); and (1,7) and black marbles at (7,1); (7,3); and (5,0): \n");
	place_marble(g, make_pos(0, 7));
	place_marble(g, make_pos(1, 6));
	place_marble(g, make_pos(3, 6));
	place_marble(g, make_pos(2, 5));
	place_marble(g, make_pos(1, 7));
	g -> next = BLACK_NEXT;
	place_marble(g, make_pos(7, 1));
	place_marble(g, make_pos(7, 3));
	place_marble(g, make_pos(5, 0));
	g -> next = WHITE_NEXT;
	board_show(g -> b);
	printf("-and now twist the SW quadrant CCW;\n");
	twist_quadrant(g, SW, CCW);
	board_show(g -> b);
	printf("\n expecting to print 0 representing black_next:\n");
	if (g -> next == BLACK_NEXT) {
	       printf("0\n");
	}
	printf("-and now twist the NE quadrant CW;\n");
	twist_quadrant(g, NE, CW);
	board_show(g -> b);
	printf("\n expecting to print 1 representing white_next:\n");
	if (g -> next == WHITE_NEXT) {
	       printf("1\n");
	}
	game_free(g);
}

void evidence_game_over() {
	printf("\n\n***testing game_over\n");
	game* g = new_game(6, CELLS);
	place_marble(g, make_pos(3,0));
	place_marble(g, make_pos(3,1));
	place_marble(g, make_pos(3,2));
	place_marble(g, make_pos(3,3));
	place_marble(g, make_pos(3,4));
	board_show(g -> b);
	printf("\n-expecting to print 1 representing that game above has over:\n");
	printf("%d\n", game_over(g));
	game_free(g);

	g = new_game(10, CELLS);
	place_marble(g, make_pos(3,0));
	place_marble(g, make_pos(3,1));
	place_marble(g, make_pos(3,2));
	place_marble(g, make_pos(3,3));
	place_marble(g, make_pos(3,4));
	board_show(g -> b);
	printf("\n-expecting to print 0 representing that game above has not over:\n");
	printf("%d\n", game_over(g));
	game_free(g);

	g = new_game(6, CELLS);
	place_marble(g, make_pos(3,2));
	place_marble(g, make_pos(1,2));
	place_marble(g, make_pos(4,2));
	place_marble(g, make_pos(0,2));
	place_marble(g, make_pos(2,2));
	board_show(g -> b);
	printf("\n-expecting to print 1 representing that game above has over:\n");
	printf("%d\n", game_over(g));
	game_free(g);

	g = new_game(6, CELLS);
	place_marble(g, make_pos(0,1));
	place_marble(g, make_pos(1,2));
	place_marble(g, make_pos(2,3));
	place_marble(g, make_pos(3,4));
	place_marble(g, make_pos(4,5));
	board_show(g -> b);
	printf("\n-expecting to print 1 representing that game above has over:\n");
	printf("%d\n", game_over(g));
	game_free(g);

	g = new_game(6, CELLS);
	place_marble(g, make_pos(1,5));
	place_marble(g, make_pos(2,4));
	place_marble(g, make_pos(3,3));
	place_marble(g, make_pos(4,2));
	place_marble(g, make_pos(5,1));
	board_show(g -> b);
	printf("\n-expecting to print 1 representing that game above has over:\n");
	printf("%d\n", game_over(g));
	game_free(g);

	g = new_game(6, CELLS);
	place_marble(g, make_pos(1,5));
	place_marble(g, make_pos(2,4));
	place_marble(g, make_pos(3,3));
	place_marble(g, make_pos(4,2));
	place_marble(g, make_pos(5,1));
	g -> next = BLACK_NEXT;
	place_marble(g, make_pos(0,0));
	place_marble(g, make_pos(0,1));
	place_marble(g, make_pos(0,2));
	place_marble(g, make_pos(0,3));
	place_marble(g, make_pos(0,4));
	place_marble(g, make_pos(5,5));
	board_show(g -> b);
	printf("\n-expecting to print 1 representing that game above has over:\n");
	printf("%d\n", game_over(g));
	game_free(g);

	g = new_game(8, CELLS);
	place_marble(g, make_pos(1,5));
	place_marble(g, make_pos(2,4));
	place_marble(g, make_pos(3,3));
	place_marble(g, make_pos(4,2));
	place_marble(g, make_pos(5,1));
	g -> next = BLACK_NEXT;
	place_marble(g, make_pos(0,0));
	place_marble(g, make_pos(0,1));
	place_marble(g, make_pos(0,2));
	place_marble(g, make_pos(0,3));
	place_marble(g, make_pos(0,4));
	place_marble(g, make_pos(5,5));
	board_show(g -> b);
	printf("\n-expecting to print 0 representing that game above has not over:\n");
	printf("%d\n", game_over(g));
	game_free(g);

}

void evidence_game_outcome() {
	printf("\n\n***testing game_outcome\n");
	game* g = new_game(6, CELLS);
	place_marble(g, make_pos(3,0));
	place_marble(g, make_pos(3,1));
	place_marble(g, make_pos(3,2));
	place_marble(g, make_pos(3,3));
	place_marble(g, make_pos(3,4));
	board_show(g -> b);
	printf("\n-expecting to print 1 representing that white wins:\n");
	printf("%d\n", game_outcome(g));
	game_free(g);

	g = new_game(6, CELLS);
	place_marble(g, make_pos(3,2));
	place_marble(g, make_pos(1,2));
	place_marble(g, make_pos(4,2));
	place_marble(g, make_pos(0,2));
	place_marble(g, make_pos(2,2));
	board_show(g -> b);
	printf("\n-expecting to print 1 representing that white wins:\n");
	printf("%d\n", game_outcome(g));
	game_free(g);

	g = new_game(6, CELLS);
	g -> next = BLACK_NEXT;
	place_marble(g, make_pos(0,1));
	place_marble(g, make_pos(1,2));
	place_marble(g, make_pos(2,3));
	place_marble(g, make_pos(3,4));
	place_marble(g, make_pos(4,5));
	board_show(g -> b);
	printf("\n-expecting to print 0 representing that black wins:\n");
	printf("%d\n", game_outcome(g));
	game_free(g);

	g = new_game(6, CELLS);
	place_marble(g, make_pos(1,5));
	place_marble(g, make_pos(2,4));
	place_marble(g, make_pos(3,3));
	place_marble(g, make_pos(4,2));
	place_marble(g, make_pos(5,1));
	board_show(g -> b);
	printf("\n-expecting to print 1 representing that white wins:\n");
	printf("%d\n", game_outcome(g));
	game_free(g);

	g = new_game(4, CELLS);
	place_marble(g, make_pos(0,2));
	place_marble(g, make_pos(1,1));
	place_marble(g, make_pos(2,0));
	board_show(g -> b);
	printf("\n-expecting to print 1 representing that white wins:\n");
	printf("%d\n", game_outcome(g));
	game_free(g);

	g = new_game(4, CELLS);
	place_marble(g, make_pos(0,2));
	place_marble(g, make_pos(1,1));
	place_marble(g, make_pos(2,0));
	g -> next = BLACK_NEXT;
	place_marble(g, make_pos(1,3));
	place_marble(g, make_pos(2,2));
	place_marble(g, make_pos(3,1));
	board_show(g -> b);
	printf("\n-expecting to print 2 representing that this is a draw:\n");
	printf("%d\n", game_outcome(g));
	game_free(g);

	g = new_game(6, CELLS);
	place_marble(g, make_pos(1,5));
	place_marble(g, make_pos(2,4));
	place_marble(g, make_pos(3,3));
	place_marble(g, make_pos(4,2));
	place_marble(g, make_pos(5,1));
	g -> next = BLACK_NEXT;
	place_marble(g, make_pos(0,0));
	place_marble(g, make_pos(0,1));
	place_marble(g, make_pos(0,2));
	place_marble(g, make_pos(0,3));
	place_marble(g, make_pos(0,4));
	place_marble(g, make_pos(5,5));
	board_show(g -> b);
	printf("\n-expecting to print 2 representing that this is a draw:\n");
	printf("%d\n", game_outcome(g));
	game_free(g);
}


int main(int argc, char** argv) {
	evidence_make_pos();
        evidence_board_new();
	evidence_board_get();
	evidence_board_set();
	evidence_new_game();
	evidence_place_marble();
	evidence_twist_quadrant();
	evidence_game_over();
	evidence_game_outcome();
	return 1;
}
