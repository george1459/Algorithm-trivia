# include <stdio.h>
# include <stdlib.h>
# include "board.h"
# include "logic.h"
# include "pos.h"
# include "string.h"

void step_1(game* g) {
	printf("Here is the current state of the board:\n");
	board_show(g -> b);
}

void step_2(game* g){
	if (g -> next == WHITE_NEXT) {
		printf("White, place marble: \n");
	}
	else if (g -> next == BLACK_NEXT) {
		printf("Black, place marble: \n");
	}
}

void invalid_char(game* g, char* row, char* col) {
	scanf(" %c%c", row, col);
	while (!((*row - '0' <= '9' - '0') || (*row - 'a' <= 'z' - 'a') || (*row - 'A' <= 'Z' - 'A')) && 
              ((*col - '0' <= '9' - '0') || (*col - 'a' <= 'z' - 'a') || (*col - 'A' <= 'Z' - 'A')))  {
		printf("invalid input, try again\n");
		step_2(g);			 	
		scanf(" %c%c", row, col);
	}
}


void step_3_8(game* g, unsigned int side) {
	char row, col;
	invalid_char(g, &row, &col);
	/*testing purporse, printf("value of char: %c\n", row);*/
	unsigned int r = (row - '0') <= '9' - '0' ? (row - '0') : ((row - 'A') <= 'Z' - 'A' ? (row - 'A' + 10) : (row - 'a' + 36)); 
	unsigned int c = (col - '0') <= '9' - '0' ? (col - '0') : ((col - 'A') <= 'Z' - 'A' ? (col - 'A' + 10) : (col - 'a' + 36)); 
	/*testing purpose, printf("value of unsigned: %u\n",r);*/
	while (r > side - 1 || c > side - 1) {
		printf("input exceeds given side, try again\n");
		invalid_char(g, &row, &col);
		r = (row - '0') <= '9' - '0' ? (row - '0') : ((row - 'A') <= 'Z' - 'A' ? (row - 'A' + 10) : (row - 'a' + 36)); 
		c = (col - '0') <= '9' - '0' ? (col - '0') : ((col - 'A') <= 'Z' - 'A' ? (col - 'A' + 10) : (col - 'a' + 36)); 
	}
	if (!place_marble(g, make_pos(r, c))) {
		printf("position already taken, try again\n");
		step_2(g);
		step_3_8(g, side);
	}
	else {
		place_marble(g, make_pos(r, c));
	}
}

void step_10(game* g) {
	step_1(g);
	printf("Now it's time to perform a twist. Please first select the quadrant to turn.\nTo turn the top-left quadrant, type 0;\nTo turn the top-right quadrant, type 1;\nTo turn the bottom-left quadrant, type 2;\nTo turn the bottom-right quadrant, type 3;\n");
	unsigned int q;
	scanf("%u", &q);
	while (q > 3) {
		printf("invalid entry, try again\n");
		printf("Now it's time to perform a twist. Please first select the quadrant to turn.\nTo turn the top-left quadrant, type 0;\nTo turn the top-right quadrant, type 1;\nTo turn the bottom-left quadrant, type 2;\nTo turn the bottom-right quadrant, type 3;\n");
		scanf("%u", &q);
	}
	printf("Good. Now please select a direction.\nTo twist in the clockwise direction, type 0;\nTo twist in the counter-clockwise direction, type 1;\n");
	unsigned int d;
	scanf("%u", &d);
	while (d > 1) {
		printf("invalid entry, try again\n");
		printf("Please select a direction.\nTo twist in the clockwise direction, type 0;\nTo twist in the counter-clockwise direction, type 1;\n");
		scanf("%u", &d);
	}
	twist_quadrant(g, q, d);
}


void cycle(game* g, unsigned int side) {
	step_1(g);
	step_2(g);
	step_3_8(g, side);
	if (game_over(g) == 1) {
		if (g -> next == WHITE_NEXT && game_outcome(g) == WHITE_WIN){
			printf("Game over. White has won. Final board shown below\n");
			board_show(g -> b);
			exit(0);
		}
		else if (g -> next == BLACK_NEXT && game_outcome(g) == BLACK_WIN) {
			printf("Game over. Black has won. Final board shown below\n");
			board_show(g -> b);
			exit(0);
		}
		else {
			step_10(g);
			if (game_over(g) == 1) {
				if (g -> next == WHITE_NEXT && game_outcome(g) == WHITE_WIN){
					printf("Game over. White has won. Final board shown below\n");
					board_show(g -> b);
					exit(0);
				}
				else if (g -> next == BLACK_NEXT && game_outcome(g) == BLACK_WIN) {
					printf("Game over. Black has won. Final board shown below\n");
					board_show(g -> b);
					exit(0);
				}
			}
		}
	}
	else if (game_over(g) != 1) {
		step_10(g);
		if (game_over(g) == 1) {
			if (g -> next == WHITE_NEXT && game_outcome(g) == WHITE_WIN){
				printf("Game over. White has won. Final board shown below\n");
				board_show(g -> b);
				exit(0);
			}
			else if (g -> next == BLACK_NEXT && game_outcome(g) == BLACK_WIN) {
				printf("Game over. Black has won. Final board shown below\n");
				board_show(g -> b);
				exit(0);
			}
		}
	}
}


int main(int argc, char** argv) {
	unsigned int side;
    if (argc < 2) {
        fprintf(stderr, "invalid input\n");
        exit(1);
    }
	if (argc > 3 || strcmp(argv[1], "-s") != 0) {
		fprintf(stderr, "invalid input\n");
		exit(1);
	}
	if (atoi(argv[2]) <= 0) {
		fprintf(stderr, "side specified negative\n");
		exit(1);
	}
	side = atoi(argv[2]);
	game* g = new_game(side, CELLS);
	while (1) {
		cycle(g, side);
	}
}
	
