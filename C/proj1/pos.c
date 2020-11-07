# include <stdio.h>
# include <stdlib.h>
# include "pos.h"

pos make_pos(unsigned int r, unsigned int c) {
	pos res;
	res.r = r;
	res.c = c;
	return res;
}
