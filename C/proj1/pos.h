#ifndef _POS_H
#define _POS_H

struct pos {
	    unsigned int r, c;
};

typedef struct pos pos;

/* simply return a pos at the given coordinates */
pos make_pos(unsigned int r, unsigned int c);

#endif /* _POS_H */
