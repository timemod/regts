#define HASHSIZE       13
#include <ctype.h>
#include <stdlib.h>
#include "months.hpp"

typedef struct Month { /* month table entry */
   char   *name;
   unsigned char number;
   struct Month *next;  /* pointer to next month in linked list */
} Month;

static Month *month_tab[HASHSIZE];

static int month_tab_size = 0;

static struct monthlist {
        char          *name;
        unsigned char number;
    } months[] = {
        {"jan"       , 1},
        {"feb"       , 2},
        {"mar"       , 3},
        {"apr"       , 4},
        {"may"       , 5},
        {"jun"       , 6},
        {"jul"       , 7},
        {"aug"       , 8},
        {"sep"       , 9},
        {"oct"       , 10},
        {"nov"       , 11},
        {"dec"       , 12},
        {"mrt"       , 3},
        {"mei"       , 5},
        {"okt"       , 10}
    };

/*
 *  * pjw hash function 
 *   */

#define NUM_BITS    (sizeof(unsigned) * 8)
#define THREE_FOURTHS   (NUM_BITS * 3 / 4)
#define ONE_EIGHTH  (NUM_BITS / 8)
#define HIGH_BITS   (~ ((unsigned) ~0 >> ONE_EIGHTH))

static  unsigned hashf(char *s) {
    unsigned h, tmp; 
    
    for( h = 0; *s; s++ ) 
    {   
        h = (h << ONE_EIGHTH) + tolower(*s);
        if( (tmp = h & HIGH_BITS) != 0 )
            h = (h ^ (tmp >> THREE_FOURTHS)) & ~ HIGH_BITS;
    }
        
    return h % HASHSIZE;
}   

/*
 *  * strlcmp:  Case-insensitive versions of strcmp()
 *   */

static int strlcmp(const char *s1, const char *s2)
{
    register int c;

    while ((c = tolower(*s1)) == tolower(*s2)) {
        if (c == 0)
            return 0;
        s1++;
        s2++;
    }
    if (c < tolower(*s2))
        return -1;
    return 1;
}


#define NBR_KWENTRIES ( sizeof(months) / sizeof(months[0]) )

static void init_month_table(void) {
    int i;
    for (i = 0 ; i < NBR_KWENTRIES; i++ ) {
        unsigned int hv = hashf(months[i].name);
        Month *mp = (Month *) malloc(sizeof(Month));
        mp->name   =  months[i].name;
        mp->number = months[i].number;
        mp->next   = month_tab[hv];    /* put at front of list */
        month_tab[hv] = mp;
    }
    month_tab_size++;
}

/* Returns the number of a month name. The function returns -1 if the
 * month name is not recognized */
int get_month_number(char *month_name) {
    if (!month_tab_size) {
        init_month_table();
    }
    Month *mp; 
    for (mp = month_tab[hashf(month_name)]; mp != NULL; mp = mp->next) {
        if (strlcmp(mp->name, month_name) == 0) {
            return mp->number;
        }
    } 
    return -1;
}
