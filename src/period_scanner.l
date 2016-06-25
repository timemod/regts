/*
 * lexical analyzer for period string
 */

%{

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "period.hpp"
#include "period_parser.hpp"
#include "months.hpp"

static int get_periodicity(char *per_text);

static char *c;

/*
 * for MKS Lex
 */

#undef  YY_INTERACTIVE
#define YY_INTERACTIVE 0

/*
 * the following is needed
 * to make flex not use isatty and fileno
 * which gave errors in fcc (Fujitsu C compiler in Lahey)
 */

#define YY_NEVER_INTERACTIVE 1

/*
 * Flex needs this
 * MKS Lex doesn't and just ignores this
 * but do use the special Flex scanner macro
 */

#ifdef FLEX_SCANNER

#define YY_INPUT(buf,result,max_size) \
{\
    result = (*c == '\0') ? YY_NULL : (buf[0] = *c++, 1); \
}

#endif
%}

DIGIT           [0-9]
SEP             [./\-:_]
PERIODICITY     [QMK]
YEAR_CHAR       [YJ]
MONTH_NAME      [a-z]{3,}

%%

{DIGIT}+      {prlval = atoi(yytext); return NUMBER;}

{PERIODICITY} {prlval = get_periodicity(yytext); return PERIODICITY;}

{YEAR_CHAR}   {return YEAR_CHARACTER;}

{SEP}         {return SEP;}

{MONTH_NAME}  {prlval = get_month_number(yytext); 
               int type = prlval > 0 ? MONTH_NAME : INVALID;
               return type;}

[ \t]+        /* eat up white space */

.             {return INVALID;}

%%

static int get_periodicity(char *per_text) {
   char c = tolower(*per_text);
   switch (c) {
       case ('q') : 
       case ('k') : 
          return PERIODICITY_Q; break;      
       case ('m') : 
          return PERIODICITY_M; break;   
       default:
	  return PERIODICITY_UNKNOWN;
  }
}

int prwrap( void ) {
    return 1;
}

void set_period_text(const char *period_text) {
    c = (char *) malloc(strlen(period_text) + 1);
    strcpy(c, period_text);
    while (isspace(*c)) c++;
    if (tolower(*c) == 't') c++; /* skip leading T (datum variable) */
}
