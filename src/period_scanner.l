/*
 * lexical analyzer for period string
 */

%{

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <string>

#include "period.hpp"
#include "period_parser.hpp"
#include "months.hpp"

static int textbuf_size = 0;
static char *textbuf, *c;
static int get_frequency(char *per_text);

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
FREQUENCY       [QMK]
YEAR_CHAR       [YJ]
MONTH_NAME      [A-Z]{3,}

%%

{DIGIT}+     {prlval = atoi(yytext); return NUMBER;}

{FREQUENCY}  {prlval = get_frequency(yytext); return FREQ;}

{YEAR_CHAR}   {return YEAR_CHARACTER;}

{SEP}         {return SEP;}

{MONTH_NAME}  {prlval = get_month_number(yytext); 
               int type = prlval > 0 ? MONTH_NAME : INVALID;
               return type;}

[ \t]+        /* eat up white space */

.             {return INVALID;}

%%

static int get_frequency(char *per_text) {
   char c = tolower(*per_text);
   switch (c) {
       case ('q') : 
       case ('k') : 
          return 4; break;      
       case ('m') : 
          return 12; break;   
       default:
	  return FREQ_UNKNOWN;
  }
}

int prwrap( void ) {
    return 1;
}

#include <Rcpp.h>
#include <iostream>

void set_period_text(const std::string &period_text) {
    // TODO: use an iterator over period_text in the macro YYINPUT

    size_t min_size = period_text.size() + 1;
    if (textbuf == NULL) {
        textbuf_size = 2 * min_size;
        textbuf = (char *) malloc(textbuf_size);
    } else  if (textbuf_size < min_size) {
        textbuf_size = 2 * min_size;
        textbuf = (char *) realloc(textbuf, textbuf_size);
    }
    strcpy(textbuf, period_text.c_str());
    c = textbuf;
}
