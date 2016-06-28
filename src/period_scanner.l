/*
 * lexical analyzer for period string
 */

%{
#include <cstring>
#include <string>
#include <algorithm>

#include "period.hpp"
#include "period_parser.hpp"
#include "months.hpp"

static const char *c;
static int get_frequency(const char *c);

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

static int textbuf_size = 0;
static char *textbuf;

%}

DIGIT           [0-9]
SEP             [./\-:_]
FREQUENCY       [qmk]
YEAR_CHAR       [yj]
MONTH_NAME      [a-z]{3,}

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

static int get_frequency(const char *c) {
   switch (*c) {
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

void init_period_scanner(const std::string &period_text) {

    int len = period_text.size();
    int size = len + 2;
  
    if (textbuf == NULL) {
        textbuf_size = 2 * size;
        textbuf = (char *) malloc(textbuf_size); 
    } else if (textbuf_size < size) {
        textbuf_size = 2 * size;
	textbuf = (char *) realloc(textbuf, textbuf_size);
    }

    period_text.copy(textbuf, len);

    // textbuf should end with two terminal ASCII NULL characters
    // (see the documentation of Flex).
    textbuf[len]     = '\0';
    textbuf[len + 1] = '\0';

    // convert to lower case
    for (char *p =  textbuf; *p; ++p) {
        *p = tolower(*p);
    }

    static YY_BUFFER_STATE buffer = NULL;
    if (buffer != NULL) {
        yy_delete_buffer(buffer);
    }
    buffer = pr_scan_buffer(textbuf, size);
    pr_switch_to_buffer(buffer);
}
