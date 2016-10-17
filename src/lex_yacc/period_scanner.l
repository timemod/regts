/*
 * lexical analyzer for period string
 */

%{
#include <cstring>
#include <string>
#include <algorithm>
#include <Rcpp.h>

#include "period_scanner.h"
#include "period_parser.h"
#include "months.h"

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

#define YY_FATAL_ERROR(msg) Rf_error(msg)

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
	  return -1;
  }
}

int prwrap( void ) {
    return 1;
}

void init_period_scanner(const std::string &period_text) {

    static std::string textbuf;

    // copy text to textbuf, and convert to lower case
    textbuf = period_text;
    std::transform(textbuf.begin(), textbuf.end(), textbuf.begin(),
                   tolower);

    // add two '\0' characters (see Flex documentation)
    textbuf.append(2, '\0');

    // update buffer
    static YY_BUFFER_STATE buffer = NULL;
    if (buffer != NULL) {
        yy_delete_buffer(buffer);
    }
    buffer = pr_scan_buffer(const_cast<char *>(textbuf.c_str()),
                            textbuf.size());
    pr_switch_to_buffer(buffer);
}
