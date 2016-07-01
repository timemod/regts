/*
 * This file contains the function parse_period for parsing a period
 * text.
 *

 * The following table presents the various period types, with examples and the
 * output arguments that are available for the different types. For each
 * type, the arguments not listed in the table contain rubbish and should not
 * be used. The numerical values for the period types are defined
 * in header file period.h.
 *
/*
 * Yacc syntax
 * for parsing period texts
 *
 * uses Bison
 */

%{
    #include <stdio.h>
    #include <Rcpp.h>
    #include <string>
    #include <cstring>
    #include <math.h>
    #include "period_scanner.hpp"
    #include "parse_period.h"
    using Rcpp::NumericVector;
    static int year, subperiod;
    static double freq, given_freq;
    static void check_year_subperiod(int freq, int &year, int &frac);
    void prerror(const char *s);
%}

%token  NUMBER

%token  FREQ    /* frequency (Q, M) */

%token  YEAR_CHARACTER  /* year character (Y or J) */

%token  SEP  /* separator (e.g. . or /)  */

%token  MONTH_NAME  /* name of a month (e.g. may) */

%token  INVALID  /* invalid token */

%%

period:   posint
        | y_period
        | qm_period
        | no_periodicity
        | month
;

posint   : NUMBER      /* e.g. 2010  */
	       {year = $1; freq = 1; subperiod = 1;}

y_period : NUMBER YEAR_CHARACTER  /* e.g. 2010y */
	       {year = $1; freq = 1; subperiod = 1;}
;

opt_sep  : SEP /* separator e.g. ., - */
         | /*empty */
;

qm_period : NUMBER opt_sep NUMBER FREQ   /* e.g. 2010.1q */
               {year = $1; subperiod = $3; freq = $4;}
             | FREQ NUMBER opt_sep NUMBER /* e.g. q2 2010 */
               {year = $4; subperiod = $2; freq = $1;}
             | NUMBER SEP FREQ NUMBER  /*e.g. 2010.q3 */
               {year = $1; subperiod = $4; freq = $3;}
             | NUMBER FREQ SEP NUMBER  /*e.g. 2q/2000 */
               {year = $4; subperiod = $1; freq = $2;}
             | NUMBER FREQ NUMBER  /*e.g. 1974m2 */
               {year = $1; subperiod = $3; freq = $2;
                check_year_subperiod(freq, year, subperiod);}
;

no_periodicity : NUMBER opt_sep NUMBER   /* e.g. 2010/1 or 5/2010 */
            // For the moment, assume the first number is a year and the
             // second number a subperiod with unknown periodicity */
               {year = $1; subperiod = $3;
                if (!ISNA(given_freq)) {
                    check_year_subperiod(given_freq, year, subperiod);
                };}
;

month:     MONTH_NAME opt_sep NUMBER  /* e.g. may 2012 */
            {year = $3; subperiod = $1; freq = 12;}
;

%%

void prerror(const char *s) {}

static void check_year_subperiod(int freq, int &year, int &subp) {

   // Periods with the format X sep Y are ambigious: both X and Y
   // could be the year. For example, for the period "3q2000", the first
   // number (3) is probably the quarter and the second number (2000) is
   // probably the year. If the subperiod is larger than frequency and the
   // year is smaller than or equal to the frequency, then
   // year and subperiod should be swapped.

    if (subp > freq && year <= freq) {
        int y = year;
        year = subp;
        subp = y;
    }
}

ParsedPeriod parse_period(const std::string &period_text, int frequency) {

    // initialise global variables
    year       = NA_INTEGER;
    freq       = NA_INTEGER;
    subperiod  = NA_INTEGER;
    given_freq = frequency;

    init_period_scanner(period_text);

    int error_flag = prparse();

    return ParsedPeriod(year, subperiod, freq, error_flag > 0);
}
