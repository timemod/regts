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
    #include "period_scanner.h"
    #include "parse_period.h"
    using Rcpp::NumericVector;
    static double year, subperiod;
    static double freq, given_freq;
    static void check_year_subperiod(double freq, double &year, double &frac);
    void prerror(const char *s);
%}

%token  NUMBER

%token  FREQ    /* frequency (Q, M) */

%token  YEAR_CHARACTER  /* year character (Y or J) */

%token  TIME_CHARACTER  /* time character (T) */

%token  SEP  /* separator (e.g. . or /)  */

%token  MONTH_NAME  /* name of a month (e.g. may) */

%token  INVALID  /* invalid token */

%%

period:   posint
        | y_period
        | qm_period
        | no_periodicity_1
        | no_periodicity_2
        | month
;

posint   : opt_t NUMBER      /* e.g. 2010  */
	       {year = $2; freq = 1; subperiod = 1;}

y_period : opt_t NUMBER YEAR_CHARACTER  /* e.g. 2010y */
	       {year = $2; freq = 1; subperiod = 1;}
         | YEAR_CHARACTER NUMBER /* e.g. y2010 */
	       {year = $2; freq = 1; subperiod = 1;}
;

opt_sep  : SEP /* separator e.g. ., - */
         | /*empty */
;

opt_t  : TIME_CHARACTER /* t */
         | /*empty */
;

qm_period :   opt_t NUMBER opt_sep NUMBER FREQ  /* e.g. 2010.1q */
               {year = $2; subperiod = $4; freq = $5;}
            | YEAR_CHARACTER NUMBER opt_sep NUMBER FREQ  /* e.g. Y2010.1q */
               {year = $2; subperiod = $4; freq = $5;}
            | FREQ NUMBER opt_sep NUMBER /* e.g. q2 2010 */
               {year = $4; subperiod = $2; freq = $1;}
            | opt_t NUMBER SEP FREQ NUMBER  /*e.g. 2010.q3 */
               {year = $2; subperiod = $5; freq = $4;}
            | YEAR_CHARACTER NUMBER SEP FREQ NUMBER  /*e.g. Y2010.q3 */
               {year = $2; subperiod = $5; freq = $4;}
            | opt_t NUMBER FREQ SEP NUMBER  /*e.g. 2q/2000 */
               {year = $5; subperiod = $2; freq = $3;}
            | YEAR_CHARACTER NUMBER FREQ NUMBER  /*e.g. Y1974m2 */
               {year = $2; subperiod = $4; freq = $3;}
            | opt_t NUMBER FREQ NUMBER  /*e.g. 1974m2 */
	        // This format is ambiguous: the first number can be a
                // year or a subperiod. For the moment, assume the first 
                // number is a year and the second number a subperiod with
                // unknown periodicity
               {year = $2; subperiod = $4; freq = $3;
                check_year_subperiod(freq, year, subperiod);}
;

no_periodicity_1 : YEAR_CHARACTER NUMBER opt_sep NUMBER /* e.g. Y2010 3 */
               {year = $2; subperiod = $4;}

no_periodicity_2 : opt_t NUMBER opt_sep NUMBER   /* e.g. 2010/1 or 5/2010 */
	       // This format is ambiguous: the first number can be a
               // year or a subperiod. For the moment, assume the first 
               // number is a year and the second number a subperiod with
               // unknown periodicity
               {year = $2; subperiod = $4;
                if (!ISNA(given_freq)) {
                    check_year_subperiod(given_freq, year, subperiod);
                };}
;

month:     MONTH_NAME opt_sep NUMBER  /* e.g. may 2012 */
            {year = $3; subperiod = $1; freq = 12;}
;

%%

void prerror(const char *s) {}

static void check_year_subperiod(double freq, double &year, double &subp) {

   // Periods with the format X sep Y are ambigious: both X and Y
   // could be the year. For example, for the period "3q2000", the first
   // number (3) is probably the quarter and the second number (2000) is
   // probably the year. If the subperiod is larger than frequency and the
   // year is smaller than or equal to the frequency, then
   // year and subperiod should be swapped.

    if (subp > freq && year <= freq) {
        double y = year;
        year = subp;
        subp = y;
    }
}

ParsedPeriod parse_period_text(const std::string &period_text, double frequency) {

    // initialise global variables
    year       = NA_REAL;
    freq       = NA_REAL;
    subperiod  = NA_REAL;
    given_freq = frequency;

    init_period_scanner(period_text);

    int error_flag = prparse();

    return ParsedPeriod(year, subperiod, freq, error_flag > 0);
}
