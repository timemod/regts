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
    #include "period.hpp"
    using Rcpp::CharacterVector;
    using Rcpp::NumericVector;
    static bool error;
    static int year, subperiod;
    double freq, given_freq;
    static void check_year_subperiod(int freq, int *year, int *frac);
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
        | INVALID   {YYABORT;}
;

posint   : NUMBER      /* e.g. 2010  */
	        {error = false; year = $1; freq = 1; subperiod = 1;} 

y_period : NUMBER YEAR_CHARACTER  /* e.g. 2010y */ 
	        {error = false; year = $1; freq = 1; subperiod = 1;} 
;

opt_sep  : SEP /* separator e.g. ., - */
         | /*empty */
;

qm_period : NUMBER opt_sep NUMBER FREQ   /* e.g. 2010.1q */
               {error = false; year = $1; subperiod = $3; freq = $4;}
             | FREQ NUMBER opt_sep NUMBER /* e.g. q2 2010 */
               {error = false; year = $4; subperiod = $2; freq = $1;}
             | NUMBER SEP FREQ NUMBER  /*e.g. 2010.q3 */
               {error = false; year = $1; subperiod = $4; freq = $3;}
             | NUMBER FREQ SEP NUMBER  /*e.g. 2q/2000 */
               {error = false; year = $4; subperiod = $1; freq = $2;}
             | NUMBER FREQ NUMBER  /*e.g. 1974m2 */
               {error = false; year = $1; subperiod = $3; freq = $2;
                check_year_subperiod(freq, &year, &subperiod);}
;

no_periodicity : NUMBER opt_sep NUMBER   /* e.g. 2010/1 or 5/2010 */
            /* for the moment, assume the first number is a year and the
             * second number a subperiod with unknown periodicity */
            {error = false; year = $1; subperiod = $3;
             if (!ISNA(given_freq)) {
                 check_year_subperiod(given_freq, &year, &subperiod);
             };}
;

month:     MONTH_NAME opt_sep NUMBER  /* e.g. may 2012 */
            {error = false; year = $3; subperiod = $1; freq = 12;}
;

%% 

void prerror(const char *s) {
    error = true;
    /*fprintf(stderr, "%s\n", s);*/
}

static void check_year_subperiod(int freq, int *year, int *frac) {

   /* For periods such as "3 q 20" (without separator), it is not
    * clear whether the first number is a year or a subperiod. 
    * If the subperiod is larger than the maximum subperiod and the
    * year is smaller than or equal to the maximum subperiod then
    * year and subperiod should be swapped. */

    if (*frac > freq && *year <= freq && *year > 0) {
       int y = *year;
       *year = *frac;
       *frac = y;
    }
}

// [[Rcpp::export]]
SEXP parse_period(const std::string &period_text, double frequency) {

    /* initialise */
    error = true;
    year = -1; 
    freq = FREQ_UNKNOWN;
    subperiod = -1;
    given_freq = frequency;

    set_period_text(period_text);

    int retval = yyparse();  
    if (retval) {
        error = true;
    }

    prrestart(NULL);

    if (error) {
        return R_NilValue;
    } else {
        if (freq == FREQ_UNKNOWN) {
            if (ISNA(frequency)) {
                Rf_error("Frequency unknown. Specify argument frequency");
            }
	    freq = frequency; 
	} else if (frequency != freq && !ISNA(frequency)) {
	    Rf_error("Supplied frequency does not agree with actual frequency "
                     "in regperiod");
	}
        NumericVector result(1);
        result[0] = year * freq + subperiod - 1;
        result.attr("class") = "regperiod";
        result.attr("frequency") = freq;
	return result;
    }
}
