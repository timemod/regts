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
    static int error, year, subperiod;
    double freq, given_freq;
    static void check_year_subperiod(int freq, int *year, int *frac);
%}

%token  NUMBER

%token  PERIODICITY     /* periodicity (Q, M) */

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
	        {error = 0; year = $1; freq = PERIODICITY_Y; subperiod = 1;} 

y_period : NUMBER YEAR_CHARACTER  /* e.g. 2010y */ 
	        {error = 0; year = $1; freq = PERIODICITY_Y; subperiod = 1;} 
;

opt_sep  : SEP /* separator e.g. ., - */
         | /*empty */
;

qm_period : NUMBER opt_sep NUMBER PERIODICITY   /* e.g. 2010.1q */
               {error = 0; year = $1; subperiod = $3; freq = $4;}
             | PERIODICITY NUMBER opt_sep NUMBER /* e.g. q2 2010 */
               {error = 0; year = $4; subperiod = $2; freq = $1;}
             | NUMBER SEP PERIODICITY NUMBER  /*e.g. 2010.q3 */
               {error = 0; year = $1; subperiod = $4; freq = $3;}
             | NUMBER PERIODICITY SEP NUMBER  /*e.g. 2q/2000 */
               {error = 0; year = $4; subperiod = $1; freq = $2;}
             | NUMBER PERIODICITY NUMBER  /*e.g. 1974m2 */
               {error = 0; year = $1; subperiod = $3; freq = $2;
                check_year_subperiod(freq, &year, &subperiod);}
;

no_periodicity : NUMBER opt_sep NUMBER   /* e.g. 2010/1 or 5/2010 */
            /* for the moment, assume the first number is a year and the
             * second number a subperiod with unknown periodicity */
            {error = 0; year = $1; subperiod = $3;
             if (!ISNA(given_freq)) {
                 check_year_subperiod(given_freq, &year, &subperiod);
             };}
;

month:     MONTH_NAME opt_sep NUMBER  /* e.g. may 2012 */
            {error = 0; year = $3; subperiod = $1; freq = PERIODICITY_M;}
;

%% 

void prerror(const char *s) {
    error = 1;
    /*fprintf(stderr, "%s\n", s);*/
}

static void check_year_subperiod(int freq, int *year, int *frac) {

   /* For periods such as "3 q 20" (without separator), it is not
    * clear whether the first number is a year or a subperiod. 
    * If the subperiod is larger than the maximum subperiod and the
    * year is smaller than or equal to the maximum subperiod then
    * year and subperiod should be swapped. */

    int max_frac;
    switch (freq) {
        case (PERIODICITY_Q) : max_frac =  4; break;
        case (PERIODICITY_M) : max_frac =  12; break;
	default: return;
    }
  
    if (*frac > max_frac && *year <= max_frac && *year > 0) {
       int y = *year;
       *year = *frac;
       *frac = y;
    }
}

// [[Rcpp::export]]
Rcpp::NumericVector parse_period(const std::string &period_text,
                                 double frequency) {

    /* initialise */
    error = 1;
    year = -1; 
    freq = PERIODICITY_UNKNOWN;
    subperiod = -1;
    given_freq = frequency;
   	
    set_period_text(period_text.c_str());

    int retval = yyparse();  
    if (!retval) {
        error = 1;
    }

    prrestart(NULL);
	
    if (error) {
        return R_NilValue;
    } else {
        if (freq == PERIODICITY_UNKNOWN) {
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


#ifdef TEST_PERIOD_PARSER

static void run_test(const char *period_text) {
    int error, y, frac, f, m, d; 
    error = parse_period(period_text, &y, &frac, &f);
    printf("=== \"%s\" ====\n", period_text);
    printf("    Error = %d\n", error);
    if (!error) {
        printf("    Year, subperiod, Freq = %d %d %d\n", y, frac, freq); 
    }
    printf("\n");
}

int main(void) {
    run_test("2010Y");
    run_test("2010 y");
    run_test("2010");
    run_test("10");
    run_test("2010.2q");
    run_test("20102q");
    run_test("2010 2q");
    run_test("2010 q02");
    run_test("10/2013");
    run_test("aap2010");
    run_test("aug 2010");
    run_test("aug2010");
    run_test("aap 2014");
    run_test("2010.1m");
    run_test("T2010_1Q");
    run_test("2013-1");
    run_test("2013-5");
    run_test("1974m2");
    run_test("2m1974");
    run_test("5q5");
    run_test("2000k4");
    run_test("2000 J");
    run_test("2000	J");
    return 0;
}

#endif
