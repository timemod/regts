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

static void parse_period_(const std::string &period_text, double frequency,
                          double &period, double &f) {

    // initialise global variables
    year = -1;
    freq = NA_REAL;
    subperiod = -1;
    given_freq = frequency;

    init_period_scanner(period_text);

    int error_flag = prparse();
    if (error_flag) {
        Rf_error("Illegal period %s.", period_text.c_str());
    } else {
        if (ISNA(freq)) {
            if (ISNA(frequency)) {
                Rf_error("Frequency of period %s unknown."
                         " Specify argument frequency.", period_text.c_str());
            }
	        freq = frequency;
	    } else if (frequency != freq && !ISNA(frequency)) {
	        Rf_error("Specified frequency %d does not agree with actual "
                     "frequency in period %s.", (int) frequency,
                     period_text.c_str());
	    }
    }

    // store results
    period = round(year * freq + subperiod - 1);
    f = freq;
}

// [[Rcpp::export]]
NumericVector parse_period(const std::string &period_text, double frequency) {

    double per, f;
    parse_period_(period_text, frequency, per, f);

    NumericVector result(1);
    result[0] = per;
    result.attr("class") = "regperiod";
    result.attr("frequency") = f;
	return result;
}

static std::string trim(const std::string& str) {
    const std::string whitespace = " \t";
    const auto strBegin = str.find_first_not_of(whitespace);
    if (strBegin == std::string::npos) {
        return ""; // no content
    }
    const auto strEnd = str.find_last_not_of(whitespace);
    const auto strRange = strEnd - strBegin + 1;
    return str.substr(strBegin, strRange);
}

// [[Rcpp::export]]
NumericVector parse_period_range(const std::string &period_text,
                                 double frequency) {

    double p1, p2, f;
    p1 = NA_REAL;
    p2 = NA_REAL;
    f  = NA_REAL;

    const auto pos = period_text.find("/");
    if (pos == std::string::npos) {
        // no / separator
        parse_period_(period_text, frequency, p1, f);
        p2 = p1;
    } else {
         // a / seperator has been found
        std::string s1 = period_text.substr(0, pos);
        std::string s2 = period_text.substr(pos + 1);
        s1 = trim(s1);
        s2 = trim(s2);
        bool both = s1.size() > 0 && s2.size() >> 0;
        if (s1.size() == 0 && s2.size() == 0) {
            Rf_error("Illegal period range %s.", period_text.c_str());
        }
        double f1, f2;
        if (s1.size() > 0) {
            parse_period_(s1, frequency, p1, f);
            f1 = f; // for checking
        }
        if (s2.size() > 0) {
            parse_period_(s2, frequency, p2, f);
            f2 = f; // for checking
        }
        if (both) {
            if (f1 != f2) {
               Rf_error("The two periods have different frequency");
            } else if (p1 > p2) {
               Rf_error("The start period (%s) is after the end period (%s)",
                s1.c_str(), s2.c_str());
            }
        }
    }

    NumericVector result(3);
    result[0] = p1;
    result[1] = p2;
    result[2] = f;
    result.attr("class") = "regperiod_range";
	return result;
}
