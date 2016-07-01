#include <Rcpp.h>
#include <math.h>
#include "period_range.h"
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;

// Returns the PeriodRange of a timeseries.
// TODO: this function is almost a duplicate of the R code of
// package regts. Create a general C++ function for this,
// also to be used in the R code of this package.
PeriodRange get_period_range(const NumericMatrix &ts) {
    PeriodRange per;
    NumericVector tsp = ts.attr("tsp");
    per.freq  = tsp[2];
    int year  = floor(tsp[0]);
    int subp  = round((tsp[0] - year) * per.freq);
    per.first = round(year * per.freq + subp);
    per.last  = per.first + ts.nrow() - 1;
    return per;
}

PeriodRange modify_frequency(PeriodRange old_range, int new_freq) {
    if ((int) new_freq % (int) old_range.freq != 0) {
        Rf_error("Frequency of regperiod_range is no divisor of the "
                "required frequency");
    }
    int factor = new_freq / old_range.freq;
    PeriodRange new_range;
    new_range.first = round(old_range.first * factor);
    new_range.last  = round((old_range.last + 1) * factor - 1);
    new_range.freq = new_freq;
    return new_range;
}

//' Returns the period range of the time series as a \link{regperiod_range}
//' object.
//'
//' @param x a \code{regts} or \code{ts}
//' @return a \code{regperiod_range}
//' @export
// [[Rcpp::export]]
NumericVector get_regperiod_range(const SEXP &tsSEXP) {
    if (!Rf_inherits(tsSEXP, "ts")) {
        Rf_error("Argument is not a regts or ts");
    }
    Rcpp::traits::input_parameter< const NumericMatrix& >::type ts(tsSEXP);
    PeriodRange range = get_period_range(ts);
    NumericVector result(3);
    result[0] = range.first;
    result[1] = range.last;
    result[2] = range.freq;
    result.attr("class") = "regperiod_range";
    return result;
}

