#include <Rcpp.h>
#include <math.h>
#include "period_range.h"
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;

NumericVector PeriodRange::get_period_range() {
    NumericVector result(3);
    result[0] = first;
    result[1] = last;
    result[2] = freq;
    result.attr("class") = "period_range";
    return (result);
}

// Returns the period range based on the tsp attribute of a timeseries
PeriodRange get_prd_range(const NumericVector &tsp) {
    PeriodRange per;
    per.freq  = tsp[2];
    int year1 = floor(tsp[0]);
    int year2 = floor(tsp[1]);
    int subp1 = round((tsp[0] - year1) * per.freq);
    int subp2 = round((tsp[1] - year2) * per.freq);
    per.first = round(year1 * per.freq + subp1);
    per.last  = round(year2 * per.freq + subp2);
    return per;
}

// Returns the PeriodRange of a numerical timeseries.
PeriodRange get_prd_range(const NumericMatrix &ts) {
    NumericVector tsp = ts.attr("tsp");
    return get_prd_range(tsp);
}

//' Returns the \link{period_range} of a timeseries.
//'
//' @param x a timeseries (\link{ts} or \link{regts})
//' @return a \code{period_range}
//' @export
//' @seealso \code{\link{get_periods}}
// [[Rcpp::export]]
NumericVector get_period_range(const SEXP &x) {
    if (!Rf_inherits(x, "ts")) {
        Rf_error("Argument is not a timeseries");
    }
    SEXP attr = Rf_getAttrib(x, Rf_install("tsp"));
    NumericVector tsp(attr);
    PeriodRange range = get_prd_range(tsp);
    return range.get_period_range();
}
