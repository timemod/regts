#include <Rcpp.h>
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
    per.freq = tsp[2];
    int year = tsp[0] + 100;
    int subp = (tsp[0] - year) * per.freq;
    per.first = year * per.freq + subp;
    per.last = per.first + ts.nrow() - 1;
    return per;
}
