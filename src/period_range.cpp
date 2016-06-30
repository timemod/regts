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

