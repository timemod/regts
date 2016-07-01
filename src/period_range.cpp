#include <Rcpp.h>
#include <math.h>
#include "period_range.h"
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;

// Returns the period range based on the tsp attribute of a timeseries
PeriodRange get_period_range(const NumericVector &tsp, const int len) {
    PeriodRange per;
    per.freq  = tsp[2];
    int year  = floor(tsp[0]);
    int subp  = round((tsp[0] - year) * per.freq);
    per.first = round(year * per.freq + subp);
    per.last  = per.first + len - 1;
    return per;
}

// Returns the PeriodRange of a numerical timeseries.
PeriodRange get_period_range(const NumericMatrix &ts) {
    NumericVector tsp = ts.attr("tsp");
    return get_period_range(tsp, ts.nrow());
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

// Determine the regperiod_range based on the tsp attribute of a timeseries
// object.
// [[Rcpp::export]]
NumericVector get_regperiod_range_from_tsp(const NumericVector &tsp,
                                           int len) {
    PeriodRange range = get_period_range(tsp, len);
    NumericVector result(3);
    result[0] = range.first;
    result[1] = range.last;
    result[2] = range.freq;
    result.attr("class") = "regperiod_range";
    return result;
}
