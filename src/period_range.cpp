#include <Rcpp.h>
#include <math.h>
#include "period_range.h"
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;

// Returns the period range based on the tsp attribute of a timeseries
PeriodRange get_period_range(const NumericVector &tsp) {
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
PeriodRange get_period_range(const NumericMatrix &ts) {
    NumericVector tsp = ts.attr("tsp");
    return get_period_range(tsp);
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

//' Returns the \link{regperiod_range} of a timeseries.
//'
//' @param x a timeseries (\link{ts} or \link{regts})
//' @return a \code{regperiod_range}
//' @export
// [[Rcpp::export]]
NumericVector get_regperiod_range(const SEXP &ts) {
    if (!Rf_inherits(ts, "ts")) {
        Rf_error("Argument is not a timeseries");
    }
    SEXP attr = Rf_getAttrib(ts, Rf_install("tsp"));
    if (Rf_isNull(attr)) {
        Rcpp::Rcout << "geen tsp-attribuut" << std::endl;
    }
    NumericVector tsp(attr);
    PeriodRange range = get_period_range(tsp);

    NumericVector result(3);
    result[0] = range.first;
    result[1] = range.last;
    result[2] = range.freq;
    result.attr("class") = "regperiod_range";
    return (result);
}
