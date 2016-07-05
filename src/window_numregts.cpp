#include <Rcpp.h>
#include <algorithm>
#include "period_range.h"
using namespace Rcpp;

void convert_selector(PeriodRange &selector, const PeriodRange &ts_range);

// [[Rcpp::export]]
List window_numregts(const NumericMatrix ts_old, const NumericVector selector) {

    // save the dimension names
    List dimnames = ts_old.attr("dimnames");
    CharacterVector names = dimnames[1];

    PeriodRange per_old = get_period_range(ts_old);
    PeriodRange per_new(selector);

    convert_selector(per_new, per_old);

    int nper_new = per_new.len();
    int nper_old = per_old.len();
    int shift = per_new.first - per_old.first;
    int rmin = std::max(0, -shift);
    int rmax = std::min(nper_new - 1, nper_old - shift - 1);
    NumericMatrix data(nper_new, ts_old.ncol());
    for (int col = 0; col < ts_old.ncol(); col++) {
        for (int row = 0; row < rmin; row++) {
            data(row, col) = NA_REAL;
        }
        for (int row = rmin; row <= rmax; row++) {
            data(row, col) = ts_old(row + shift, col);
        }
        for (int row = rmax + 1; row < nper_new; row++) {
            data(row, col) = NA_REAL;
        }
    }

    Rcpp::List retval(2);
    retval[0] = data;
    retval[1] = per_new.get_regperiod_range();
    return retval;
}

// [[Rcpp::export]]
NumericVector convert_range_selector(const NumericVector &selector,
                                     const NumericVector &ts_range) {
    PeriodRange ts_per(ts_range);
    PeriodRange sel_per(selector);
    convert_selector(sel_per, ts_per);
    return sel_per.get_regperiod_range();
}

// Convert the PeriodRange selector. The frequency is made equal to
// the frequency of ts_range, and NA values are replaced with values
// from ts_range
void convert_selector(PeriodRange &selector, const PeriodRange &ts_range) {
    if (selector.freq != ts_range.freq) {
        selector.modify_frequency(ts_range.freq);
    }
    selector.first = ISNA(selector.first) ? ts_range.first : selector.first;
    selector.last  = ISNA(selector.last)  ? ts_range.last  : selector.last;
}





