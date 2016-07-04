#include <Rcpp.h>
#include <string>
#include "period_range.h"
using namespace Rcpp;

// TODO: @export only used in the testing phase of regts,
// in the final version it should not be exported.
//' @export
// [[Rcpp::export]]
SEXP window_numregts(NumericMatrix &ts_old, NumericVector &range) {

    // save the dimension names
    List dimnames = ts_old.attr("dimnames");
    CharacterVector names = dimnames[1];

    PeriodRange per_old = get_period_range(ts_old);

    PeriodRange per_new;
    per_new.first = range[0];
    per_new.last  = range[1];
    per_new.freq  = range[2];

    if (per_old.freq != per_new.freq) {
        per_new = modify_frequency(per_new, per_old.freq);
    }

    per_new.first = ISNA(per_new.first) ? per_old.first : per_new.first;
    per_new.last  = ISNA(per_new.last)  ? per_old.last  : per_new.last;

    int nper_new = per_new.len();
    int nper_old = per_old.len();
    int shift = per_new.first - per_old.first;
    int rmin = shift > 0 ? 0 : -shift;
    int rmax = nper_new + shift > nper_old ? nper_old - shift : nper_new;

    NumericMatrix data(nper_new, ts_old.ncol());
    for (int col = 0; col < ts_old.ncol(); col++) {
        for (int row = 0; row < rmin; row++) {
            data(row, col) = NA_REAL;
        }
        for (int row = rmin; row < rmax; row++) {
            data(row, col) = ts_old(row + shift, col);
        }
        for (int row = rmax; row < nper_new; row++) {
            data(row, col) = NA_REAL;
        }
    }

    Rcpp::NumericVector range_new(3);
    range_new[0] = per_new.first;
    range_new[1] = per_new.last;
    range_new[2] = per_new.freq;

    Rcpp::List retval(2);
    retval[0] = data;
    retval[1] = range_new;
    return retval;
}
