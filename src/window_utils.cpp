#include <Rcpp.h>
#include <algorithm>
#include "period_range.h"
using namespace Rcpp;

void convert_selector(PeriodRange &selector, const PeriodRange &ts_range);

template <int RTYPE>
SEXP select_rows_templ(Matrix<RTYPE> ts_old, const NumericVector &selector){

    // determine the rows to select
    PeriodRange per_old = get_period_range(ts_old);
    PeriodRange per_new(selector);
    convert_selector(per_new, per_old);

    int nper_new = per_new.len();
    int nper_old = per_old.len();
    int shift = per_new.first - per_old.first;
    int rmin = std::max(0, -shift);
    int rmax = std::min(nper_new - 1, nper_old - shift - 1);

    Matrix<RTYPE> data(nper_new, ts_old.ncol());
    std::fill(data.begin(), data.end(), Vector<RTYPE>::get_na());
    for (int col = 0; col < ts_old.ncol(); col++) {
        for (int row = rmin; row <= rmax; row++) {
            data(row, col) = ts_old(row + shift, col);
        }
    }

    Rcpp::List retval(2);
    retval[0] = data;
    retval[1] = per_new.get_regperiod_range();
    return retval;
}

// [[Rcpp::export]]
List select_rows(const SEXP ts_old, const NumericVector selector) {
    switch(TYPEOF(ts_old)) {
    case INTSXP:
        return select_rows_templ<INTSXP>(ts_old, selector);
    case REALSXP:
        return select_rows_templ<REALSXP>(ts_old, selector);
    case LGLSXP:
        return select_rows_templ<LGLSXP>(ts_old, selector);
    case STRSXP:
        return select_rows_templ<STRSXP>(ts_old, selector);
    case CPLXSXP:
        return select_rows_templ<CPLXSXP>(ts_old, selector);
    case VECSXP:
        return select_rows_templ<VECSXP>(ts_old, selector);
    default:
        // no implementation for EXPRSXP and RAWSXP, because for these
        // types the function Vector<RTYPE>::get_na() is not avaiable.
        Rf_error("Function regts::select_row  not implemented for timeseries "
                 "of type %d", TYPEOF(ts_old));
        return R_NilValue;
    }
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
    if (selector.first > selector.last) {
        // period range incorrect. For example, a timeseries has range
        // 2010Q2/2011Q4, and the selector is 2017Q4/ (no upper boundary)
        // TODO: better error message
        Rf_error("Incorrect period selection");
    }
}


