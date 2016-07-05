#include <Rcpp.h>
#include <algorithm>
#include "period_range.h"
using namespace Rcpp;

static int nper_new, shift, rmin, rmax;

void convert_selector(PeriodRange &selector, const PeriodRange &ts_range);

template <int RTYPE>
SEXP select_rows_templ(Matrix<RTYPE> xin){
    Matrix<RTYPE> xout(nper_new, xin.ncol());
    std::fill(xout.begin(), xout.end(), Vector<RTYPE>::get_na());
    for (int col = 0; col < xin.ncol(); col++) {
        for (int row = rmin; row <= rmax; row++) {
            xout(row, col) = xin(row + shift, col);
        }
    }
    return xout;
}

// [[Rcpp::export]]
List select_rows(const SEXP ts_old, const NumericVector selector) {
    PeriodRange per_old = get_period_range(ts_old);
    PeriodRange per_new(selector);
    convert_selector(per_new, per_old);
    nper_new = per_new.len();
    int nper_old = per_old.len();
    shift = per_new.first - per_old.first;
    rmin = std::max(0, -shift);
    rmax = std::min(nper_new - 1, nper_old - shift - 1);
    SEXP data;
    switch(TYPEOF(ts_old)) {
    case INTSXP:
        data = select_rows_templ<INTSXP>(ts_old); break;
    case REALSXP:
        data = select_rows_templ<REALSXP>(ts_old); break;
    case LGLSXP:
        data = select_rows_templ<LGLSXP>(ts_old); break;
    case STRSXP:
        data = select_rows_templ<STRSXP>(ts_old); break;
    case CPLXSXP:
        data = select_rows_templ<CPLXSXP>(ts_old); break;
    case VECSXP:
        data = select_rows_templ<VECSXP>(ts_old); break;
    default:
        // not implementation for EXPRSXP and RAWSXP, because for these
        // types the function Vector<RTYPE>::get_na() is not avaiable.
        Rf_error("Function regts::select_row  not implemented for timeseries "
                 "of type %d", TYPEOF(ts_old));
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





