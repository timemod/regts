#include <Rcpp.h>
#include <math.h>
#include <string>
#include "period_range.h"
#include "intpol_cspline.h"

using namespace Rcpp;

static void disagg_spline_single(NumericMatrix::Column column_old,
                                 NumericMatrix::Column column_new,
                                 int nper_old, int nper_new,
                                 int frac, bool do_cumul, 
                                 const char spline_method,
                                 double x[], double y[], double xnew[], 
                                 double ynew[], double *work[4]);

// [[Rcpp::export]]
List disagg_spline(NumericMatrix ts_old, const int freq_new,
                   const std::string &constraint, const std::string &method) {

    PeriodRange per_old = get_prd_range(ts_old);
    PeriodRange per_new;
    per_new.freq = freq_new;

    if (freq_new <= (int) per_old.freq) {
        Rf_error("nfrequency (%d) is not larger than the "
                "input frequency (%d)", freq_new, (int) per_old.freq);
    }
    if (freq_new % (int) per_old.freq != 0) {
        Rf_error("nfrequency (%d) is not an integer multiple "
                "of the input frequency (%d)", freq_new, (int) per_old.freq);
    }

    bool do_cumul = constraint == "average" || constraint == "sum"; 
    const char spline_method = method == "nakn" ? '3' : 'n';

    int frac = ((int) freq_new / ((int) per_old.freq));
    per_new.first = per_old.first * frac;
    per_new.last  = per_old.last * frac;
    if (do_cumul) {
        per_new.last  = per_new.last + frac - 1;
    } else if (constraint == "last") {
        per_new.first = per_new.first + frac - 1;
        per_new.last  = per_new.last + frac - 1;
    }
    int nper_new = per_new.len();
    int nper_old = per_old.len();

    NumericMatrix data(nper_new, ts_old.ncol());
    std::fill(data.begin(), data.end(), NumericVector::get_na());

    // allocate auxiliary arrays
    int n_max = nper_old;
    int nnew_max = nper_new;
    if (do_cumul) {
        n_max++;
        nnew_max++;
    }
    double *x = new double[n_max];
    double *y = new double[n_max];
    double *xnew = new double[nnew_max];
    double *ynew = new double[nnew_max];
    double *work[4];
    for (int i = 0; i < 4; i++) {
        work[i] = new double[n_max];
    }

    for (int i = 0; i < n_max; i++) {
       x[i] = i;
    }
    for (int i = 0; i < nnew_max; i++) {
       xnew[i] = ((double) i) / frac;
    }
     
    for (int col = 0; col < ts_old.ncol(); col++) {
        disagg_spline_single(ts_old(_, col), data(_, col), nper_old,
                             nper_new, frac, do_cumul, spline_method, 
                             x, y, xnew, ynew, work);
    }

    delete[] x;
    delete[] y;
    delete[] xnew;
    delete[] ynew;
    for (int i = 0; i < 4; i++) {
        delete[] work[i];
    }

    if (constraint == "average") {
        data = data * frac;
    }

    List result(2);
    result[0] = data;
    result[1] = per_new.get_period_range();
    return (result);
}

static void disagg_spline_single(NumericMatrix::Column column_old,
                          NumericMatrix::Column column_new,
                          int nper_old, int nper_new,
                          int frac, bool do_cumul, char spline_method,
                          double x[], double y[], double xnew[],
                          double ynew[], double *work[4]) {
    
    // find first non-NA 
    int i1_old;
    for (i1_old = 0; i1_old < nper_old; i1_old++) {
        if (R_FINITE(column_old[i1_old])) break;
    }

    if (i1_old == nper_old) return; // all NA 

    int i2_old;
    for (i2_old = nper_old - 1; i2_old >= 0; i2_old--) {
        if (R_FINITE(column_old[i2_old])) break;
    }
    
    int n = i2_old - i1_old + 1;

    int i1_new = i1_old * frac;
    int i2_new = nper_new - 1 - (nper_old - 1 - i2_old) * frac;
    int nnew = i2_new - i1_new + 1;

    if (do_cumul) {
        n++;
        nnew++;
        y[0] = 0.0;
        for (int i = 1; i < n; i++) {
            y[i] = y[i - 1] + column_old[i - 1 + i1_old];
        }
    } else {
        for (int i = 0; i < n; i++)  {
            y[i] = column_old[i + i1_old];
        }
    }
    
    // check for NA values in y
    for (int i = 0; i < n; i++) {
        if (!R_FINITE(y[i])) return;
    }

    int ierr  = intpol_cspline(n, nnew, x, y, xnew, ynew, spline_method, work);
    if (ierr != 0) return;

    if (do_cumul) {
        for (int i = 1; i < nnew; i++) {
            column_new[i - 1 + i1_new] = (ynew[i] - ynew[i - 1]);
        }
    } else {
        for (int i = 0; i < nnew; i++) {
            column_new[i + i1_new] = ynew[i];
        }
    }
}
