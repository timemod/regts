#include <Rcpp.h>
#include <math.h>
#include <string>
#include "period_range.h"
#include "intpol_cspline.h"

using namespace Rcpp;

void disagg_spline_single(NumericMatrix::Column column_old,
                          NumericMatrix::Column column_new,
                          int frac, bool do_cumul, const char spline_method,
                          double x[], double y[], double xnew[], double ynew[],
                          double *work[4]);

// [[Rcpp::export]]
List disagg_spline(NumericMatrix ts_old, const int freq_new,
                   const std::string &constraint, const std::string &method) {

    PeriodRange per_old = get_prd_range(ts_old);
    PeriodRange per_new;
    per_new.freq = freq_new;

    if ((int) per_new.freq % (int) per_old.freq != 0) {
        Rf_error("The new frequency %d is not an integer multiple "
                "of the input frequency (%d)", freq_new, per_old.freq);
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

     
    for (int col = 0; col < ts_old.ncol(); col++) {
        disagg_spline_single(ts_old(_, col), data(_, col), frac,
                             do_cumul, spline_method, x, y, xnew,
                             ynew, work);
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

void disagg_spline_single(NumericMatrix::Column column_old,
                          NumericMatrix::Column column_new,
                          int frac, bool do_cumul, char spline_method,
                          double x[], double y[], double xnew[],
                          double ynew[], double *work[4]) {
        
    // TODO: take care of leading and trailing NA values
    int n = column_old.size();
    int nnew = column_new.size();

    if (do_cumul) {
        n++;
        nnew++;
    }

    for (int i = 0; i < n; i++) {
       x[i] = i;
    }
    for (int i = 0; i < nnew; i++) {
       xnew[i] = ((double) i) / frac;
    }

    if (do_cumul) {
        y[0] = 0;
        double sum = 0.0;
        for (int i = 1; i < n; i++) {
            sum = sum + column_old[i - 1];
            y[i] = sum;
        }
    } else {
        for (int i = 0; i < n; i++)  {
            y[i] = column_old[i];
        }
    }

    intpol_cspline(n, nnew, x, y, xnew, ynew, spline_method, work);

    if (do_cumul) {
        for (int i = 1; i < nnew; i++) {
            column_new[i - 1] = (ynew[i] - ynew[i - 1]);
        }
    } else {
        for (int i = 0; i < nnew; i++) {
            column_new[i] = ynew[i];
        }
    }
}


