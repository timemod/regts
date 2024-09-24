#include <Rcpp.h>
#include <math.h>
#include <string>
#include "period_range.h"
using namespace Rcpp;

void agg_gr_abs(NumericMatrix::Column column_old,
                NumericMatrix::Column column_new, int rep, int shift);
void agg_gr_rel(NumericMatrix::Column column_old,
                NumericMatrix::Column column_new, double work[], int rep,
                int shift, int perc);

// [[Rcpp::export]]
List agg_gr(NumericMatrix ts_old, const int freq_new,
            const std::string &method) {

    PeriodRange per_old = get_prd_range(ts_old);

    int freq_old = (int) per_old.freq;

    if (freq_new > freq_old) {
        stop("The new frequency %d is higher than the old frequency "
             "%d.", freq_new, freq_old);
    }

    if (freq_old % (int) freq_new != 0) {
        stop("The new frequency %d is not a divisor of the old frequency "
             "%d.", freq_new, freq_old);
    }

    PeriodRange per_new;
    per_new.freq = freq_new;
    int rep = freq_old / freq_new;
    per_new.first = (((int) per_old.first) + 2 * (rep - 1)) / rep;
    per_new.last  = (((int) per_old.last) - (rep - 1)) / rep;
    int nper_new = per_new.len();

    if (nper_new <= 0) {
        stop("Cannot perform aggregation because the input timeseries\n"
             "contains too few observations.");
    }

    NumericMatrix data(nper_new, ts_old.ncol());

    int shift = per_new.first * rep - rep - per_old.first;
    if (method == "difmean" || method == "difsum") {
        for (int col = 0; col < ts_old.ncol(); col++) {
            agg_gr_abs(ts_old(_, col), data(_, col), rep, shift);
        }
        if (method == "difmean") {
            data = data / rep;
        }
    } else if (method == "rel" || method == "pct") {
        double *work = new double[2 * rep];
        int perc = method == "rel" ? 1 : 100;
        for (int col = 0; col < ts_old.ncol(); col++) {
            agg_gr_rel(ts_old(_, col), data(_, col), work, rep, shift, perc);
        }
        delete[] work;
    } else {
        stop((std::string("Illegal aggregation method ") + method).c_str());
    }

    List result(2);
    result[0] = data;
    result[1] = per_new.get_period_range();
    return (result);
}

void agg_gr_abs(NumericMatrix::Column column_old,
                NumericMatrix::Column column_new, int rep, int shift) {
    for (int row = 0; row < column_new.size(); row++) {
        for (int i = 1; i < rep; i++) {
            column_new[row] = column_new[row] + i *
                              column_old[i + rep * row + shift];
        }
        for (int i = 0; i < rep; i++) {
            column_new[row] = column_new[row] + (rep - i) *
                              column_old[i + rep + rep * row + shift];
        }
    }
}

// The macro CONVERT_INVALID converts an invalid number
// (Inf, -Inf, NaN or NA) to either NA_REAL or NAN
#define CONVERT_INVALID(X) ISNA(X) ? NA_REAL : NAN

void agg_gr_rel(NumericMatrix::Column column_old,
                NumericMatrix::Column column_new, double work[], int rep,
                int shift, int perc) {

    bool na_found = false;
    double xtot1 = 0.0, xtot2 = 0.0, help1 = 0.0, help2 = 0.0;
    for (int row = 0; row < column_new.size(); row++) {
        if (row == 0 || na_found) {
            na_found = false;
            work[0] = 1.0;
            xtot1 = 1.0;
            xtot2 = 0.0;
            for (int j = 1; j < rep; j++) {
                double x_old = column_old[j + shift + rep * row];
                if (!R_FINITE(x_old)) {
                    column_new[row] = CONVERT_INVALID(x_old);
                    na_found = true;
                    goto next_row;
                }
                work[j] = work[j - 1] * (x_old / perc + 1);
                xtot1 = xtot1 + work[j];
            }
            help1 = work[rep - 1];
        } else {
            // for row > 0 results are kept in help1 and help2 to be used in
            // next round
            xtot1 = xtot2 / help1;
            help1 = work[rep - 1];
            xtot2 = 0.0;
            work[rep - 1] = help2;
        }
        for (int j = rep; j < 2 * rep; j++) {
            double x_old = column_old[j + shift + rep * row];
            if (!R_FINITE(x_old)) {
                column_new[row] = CONVERT_INVALID(x_old);
                na_found = true;
                goto next_row;
            }
            work[j] = work[j - 1] * (x_old / perc + 1);
            xtot2 = xtot2 + work[j];
        }
        help2 = work[2 * rep - 1] / help1;
        column_new[row] = perc * (xtot2 / xtot1 - 1);

        next_row:
            continue;
    }
}
