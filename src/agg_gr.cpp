#include <Rcpp.h>
#include <string>
using namespace Rcpp;

class PeriodRange {
public:
    // the first and last subperiod after Christ, starting at 0 in the year 0:
    int first, last;
    int freq;
    int len() {return last - first + 1;}
};

PeriodRange get_period_range(const NumericMatrix &ts);
NumericMatrix create_regts(const NumericMatrix &x, const PeriodRange &per,
                           const CharacterVector &names);
void agg_gr_abs(NumericMatrix::Column column_old,
                NumericMatrix::Column column_new, int rep, int shift);
void agg_gr_rel(NumericMatrix::Column column_old,
                NumericMatrix::Column column_new, double work[], int rep,
                int shift, int perc);

// [[Rcpp::export]]
NumericMatrix agg_gr(NumericMatrix &ts_old, const int freq_new,
                     const std::string &method) {
    //ts_old = ts_old * 2;
    // save the dimension names
    List dimnames = ts_old.attr("dimnames");
    CharacterVector names = dimnames[1];
    PeriodRange per_old = get_period_range(ts_old);
    PeriodRange per_new;
    per_new.freq = freq_new;
    int rep = per_old.freq / freq_new;
    per_new.first = (per_old.first + 2 * (rep - 1)) / rep;
    per_new.last = (per_old.last - (rep - 1)) / rep;
    int nper_new = per_new.len();

    if (per_old.freq % freq_new != 0) {
        Rf_error("The new frequency %d is not a divisor of the old frequency "
                 "%d\n", freq_new, per_old.freq);
    }
    if (nper_new <= 0) {
        Rf_error("Cannot perform aggregation because the input timeseries "
                "contains too few observations");
    }

    NumericMatrix data(nper_new, ts_old.ncol());

    if (method == "cgr" || method == "cgrs") {
        int shift = per_new.first * rep - (rep - 1) - per_old.first;
        for (int col = 0; col < ts_old.ncol(); col++) {
            agg_gr_abs(ts_old(_, col), data(_, col), rep, shift);
        }
        if (method == "cgr") {
            data = data / rep;
        }
    } else if (method == "cgru" || method == "cgrc") {
        int shift = per_new.first * rep - rep  - per_old.first;
        double work[2*rep];
        int perc = method == "cgru" ? 1 : 100;
        for (int col = 0; col < ts_old.ncol(); col++) {
            agg_gr_rel(ts_old(_, col), data(_, col), work, rep, shift, perc);
        }
    } else {
        Rf_error((std::string("Illegal aggregation method ") + method).c_str());
    }

    NumericMatrix ts_new = create_regts(data, per_new, names);

    // check if labels are present in ts, if so that add to result
    SEXP label_att = ts_old.attr("ts_labels");
    if (!Rf_isNull(label_att)) {
        CharacterVector labels(label_att);
        ts_new.attr("ts_labels") = labels;
    }

    return ts_new;
}

void agg_gr_abs(NumericMatrix::Column column_old,
                NumericMatrix::Column column_new, int rep, int shift) {
    for (int row = 0; row < column_new.size(); row++) {
        for (int i = 0; i < rep; i++) {
            for (int j = 0; j < rep; j++) {
                column_new[row] = column_new[row] +
                              column_old[i + j + rep * row + shift];
            }
        }
    }
}

void agg_gr_rel(NumericMatrix::Column column_old,
                NumericMatrix::Column column_new, double work[], int rep,
                int shift, int perc) {

    bool na_found = false;
    double xtot1, xtot2, help1, help2;
    for (int row = 0; row < column_new.size(); row++) {
        if (row == 0 || na_found) {
            na_found = false;
            work[0] = 1.0;
            xtot1 = 1.0;
            xtot2 = 0.0;
            for (int j = 1; j < rep; j++) {
                if (!R_FINITE(column_old[j + shift + rep * row])) {
                    if (ISNA(column_old[j + shift + rep * row])) {
                        column_new[row] = NA_REAL;
                    } else {
                        column_new[row] = NAN;
                    }
                    na_found = true;
                    goto next_row;
                }
                work[j] = work[j - 1] *
                      (column_old[j + shift + rep * row] / perc + 1);
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
            if (!R_FINITE(column_old[j + shift + rep * row])) {
                if (ISNA(column_old[j + shift + rep * row])) {
                    column_new[row] = NA_REAL;
                } else {
                    column_new[row] = NAN;
                }
                na_found = true;
                goto next_row;
            }
            work[j] = work[j - 1] *
                (column_old[j + shift + row * rep] / perc + 1);
            xtot2 = xtot2 + work[j];
        }
        help2 = work[2 * rep - 1] / help1;
        column_new[row] = perc * (xtot2 / xtot1 - 1);
    next_row:
        continue;
    }
}


// Returns the PeriodRange of a timeseries.
// TODO: this function is almost a duplicate of the R code of
// package regts. Create a general C++ function for this,
// also to be used in the R code of this package.
PeriodRange get_period_range(const NumericMatrix &ts) {
    PeriodRange per;
    NumericVector tsp = ts.attr("tsp");
    per.freq = tsp[2];
    int year = tsp[0] + 100;
    int subp = (tsp[0] - year) * per.freq;
    per.first = year * per.freq + subp;
    per.last = per.first + ts.nrow() - 1;
    return per;
}

NumericMatrix create_regts(const NumericMatrix &x, const PeriodRange &per,
                           const CharacterVector &names) {

    // get start and end vectors

    IntegerVector start(2);
    start[0] = per.first / per.freq;
    start[1] = per.first % per.freq + 1;
    IntegerVector end(2);
    end[0] = per.last / per.freq;
    end[1] = per.last % per.freq + 1;

    Environment stats("package:stats");
    Function ts = stats["ts"];

    // TODO: could this code not be simplified?
    if (x.ncol() > 1) {
        CharacterVector classes(4);
        classes[0] = "regts";
        classes[1] = "mts";
        classes[2] = "ts";
        classes[3] = "matrix";
        return ts(x, start, end, per.freq, 1, 0, classes, names);
    } else {
        CharacterVector classes(2);
        classes[0] = "regts";
        classes[1] = "ts";
        return ts(x, start, end, per.freq, 1, 0, classes, names);
    }


}
