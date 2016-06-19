#include <Rcpp.h>
using namespace Rcpp;

class PeriodRange {
public:
    // the first and last subperiod after Christ, starting at 0 in the year 0:
    int first, last;
    int freq;
    int len() {return last - first + 1;}
};

PeriodRange get_period_range(NumericMatrix &ts);
NumericMatrix create_regts(NumericMatrix x, PeriodRange per,
                           CharacterVector names);


// [[Rcpp::export]]
NumericMatrix agg_gr(NumericMatrix ts_old, int freq_new) {

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

    int shift = per_new.first * rep - (rep - 1) - per_old.first;

    for (int col = 0; col < ts_old.ncol(); col++) {
        for (int row = 0; row < nper_new; row++) {
            for (int i = 0; i < rep; i++) {
                for (int j = 0; j < rep; j++) {
                    data(row, col) = data(row, col) +
                         ts_old(i + j + rep * row + shift, col);
                }
            }
        }
    }

    data = data / rep; // cgr method

    NumericMatrix ts_new = create_regts(data, per_new, names);

    // check if labels are present in ts, if so that add to result
    SEXP label_att = ts_old.attr("ts_labels");
    if (!Rf_isNull(label_att)) {
        CharacterVector labels(label_att);
        ts_new.attr("ts_labels") = labels;
    }

    return ts_new;
}

// TODO: create a class for period, return this class

// Returns the PeriodRange of a timeseries.
// TODO: this function is almost a duplicate of the R code of
// package regts. Create a general C++ function for this,
// also to be used in the R code of this package.
PeriodRange get_period_range(NumericMatrix &ts) {
    PeriodRange per;
    NumericVector tsp = ts.attr("tsp");
    per.freq = tsp[2];
    int year = tsp[0] + 100;
    int subp = (tsp[0] - year) * per.freq;
    per.first = year * per.freq + subp;
    per.last = per.first + ts.nrow() - 1;
    return per;
}

NumericMatrix create_regts(NumericMatrix x, PeriodRange per,
                           CharacterVector names) {

    // get start and end vectors

    IntegerVector start(2);
    start[0] = per.first / per.freq;
    start[1] = per.first % per.freq + 1;
    IntegerVector end(2);
    end[0] = per.last / per.freq;
    end[1] = per.last % per.freq + 1;

    CharacterVector classes(4);
    classes[0] = "regts";
    classes[1] = "mts";
    classes[2] = "ts";
    classes[3] = "matrix";

    Environment stats("package:stats");
    Function ts = stats["ts"];

    return ts(x, start, end, per.freq, 1, 0, classes, names);
}
