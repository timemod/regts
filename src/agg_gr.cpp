#include <Rcpp.h>
using namespace Rcpp;

int get_frequency(NumericMatrix ts);
int get_first_period(NumericMatrix ts);
NumericMatrix create_ts(NumericMatrix x, int first, int last, int freq,
                       CharacterVector names);

// [[Rcpp::export]]
NumericMatrix agg_gr(NumericMatrix ts_old, int freq_new) {

    // save the dimension names
    List dimnames = ts_old.attr("dimnames");
    CharacterVector names = dimnames[1];

    int freq_old = get_frequency(ts_old);
    int rep = freq_old / freq_new;
    int first_old = get_first_period(ts_old);
    int first_new = (first_old + 2 * (rep - 1)) / rep;
    int last_old = first_old + ts_old.nrow() - 1;
    int last_new = (last_old - (rep - 1)) / rep;
    int nper_new = last_new - first_new + 1;

    if (freq_old % freq_new != 0) {
        Rf_error("The new frequency %d is not a divisor of the old frequency "
                 "%d\n", freq_new, freq_old);
    }
    if (nper_new <= 0) {
        Rf_error("Cannot perform aggregation because the input timeseries "
                "contains too few observations");
    }

    NumericMatrix data(nper_new, ts_old.ncol());

    int shift = first_new * rep - (rep - 1) - first_old;

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

    NumericMatrix ts_new = create_ts(data, first_new, last_new, freq_new,
                       names);

    // check if labels are present in ts, if so that add to result
    SEXP label_att = ts_old.attr("ts_labels");
    if (!Rf_isNull(label_att)) {
        CharacterVector labels(label_att);
        ts_new.attr("ts_labels") = labels;
    }

    return ts_new;
}

// TODO: create a class for period, return this class

// Returns the first period of timeseries ts as the number
// of subperiods after Christ.
// TODO: this function is almost a duplicate of the R code of
// package regts. Create a general C++ function for this,
// also to be used in the R code of this package.
int get_first_period(NumericMatrix ts) {

    NumericVector tsp = ts.attr("tsp");
    int freq = tsp[2];
    int year = tsp[0];
    int subp = (tsp[0] - year) * freq;
    return year * freq + subp;
}

// Returns the frequency of the timeseries
int get_frequency(NumericMatrix ts) {
    NumericVector tsp = ts.attr("tsp");
    int freq = tsp[2];
    return freq;
}

NumericMatrix create_ts(NumericMatrix x, int first, int last, int freq,
                        CharacterVector names) {

    // get start and end vectors
    Environment stats("package:stats");
    Function ts = stats["ts"];
    NumericVector start(2);
    start[0] = first / freq;
    start[1] = first % freq + 1;
    NumericVector end(2);
    end[0] = last / freq;
    end[1] = last % freq + 1;

    //
    CharacterVector classes(4);
    classes[0] = "regts";
    classes[1] = "mts";
    classes[2] = "ts";
    classes[3] = "matrix";

    return ts(x, start, end, freq, 1, 0, classes, names);
}
