#include <Rcpp.h>
using namespace Rcpp;

int get_frequency(NumericMatrix ts);
int get_first_period(NumericMatrix ts);
NumericMatrix create_ts(NumericMatrix x, int first, int last, int freq);

// [[Rcpp::export]]
NumericMatrix agg_gr(NumericMatrix ts, int freq_new) {

    int freq_old = get_frequency(ts);
    int rep = freq_old / freq_new;
    int first_old = get_first_period(ts);
    int first_new = (first_old + 2 * (rep - 1)) / rep;
    int last_old = first_old + ts.nrow() - 1;
    int last_new = (last_old - (rep - 1)) / rep;
    int nper_new = last_new - first_new + 1;

    NumericMatrix result(nper_new, ts.ncol());

    int shift = first_new * rep - (rep - 1) - first_old;
    
    for (int col = 0; col < ts.ncol(); col++) {    
        for (int row = 0; row < nper_new; row++) {
            for (int i = 0; i < rep; i++) {
                for (int j = 0; j < rep; j++) {
                    result(row, col) = result(row, col) + 
                         ts(i + j + rep * row + shift, col);
                }
            }
        }
    }
    
    result = result / 4;
    result = create_ts(result, first_new, last_new, freq_new);
    return result;
}

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

NumericMatrix create_ts(NumericMatrix x, int first, int last, int freq) {
    Environment stats("package:stats");
    Function ts = stats["ts"];
    NumericVector start(2);
    start[0] = first / freq;
    start[1] = first % freq + 1;
    NumericVector end(2);
    end[0] = last / freq;
    end[1] = last % freq + 1;
    return ts(x, start, end, freq);
}
