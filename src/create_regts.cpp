#include <Rcpp.h>
#include "period_range.h"
#include "create_regts.h"
using namespace Rcpp;

// [[Rcpp::export]]
SEXP create_regts(const NumericMatrix &x, const NumericVector &period_range,
                  const CharacterVector &names) {
    PeriodRange per;
    per.first = period_range[0];
    per.last  = period_range[1];
    per.freq  = period_range[2];
    return create_regts(x, per, names);
}

SEXP create_regts(const NumericMatrix &x, const PeriodRange &per,
                  const CharacterVector &names) {

    // get start and end vectors

    IntegerVector start(2);
    start[0] = ((int) per.first) / ((int) per.freq);
    start[1] = ((int) per.first) % ((int) per.freq) + 1;
    IntegerVector end(2);
    end[0] = ((int) per.last) / ((int) per.freq);
    end[1] = ((int)per.last) % ((int) per.freq) + 1;

    // Return a CharacterVector with the class names of the return value.
    CharacterVector classes;
    classes.push_back("regts");
    if (x.ncol() > 1) {
        classes.push_back("mts");
        classes.push_back("ts");
        classes.push_back("matrix");
    } else {
        classes.push_back("ts");
    }

    // Call R function ts to create a regts.
    Environment stats("package:stats");
    Function ts = stats["ts"];
    return ts(x, start, end, per.freq, 1, 0, classes, names);
}
