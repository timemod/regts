#include <Rcpp.h>
#include "period_range.h"
#include "create_regts.h"
using namespace Rcpp;

NumericMatrix create_regts(const NumericMatrix &x, const PeriodRange &per,
                           const CharacterVector &names) {

    // get start and end vectors

    IntegerVector start(2);
    start[0] = ((int) per.first) / per.freq;
    start[1] =( (int) per.first) % per.freq + 1;
    IntegerVector end(2);
    end[0] = ((int) per.last) / per.freq;
    end[1] = ((int) per.last) % per.freq + 1;

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
