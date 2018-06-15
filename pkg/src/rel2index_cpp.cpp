#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rel2index_cpp(NumericMatrix &grts) {

    /* Compute index timeseries from a growth timeseries grts.
     * (grts[t] = X[t] - X_[t-1] / |X[t-1]|.
     * base_index is the period index of the index series that should
     * have the value 100, using index base 1 as in R! */

    int ncol = grts.ncol();
    int nrow = grts.nrow();

    NumericMatrix result(nrow + 1, ncol);

    // initialize matrix with NA
    std::fill(result.begin(), result.end(), NA_REAL);

    double gr;

    for (int col = 0; col < ncol; col++) {
        NumericMatrix::Column col_gr = grts(_, col);
        NumericMatrix::Column col_result = result(_, col);
        col_result[0] = 1.0;
        for (int row = 0; row < nrow; row++) {
            gr = col_gr[row];
            if (ISNA(gr)) break;
            if (col_result[row] < 0) gr = -gr;
            col_result[row + 1] = col_result[row] * (1 + gr);
        }
    }
    return result;
}
