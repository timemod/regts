#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix moving_average(NumericMatrix &x, const int max_lag,
                             const int max_lead, const bool na_pad) {

    // Calculate the moving average of number matrix x.
    // The avering number runs from index "from" to "to", where
    // "from" <= 0 and "to >= 0".
    
    int nord = to - from + 1;

    int ncol = x.ncol();
    int nrow = x.nrow();

    int nrow_result = na_pad ? nrow : nrow - from + to;
    int row_shift = na_pad ? 0 : from;

    NumericMatrix result(nrow_result, ncol);

    for (int col = 0; col < ncol; col++) {
        NumericMatrix::Column col_old = x(_, col);
        NumericMatrix::Column col_result = result(_, col);
        for (int row = -from; row < nrow - to; row++) {
            col_result[row] = 0.0;
            for (int k = from; k <= to; k++) {
                col_result[row + row_shift] = col_result[row + row_shift] + 
                                              col_old[row + k];
            }
            col_result[row + row_shift] = col_result[row + row_shift] / nord;
        }
    }

    if (na_pad) {
        for (int col = 0; col < ncol; col++) {
            NumericMatrix::Column col_result = result(_, col);
            for (int row = 0; row < -from; row++) {
                col_result[row] = NA_REAL;
            }
            for (int row = nrow - to; row < nrow; row++) {
                col_result[row] = NA_REAL;
            }
        }
    }

    return result;
}
