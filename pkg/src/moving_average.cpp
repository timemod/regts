#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix moving_average(NumericMatrix &x, const int max_lag,
                             const int max_lead, const bool keep_range) {

    // Calculate the moving average of number matrix x.
    // The avering number runs from index -max_lead to max_lag.
    
    int nord = max_lag + max_lead + 1;

    int ncol = x.ncol();
    int nrow = x.nrow();

    int nrow_result = keep_range ? nrow : nrow + max_lag + max_lead;
    int row_shift = keep_range ? 0 : -max_lag;

    NumericMatrix result(nrow_result, ncol);

    for (int col = 0; col < ncol; col++) {
        NumericMatrix::Column col_old = x(_, col);
        NumericMatrix::Column col_result = result(_, col);
        for (int row = max_lag; row < nrow - max_lead; row++) {
            col_result[row] = 0.0;
            for (int k = -max_lag; k <= max_lead; k++) {
                col_result[row + row_shift] = col_result[row + row_shift] + 
                                              col_old[row + k];
            }
            col_result[row + row_shift] = col_result[row + row_shift] / nord;
        }
    }

    if (keep_range) {
        for (int col = 0; col < ncol; col++) {
            NumericMatrix::Column col_result = result(_, col);
            for (int row = 0; row < max_lag; row++) {
                col_result[row] = NA_REAL;
            }
            for (int row = nrow - max_lead; row < nrow; row++) {
                col_result[row] = NA_REAL;
            }
        }
    }

    return result;
}
