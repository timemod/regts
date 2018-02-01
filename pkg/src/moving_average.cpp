#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
    NumericMatrix moving_average(NumericMatrix &x, NumericVector &w, 
                                 const int from, const int to, 
                                 const bool keep_range) {

    // Calculate the moving average of number matrix x.
    // The averaging index run from "from" to "to", where "from" <= 0
    // and "to" >= 0.
    // If  w is a numeric vecor of length > 0, then the weighted
    // moving average is computed. Note that in that case the length
    // of x should be equal to the number of periods summed over (this should be
    // checked in the calling R function). The weights do not have to add
    // up to 1: the weights are normalized automatically.
    
    int nord = to - from + 1;

    int ncol = x.ncol();
    int nrow = x.nrow();

    int nrow_result = keep_range ? nrow : nrow + from - to;
    int row_shift = keep_range ? 0 : from;

    bool weighted = w.size() > 0;

    NumericMatrix result(nrow_result, ncol);

    // For efficiency, separate code is used for the weighted moving average.
    
    if (weighted) {
    
        double *weights = REAL(w);
        double wsum = 0.0;
        for (int  i = 0; i < nord; i++) {
            wsum = wsum + weights[i];
        }
        for (int  i = 0; i < nord; i++) {
            weights[i] = weights[i] / wsum;
        }

        for (int col = 0; col < ncol; col++) {
            NumericMatrix::Column col_old = x(_, col);
            NumericMatrix::Column col_result = result(_, col);
            for (int row = -from; row < nrow - to; row++) {
                int row_result = row + row_shift;
                col_result[row_result] = 0.0;
                for (int k = from; k <= to; k++) {
                    col_result[row_result] = col_result[row_result] + 
                                      col_old[row + k] * weights[k - from];
                }
            }
        }

    } else {

        for (int col = 0; col < ncol; col++) {
            NumericMatrix::Column col_old = x(_, col);
            NumericMatrix::Column col_result = result(_, col);
            for (int row = -from; row < nrow - to; row++) {
                int row_result = row + row_shift;
                col_result[row_result] = 0.0;
                for (int k = from; k <= to; k++) {
                    col_result[row_result] = col_result[row_result] + 
                                              col_old[row + k];
                }
                col_result[row_result] = col_result[row_result] / nord;
            }
        }
    }

    if (keep_range) {
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
