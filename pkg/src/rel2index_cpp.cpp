#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rel2index_cpp(NumericMatrix &grts, int first_base_row) {

    /* Compute index timeseries x[t] from a growth timeseries grts.
     * grts[t] = (x[t] - x[t-1]) / |X[t-1]|.
     *
     * first_base_row is the row index in grts (zero based) corresponding 
     * to the base period (or the start of the base period if the base 
     * period is a period range).
     */

    int ncol = grts.ncol();
    int nrow = grts.nrow();

    NumericMatrix result(nrow + 1, ncol);

    // initialize matrix with NA
    std::fill(result.begin(), result.end(), NA_REAL);

    double gr;

    for (int col = 0; col < ncol; col++) {

        NumericMatrix::Column col_gr = grts(_, col);
        NumericMatrix::Column col_result = result(_, col);

	/* Find the start_row in grts. 
         * - If first_base_row <= 0, then start_row is 0 
         *   (corresponding to the first row of grts). 
         * - If col_gr[first_base_row] == NA, then start_row is first_base_row
         *   (in that case that timeseries will only contain NA values).
         * - Otherwise, start_row is the first row for which there are no NA
         *   values in col_gr[r] for r in the range row <= r <= first_base_row.
         */
        int start_row;
        if (first_base_row <= 0) {
	    start_row = 0;
        } else {
            start_row = first_base_row;
            for (int row = first_base_row; row >= 0; row--) {
                if (ISNA(col_gr[row])) break;
                start_row = row;
            }
        }

        col_result[start_row] = 1.0;
        for (int row = start_row; row < nrow; row++) {
            gr = col_gr[row];
            if (ISNA(gr)) break;
            if (col_result[row] < 0) gr = -gr;
            col_result[row + 1] = col_result[row] * (1 + gr);
        }
    }

    return result;
}
