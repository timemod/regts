#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix agg_gr(NumericMatrix x, int rep, int first_old,
                     int first_new) {

    int last_old = first_old + x.nrow() - 1;
    int last_new = (last_old - (rep - 1)) / rep;
    int nper_new = last_new - first_new + 1;

    NumericMatrix result(nper_new, x.ncol());

    int shift = first_new * rep - (rep - 1) - first_old;
    
    for (int col = 0; col < x.ncol(); col++) {    
        for (int row = 0; row < nper_new; row++) {
            for (int i = 0; i < rep; i++) {
                for (int j = 0; j < rep; j++) {
                    result(row, col) = result(row, col) + 
                         x(i + j + rep * row + shift, col);
                }
            }
        }
    }

    return result / 4;
}
