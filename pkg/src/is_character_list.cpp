#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector is_character_list(List l) {
    
    int n = l.size();

    LogicalVector ret(n);

    for (int i = 0; i < n; i++) {
        SEXP obj = l[i];
        ret[i] = TYPEOF(obj) == STRSXP;
    }

    return ret;
}
