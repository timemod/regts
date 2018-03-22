#include <Rcpp.h>
#include <stdlib.h>
#include <sstream>
#include <algorithm>

using namespace Rcpp;

#define NWEIRD_MAX 10

//  Tnternal function used in function read_ts_xlsx.
//  Converts a tibble for which each column is a List
//  to a numeric matrix. For efficiency this function has been implemented
//  in C++.
// [[Rcpp::export]]
NumericMatrix list_tbl_2_mat(List tbl) {

    int ncols = tbl.size();
    if (ncols == 0) return R_NilValue;

    List col = tbl[0];
    int nrows = col.size();

    double na_val = NumericVector::get_na();
    double val;
    char *txt_p;

    NumericMatrix mat(nrows, ncols);

    int error_count = 0;
    const char *weird_texts[NWEIRD_MAX];

    for (int colnr = 0; colnr < ncols; colnr++) {
        List col = tbl[colnr];
        for (int rownr = 0; rownr < nrows; rownr++) {
            SEXP obj = col[rownr];
            switch (TYPEOF(obj)) {
                case INTSXP:
                case REALSXP:
                case LGLSXP:
                    val = as<double>(obj);
                    break;
                case STRSXP:
                    {
                        const char *txt = CHAR(STRING_ELT(obj, 0));
                        // Note that strtod is locale-dependent.
                        // this could lead to different behaviour on
                        // different platforms. It would be better
                        // to use a locale-independent version.
                        val = strtod(txt, &txt_p);
                        if (*txt_p != '\0') {
                            // not a number
                            if (error_count < NWEIRD_MAX) {
                                weird_texts[error_count] = txt;
                            }
                            error_count++;
                            val = na_val;
                        }
                    }
                    break;
                default:
                    // this situation can probably never occur
                    val = na_val;
            }
            mat(rownr, colnr) = val;
        }
    }

    if (error_count > 0) {
        // TODO: only print unique texts, to prevent many double texts
        std::stringstream ss;
        ss << "NAs introduced by coercion" << std::endl;
        if (error_count <= NWEIRD_MAX) {
            ss << "The following texts could not be converted to numeric:";
        } else {
            ss << error_count << " texts could not be converted to numeric:";
            ss << std::endl;
            ss << "The first " << NWEIRD_MAX << " texts that gave problems are";
        } 
        for (int i = 0; i < std::min(error_count, NWEIRD_MAX); i++) {
            ss << std::endl << "\"" << weird_texts[i] << "\"";
        }
        Rf_warning(ss.str().c_str());
    }

    return mat;
}
