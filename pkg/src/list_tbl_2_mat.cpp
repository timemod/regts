#include <Rcpp.h>
#include <sstream>
#include <algorithm>
#include <set>
#include <string>

using namespace Rcpp;

#define NWEIRD_MAX 10

//  Tnternal function used in function read_ts_xlsx.
//  Converts a "list tibble" (a tibble for which each column is a list)
//  to a numeric matrix. For efficiency this function has been implemented in C++.
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

    std::set<std::string> weird_texts;

    // boolean more_weird_texts will be set to true if more weird
    // texts than NWEIRD_MAX were found
    bool more_weird_texts = false;

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
                        // Note that strtod is locale dependent.
                        val = strtod(txt, &txt_p);
                        if (*txt_p != '\0') {
                            // not a number
                            if (!more_weird_texts) {
                                if (weird_texts.size() < NWEIRD_MAX) {
                                    weird_texts.insert(txt);
                                } else {
                                    more_weird_texts = weird_texts.find(txt) != 
                                                       weird_texts.end();
                                }
                            }
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

    int nweird = weird_texts.size();

    if (nweird > 0) {
        std::stringstream ss;
        ss << "NAs introduced by coercion" << std::endl;
        if (nweird <= NWEIRD_MAX) {
            ss << "The following texts could not be converted to numeric:";
        } else {
            ss << "Some texts could not be converted to numeric:";
            ss << std::endl;
            ss << "The first " << nweird << " texts that gave problems are";
        } 
        std::vector<std::string> txts(weird_texts.begin(), weird_texts.end());
        std::sort(txts.begin(), txts.end());
        for (int i = 0; i < nweird; i++) {
            ss << std::endl << "\"" << txts[i] << "\"";
        }
        Rf_warning(ss.str().c_str());
    }

    return mat;
}
