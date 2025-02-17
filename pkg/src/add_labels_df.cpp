#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
/* Add labels to the columns of a data frame.
 * The data frame is modified in place. The function returns NULL */
SEXP add_labels_df(List &df, CharacterVector &labels) {

  int n = df.size();

  if (labels.size() != n) {
    stop("Internal error: length of labels vector does not match the "
         "number of columns in df.");
  }

  for (int i = 0; i < n; i++) {
    RObject col = df[i];  // Extract column as an RObject
    col.attr("label") = as<std::string>(labels[i]); //  Set label
  }

  return R_NilValue;  // Return NULL
}
