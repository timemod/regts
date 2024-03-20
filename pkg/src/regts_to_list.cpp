#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List regts_to_list(NumericMatrix ts) {
   Rcpp::List dimnames = ts.attr("dimnames");
   CharacterVector colnames = dimnames[1];
   NumericVector tsp = ts.attr("tsp");
   CharacterVector classes = CharacterVector::create("regts", "ts");
   bool has_labels = ts.hasAttribute("ts_labels");
   CharacterVector labels;
   if (has_labels) labels = ts.attr("ts_labels");
   int n = ts.ncol();
   Rcpp::List retval(n);
   for (int i = 0; i < n; i++) {
       NumericVector single_ts = ts(_, i);
       single_ts.attr("tsp") = tsp;
       single_ts.attr("class") = classes;
       if (has_labels) {
           single_ts.attr("ts_labels") = as<std::string>(labels[i]);
       }
       retval[i] = single_ts;
   }
   retval.attr("names") = colnames;
   return retval;
}
