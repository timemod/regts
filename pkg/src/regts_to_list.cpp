#include <Rcpp.h>

using namespace Rcpp;

template <class T>
List regts_to_list_templ (T &ts) {
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
       Vector single_ts = ts(_, i);
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

// TODO: maybe conversion to a general matrix type possible?
List regts_to_list(SEXP ts) {
    switch (TYPEOF(ts)) {
        case INTSXP: 
             {
             Rcpp::IntegerMatrix ts_int = Rcpp::as <Rcpp::IntegerMatrix>(ts);
             return regts_to_list_templ(ts_int);
	     }
        case REALSXP: 
	     {
             Rcpp::NumericMatrix ts_num = Rcpp::as <Rcpp::NumericMatrix>(ts);
             return regts_to_list_templ(ts_num);
	     }
        case LGLSXP: 
	     {
             Rcpp::LogicalMatrix ts_lgl = Rcpp::as <Rcpp::LogicalMatrix>(ts);
             return regts_to_list_templ(ts_lgl);
	     }
        case CHARSXP: 
	     {
             Rcpp::CharacterMatrix ts_char = Rcpp::as <Rcpp::CharacterMatrix>(ts);
             return regts_to_list_templ(ts_char);
	     }
        default: Rcpp::stop ("Illegal matrix type"); 
    }
}

// [[Rcpp::export]]
List regts_to_list_rcpp(SEXP &ts) {
    return regts_to_list(ts);
}
