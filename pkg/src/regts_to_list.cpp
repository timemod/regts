#include <Rcpp.h>

using namespace Rcpp;

template <class T>
List regts_to_list_templ (T &ts) {
   List dimnames = ts.attr("dimnames");
   CharacterVector colnames = dimnames[1];
   NumericVector tsp = ts.attr("tsp");
   bool has_labels = ts.hasAttribute("ts_labels");
   CharacterVector labels;
   if (has_labels) labels = ts.attr("ts_labels");
   CharacterVector classes = CharacterVector::create("regts", "ts");
   int n = ts.ncol();
   List retval(n);
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

List regts_to_list(SEXP &ts) {
    switch (TYPEOF(ts)) {
        case INTSXP: 
             {
             IntegerMatrix ts_int = as <IntegerMatrix>(ts);
             return regts_to_list_templ(ts_int);
	     }
        case REALSXP: 
	     {
             NumericMatrix ts_num = as <NumericMatrix>(ts);
             return regts_to_list_templ(ts_num);
	     }
        case CPLXSXP: 
	     {
             ComplexMatrix ts_cplx = as <ComplexMatrix>(ts);
             return regts_to_list_templ(ts_cplx);
	     }
        case LGLSXP: 
	     {
             LogicalMatrix ts_lgl = as <LogicalMatrix>(ts);
             return regts_to_list_templ(ts_lgl);
	     }
        case STRSXP: 
	     {
             CharacterMatrix ts_char = as <CharacterMatrix>(ts);
             return regts_to_list_templ(ts_char);
	     }
        default: stop ("Unknown timeseries element type"); 
    }
}

// [[Rcpp::export]]
List regts_to_list_rcpp(SEXP &ts) {
    return regts_to_list(ts);
}
