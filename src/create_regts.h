SEXP create_regts(const Rcpp::NumericMatrix &x, 
                  const Rcpp::NumericVector &period_range,
                  const Rcpp::CharacterVector &names);
SEXP create_regts(const Rcpp::NumericMatrix &x, 
                                 const PeriodRange &per,
                                 const Rcpp::CharacterVector &names);
