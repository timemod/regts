SEXP create_regts(const SEXP &x, const Rcpp::NumericVector &period_range,
                  const Rcpp::CharacterVector &names);
SEXP create_regts(const SEXP &x, const PeriodRange &per,
                  const Rcpp::CharacterVector &names);
