class PeriodRange {
public:
    // the first and last subperiod after Christ, starting at 0 in the year 0:
    double first, last;
    double freq;
    int len() {return last - first + 1;}
    Rcpp::NumericVector get_period_range();
};

PeriodRange get_prd_range(const Rcpp::NumericMatrix &ts);
PeriodRange get_prd_range(const SEXP &ts);
