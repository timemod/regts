class PeriodRange {
public:
    // the first and last subperiod after Christ, starting at 0 in the year 0:
    double first, last;
    double freq;
    int len() {return last - first + 1;}
    PeriodRange(){}
    PeriodRange(const Rcpp::NumericVector x);
    void modify_frequency(int new_freq);
    Rcpp::NumericVector get_regperiod_range();
};

PeriodRange get_period_range(const Rcpp::NumericMatrix &ts);
PeriodRange get_period_range(const SEXP &ts);
PeriodRange modify_frequency(PeriodRange period_range, int new_freq);
