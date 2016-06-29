class PeriodRange {
public:
    // the first and last subperiod after Christ, starting at 0 in the year 0:
    int first, last;
    int freq;
    int len() {return last - first + 1;}
};

PeriodRange get_period_range(const Rcpp::NumericMatrix &ts);
