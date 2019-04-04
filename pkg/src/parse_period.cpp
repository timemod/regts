#include <Rcpp.h>
#include <string>
#include "parse_period.h"
using Rcpp::NumericVector;
using Rcpp::CharacterVector;
using Rcpp::LogicalVector;
using std::string;

static string trim(const string& str);
static void parse_single_period(const std::string &period_text, 
                                const double given_freq,
                                double &subperiods, double &freq);

// [[Rcpp::export]]
NumericVector parse_period(const std::string &period_text, double frequency) {

    double per, f;
    parse_single_period(period_text, frequency, per, f);

    NumericVector result(1);
    result[0] = per;
    result.attr("class") = "period";
    result.attr("frequency") = f;
	return result;
}


// [[Rcpp::export]]
NumericVector parse_period_range(const std::string &period_text,
                                    double frequency) {

    double p1, p2, f;
    p1 = NA_REAL;
    p2 = NA_REAL;
    f  = NA_REAL;

    const size_t pos = period_text.find("/");
    if (pos == string::npos) {
        // no / separator
        parse_single_period(period_text, frequency, p1, f);
        p2 = p1;
    } else {
         // a / seperator has been found
        static string s1, s2;
        s1 = period_text.substr(0, pos);
        s2 = period_text.substr(pos + 1);
        s1 = trim(s1);
        s2 = trim(s2);
        bool both = s1.size() > 0 && s2.size() >> 0;
        if (s1.size() == 0 && s2.size() == 0) {
            Rf_error("Illegal period range %s.", period_text.c_str());
        }
        double f1 = 0.0, f2 = 0.0;
        if (s1.size() > 0) {
            parse_single_period(s1, frequency, p1, f);
            f1 = f; // for checking
        }
        if (s2.size() > 0) {
            parse_single_period(s2, frequency, p2, f);
            f2 = f; // for checking
        }
        if (both) {
            if (f1 != f2) {
               Rf_error("The two periods have different frequency");
            } else if (p1 > p2) {
               Rf_error("The start period (%s) is after the end period (%s)",
                s1.c_str(), s2.c_str());
            }
        }
    }

    NumericVector result(3);
    result[0] = p1;
    result[1] = p2;
    result[2] = f;
    result.attr("class") = "period_range";
	return result;
}

static void parse_single_period(const std::string &period_text, 
                                const double given_freq,
                                double &subperiods, double &freq) {
    
    subperiods = NA_REAL;
    freq       = NA_REAL;
    ParsedPeriod per = parse_period_text(period_text, given_freq);

    if (per.error) {
        Rf_error("Illegal period %s.", period_text.c_str());
    } else {
        freq = per.freq;
        if (ISNA(per.freq)) {
            if (ISNA(given_freq)) {
                Rf_error("Frequency of period %s unknown."
                         " Specify argument frequency.", period_text.c_str());
                return;
            }
	        freq = given_freq;;
            if (per.subperiod > freq) {
                Rf_error("Subperiod of period %s is larger than the"
                         " specified frequency %d", period_text.c_str(),
                         (int) given_freq);
            }
	    } else if (!ISNA(given_freq) && per.freq != given_freq) {
	        Rf_error("Specified frequency %d does not agree with actual "
                     "frequency in period %s.", (int) given_freq,
                     period_text.c_str());
            return;
	    }
        subperiods = per.year * freq + per.subperiod - 1;
    }
}

// [[Rcpp::export]]
LogicalVector is_period_text(std::vector<std::string> strings,
                             const double frequency) {
     int n = strings.size();
     LogicalVector out(n);
     for (int i = 0; i < n; i++) {
          ParsedPeriod per = parse_period_text(strings[i], frequency);
          out[i] = !per.error;
          if (per.error) {
              continue;
          }
          if (!ISNA(frequency)) {
              if (ISNA(per.freq)) {
                  per.freq = frequency;
              } else if (per.freq != frequency) {
                  /* the given frequency does not agree with the actual
                   * frequency */
                  out[i] = false;
              }
          }
          if (!ISNA(per.freq) && per.subperiod > per.freq) {
              out[i] = false;
          }
     }
     return out;
}

static string trim(const string& str) {
    const string whitespace = " \t";
    const size_t strBegin = str.find_first_not_of(whitespace);
    if (strBegin == string::npos) {
        return ""; // no content
    }
    const size_t strEnd = str.find_last_not_of(whitespace);
    const size_t strRange = strEnd - strBegin + 1;
    return str.substr(strBegin, strRange);
}
