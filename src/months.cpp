#include <string>
#include <map>
#include <algorithm>
#include "months.hpp"
using std::map;
using std::string;

static const map<string, int> months {
    {"jan"       , 1},
    {"feb"       , 2},
    {"mar"       , 3},
    {"apr"       , 4},
    {"may"       , 5},
    {"jun"       , 6},
    {"jul"       , 7},
    {"aug"       , 8},
    {"sep"       , 9},
    {"oct"       , 10},
    {"nov"       , 11},
    {"dec"       , 12},
    {"mrt"       , 3},
    {"mei"       , 5},
    {"okt"       , 10}
};

// Returns the number of a month name. The function returns -1 if the
// month name is not recognized */
int get_month_number(const char *name) {

    // covert *name to string and to lowercase
    string mname = string(name);
    std::transform(mname.begin(), mname.end(), mname.begin(), ::tolower);

    const auto iter = months.find(mname);
    if (iter != months.end()) {
        return iter->second;
    } else {
        return -1;
    }
}

#ifdef TEST_MONTHS
#include<iostream>
int main() {
    std::cout << get_month_number("may") << std::endl;
}
#endif
