#include <string>
#include <map>
#include "months.h"
using std::map;
using std::string;

static bool init = true;
static map<string, int> months;

static void init_months() {
    months["jan"] = 1;
    months["feb"] = 2;
    months["mar"] = 3;
    months["apr"] = 4;
    months["may"] = 5;
    months["jun"] = 6;
    months["jul"] = 7;
    months["aug"] = 8;
    months["sep"] = 9;
    months["oct"] = 10;
    months["nov"] = 11;
    months["dec"] = 12;
}


// Returns the number of a month name. The function returns -1 if the
// month name is not recognized */
int get_month_number(const char *name) {
    if (init) {
        init_months();
        init = false;
    }
    const std::map<string,int>::iterator iter = months.find(name);
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
