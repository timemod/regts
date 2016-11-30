class ParsedPeriod {
public:
    int year, subperiod, freq;
    bool error;
    ParsedPeriod(int year, int subperiod, int freq, bool error) {
        this->year = year;
        this->subperiod = subperiod;
        this->freq = freq;
        this->error = error;
    }
};

ParsedPeriod parse_period(const std::string &period_text, int frequency);
