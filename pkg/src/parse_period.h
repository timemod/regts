class ParsedPeriod {
public:
    double year, subperiod, freq;
    bool error;
    ParsedPeriod(double year, double subperiod, double freq, bool error) {
        this->year = year;
        this->subperiod = subperiod;
        this->freq = freq;
        this->error = error;
    }
};

ParsedPeriod parse_period_text(const std::string &period_text, double frequency);
