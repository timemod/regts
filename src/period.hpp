/* Constants for periodicity. These number should be consistent with
 * the definition of ULT_PERIODICITY in src/incl/ult_date.tsl 
 */
#define FREQ_UNKNOWN  0

void set_period_text(const std::string &period_text);
int prlex(void);
void prerror(const char *s);
void prrestart(FILE *s);
