/* Constants for periodicity. These number should be consistent with
 * the definition of ULT_PERIODICITY in src/incl/ult_date.tsl 
 */
#define PERIODICITY_UNKNOWN  0
#define PERIODICITY_Y        1
#define PERIODICITY_Q        4
#define PERIODICITY_M        12

void set_period_text(const char *period_text);
int prlex(void);
void prerror(const char *s);
void prrestart(FILE *s);
