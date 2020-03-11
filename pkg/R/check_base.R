# check argument base of function index_ts or rel2index
check_base <- function(base, x) {
  base <- as.period_range(base)
  freq_base <- frequency(base)
  freq_x <- frequency(x)
  if (freq_base != freq_x) {
    if (freq_base > freq_x) {
      stop(paste0("Base period (", base, ") should not have a higher",
                  " frequency than the input timeseries (", freq_x, ")"))
    }
    base <- change_frequency(base, new_frequency = freq_x)
  }
  return(base)
}
