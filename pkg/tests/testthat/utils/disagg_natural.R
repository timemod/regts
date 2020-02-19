# disaggregation of a timeseries using natural spline interpolation,
# employing function stats::spline. We don't use function spline in package
# regts because stats::spline does not implement the not-a-knot boundary condition.
# we use this function to test regts::spline
disagg_natural <- function(x, nfrequency,
                           constraint = c("sum", "average", "last", "first")) {

  constraint <- match.arg(constraint)

  old_frequency <- frequency(x)

  frac <- nfrequency / old_frequency

  do_cumul <- constraint %in% c("sum", "average")

  input_is_matrix <- is.matrix(x)
  if (input_is_matrix) {
    col_names <- colnames(x)
  }

  x <- na_trim(x)

  # internal function to disaggregate a single timeseries
  disagg_uni <- function(x) {

    x_trim <- na_trim(x)

    if (is.null(x_trim)) {
      x_trim <- x
    }

    if (do_cumul) {
      x_trim[start_period(x_trim) - 1] <- 0
      x_trim[] <- cumsum(x_trim)
    }

    p_start_inp <- start_period(x_trim)
    year <- get_year(p_start_inp)
    subp_inp <- get_subperiod(p_start_inp)
    if (constraint == "first") {
      subp_out <- (subp_inp - 1) * frac + 1
    } else {
      subp_out <- subp_inp * frac
    }
    p_start_out <- period(year, frequency = nfrequency) + (subp_out - 1)
    n <- length(x_trim)
    nres <- (n - 1) * frac + 1
    if (any(is.na(x_trim))) {
      result <- rep(NA_real_, nres)
    } else {
      result  <- spline(x = seq_len(n), y = x_trim, n = nres,
                        method = "natural")$y
    }
    result <- regts(result, start = p_start_out)
    if (do_cumul) {
      result <- diff(result)
      if (constraint == "average") {
        result <- result * frac
      }
    }
    return(result)
  }

  l_input <- as.list(x)
  l_splined <- lapply(l_input, FUN = disagg_uni)
  result <- as.regts(l_splined)

  if (!input_is_matrix) {
    result <- result[, 1]
  } else if (input_is_matrix && !is.matrix(result)) {
    dim(result) <- c(length(result), 1)
    colnames(result) <- col_names
  }

  return(result)
}




