#' Calculate the intersection or union of two \code{\link{period_range}} objects.
#'
#' These functions calculate the intersection or union of two \code{\link{period_range}}
#' objects. The start and end periods of the period_range objects may
#' not be \code{NULL}.
#' @param range1 a \code{period_range} object or object that can be coerced to
#' a period_range
#' @param range2 another such (\code{period_range}) object
#' @return the intersection or union of \code{range1} and \code{range2}.
#' If there are no common periods \code{range_intersection} returns \code{NULL}
#' @examples
#' range1 <- period_range("2016Q1", "2017Q4")
#' range2 <- period_range("2017Q1", "2018Q2")
#' range_intersect <- range_intersect(range1, range2)
#' range_union <- range_union(range1, range2)
#'
#' @name range_intersect/range_union

#' @rdname range_intersect-slash-range_union
#' @export
range_intersect <- function(range1, range2) {

  range1 <- as.period_range(range1)
  range2 <- as.period_range(range2)
  #   function checks input and returns list with information about the ranges:
  #   begin, end and freq
  res <- range_check(range1, range2)

  p_start <- max(res$p1_start, res$p2_start)
  p_end   <- min(res$p1_end, res$p2_end)

  if (p_end >= p_start) {
      return (create_period_range(p_start, p_end, res$freq))
  } else {
      return (NULL)
  }
}

#' @rdname range_intersect-slash-range_union
#' @export
range_union <- function(range1, range2) {

  range1 <- as.period_range(range1)
  range2 <- as.period_range(range2)

  #   function checks input and returns list with information about the ranges:
  #   begin, end and freq
  res <- range_check(range1, range2)

  p_start <- min(res$p1_start, res$p2_start)
  p_end   <- max(res$p1_end, res$p2_end)

  return (create_period_range(p_start, p_end, res$freq))
}

# internal function that checks the ranges and returns information about them
range_check <- function(range1, range2) {

    p1_start <- start_period(range1)
    p1_end   <- end_period(range1)
    p2_start <- start_period(range2)
    p2_end   <- end_period(range2)

    if (is.null(p1_start) | is.null(p1_end) |
        is.null(p2_start) | is.null(p2_end)) {
        stop("Start and end periods of both ranges should not be NULL")
    }

    freq1    <- frequency.period(p1_start)
    freq2    <- frequency.period(p2_start)
    if (freq1 != freq2) {
        stop("The two periods have different frequency")
    }
    return(list(p1_start = p1_start, p1_end = p1_end, p2_start = p2_start,
                p2_end = p2_end, freq = freq1))
}
