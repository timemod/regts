# This file contains a number of utilities for regperiod_range objects
# that could be usefull. For now they are not exported to the package.

# Calculates the intersection of two regperiod_range objects.
# The start and end periods of the regperiod_range objects should not be NULL.
#
# @param range1 a \code{regperiod_range} object
# @param range2 another \code{regperiod_range} object
# @return the intersection of \code{range1} and \code{range2}. Returns
# NULL if there is no intersection
regrange_intersect <- function(range1, range2) {

    if (!inherits(range1, "regperiod_range")) {
        stop("The first range should be a regperiod_range object")
    }
    if (!inherits(range2, "regperiod_range")) {
        stop("The second range should be a regperiod_range object")
    }

    p_start <- start_period(range1)
    p_end   <- end_period(range1)
    p2_start <- start_period(range2)
    p2_end   <- end_period(range2)

    if (is.null(p_start) | is.null(p_end) |
        is.null(p2_start) | is.null(p2_end)) {
        stop("Start and end periods of both ranges should not be NULL")
    }

    freq1    <- frequency.regperiod(p_start)
    freq2    <- frequency.regperiod(p2_start)
    if (freq1 != freq2) {
        stop("The two periods have different frequency")
    }

    if (p2_start > p_start) {
        p_start <- p2_start
    }
    if (p2_end < p_end) {
        p_end <- p2_end
    }
    if (p_end >= p_start) {
        return (regperiod_range(p_start, p_end))
    } else {
        return (NULL)
    }
}

# Calculates the union of two regperiod_range objects.
# The start and end periods of the regperiod_range objects should not be NULL.
#
# @param range1 a \code{regperiod_range} object
# @param range2 another \code{regperiod_range} object
# @return the union of \code{range1} and \code{range2}. Returns
# NULL if there is no intersection
regrange_union <- function(range1, range2) {

    if (!inherits(range1, "regperiod_range")) {
        stop("Range 1 should be a regperiod_range object")
    }
    if (!inherits(range2, "regperiod_range")) {
        stop("Range 2 should be a regperiod_range object")
    }

    p_start <- start_period(range1)
    p_end   <- end_period(range1)

    p2_start <- start_period(range2)
    p2_end   <- end_period(range2)

    if (is.null(p_start) | is.null(p_end) |
        is.null(p2_start) | is.null(p2_end)) {
        stop("Start and end periods of both ranges should not be NULL")
    }
    freq1    <- frequency.regperiod(p_start)
    freq2    <- frequency.regperiod(p2_start)
    if (freq1 != freq2) {
        stop("The two periods have different frequency")
    }

    if (p2_start < p_start) {
        p_start <- p2_start
    }
    if (p2_end > p_end) {
        p_end <- p2_end
    }
    return (regperiod_range(p_start, p_end))
}
