# This file contains a number of utilities for regperiod_range objects
# that are used internally in the regts package. They should
# not be exported to the package.
# All function in this file assume that the regperiod_range objects
# do not have start or end periods equal to NULL.


# Calculates the intersection of two regperiod_range objects.
# The start and end periods of the regperiod_range objects should not be NULL.
#
# @param range1 a \code{regperiod_range} object
# @param range2 another \code{regperiod_range} object
# @return the intersection of \code{range1} and \code{range2}. Returns
# NULL if there is no intersection
regrange_intersect <- function(range1, range2) {

    p_start <- start_period(range1)
    p_end   <- end_period(range1)

    p2_start <- start_period(range2)
    p2_end   <- end_period(range2)
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

    p_start <- start_period(range1)
    p_end   <- end_period(range1)

    p2_start <- start_period(range2)
    p2_end   <- end_period(range2)
    if (p2_start < p_start) {
        p_start <- p2_start
    }
    if (p2_end > p_end) {
        p_end <- p2_end
    }
    return (regperiod_range(p_start, p_end))
}
