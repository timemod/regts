#' @export
Ops.regts <- function(e1, e2) {

    # call Ops.ts
    retval <- as.regts(NextMethod(.Generic))

    # Handling of labels: sometimes the result of NextMethod(.Generic)
    # does not have ts_labels while e1 or e2 do have labels. For example, if ts1
    # is a numerical timeseries with labels, then the result of ts1 < 2 does
    # not have labels. Therefore we have to add the labels in that case. For
    # performance reasons, we first check if we have to take care of the
    # labels.
    lbl1 <- ts_labels(e1)
    lbl2 <- ts_labels(e2)
    check_lbls <- (!is.null(lbl1)|| !is.null(lbl2)) &&
                  is.null(ts_labels(retval))

    if (is.ts(e1) && is.ts(e2)) {
        # if both e1 and e2 are timeseries, then the result of
        # NextMethod(.Generic) has column names with prefix "e1." due to
        # non-standard evaluation. For regts we therefore give the columns
        # the same name as e1.
        colnames(retval) <- colnames(e1)
        if (check_lbls) {
            ts_labels(retval) <- lbl1
        }
    } else if (check_lbls) {
        if (is.regts(e1)) {
            ts_labels(retval) <- lbl1
        } else {
            ts_labels(retval) <- lbl2
        }
    }
    return (retval)
}
