MedianMARE <- function(a.cv.result) {
    # return median of the absolute relative errors in the folds
    are <- AbsoluteRelativeErrors(a.cv.result)
    median.are <- sapply(are, function(are.one.fold) median( are.one.fold
                                                            ,na.rm = TRUE
                                                            )
    )
    result <- median(median.are, na.rm = TRUE)
    result
}
