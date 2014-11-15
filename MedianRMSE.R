MedianRMSE <- function(a.cv.result) {
    # return median of the rootMedianSquaredError values in the folds
    result <- median(RootMedianSquaredErrors(a.cv.result))
    result
}
