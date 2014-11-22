MeanRMSE <- function(a.cv.result) {
    # return mean of the rootMeanSquaredError values in the folds
    result <- mean(RootMeanSquaredErrors(a.cv.result))
    result
}
