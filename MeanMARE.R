MeanMARE <- function(a.cv.result) {
    # return mean of the mean absolute relative errors in the folds
    are <- AbsoluteRelativeErrors(a.cv.result)
    mean.are <- sapply( are
                       ,function(are.one.fold) {
                           result <- mean(are.one.fold, na.rm = TRUE)
                           result
                       }
                       )
    result <- mean(mean.are, na.rm = TRUE)
    result
}
