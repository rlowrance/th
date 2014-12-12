AbsoluteRelativeErrors <- function(a.cv.result) {
    # return vector of the absolute relative errors from each fold
    nfolds <- length(a.cv.result)
    stopifnot(nfolds == 10)
    values <- 
        sapply(1 : nfolds,
               function(fold.index) {
                   evaluation <- a.cv.result[[fold.index]]
                   absolute.error <- abs(evaluation$prediction - evaluation$actual)
                   absolute.relative.error <- absolute.error / evaluation$actual
                   absolute.relative.error
               }
               )
    values
}
