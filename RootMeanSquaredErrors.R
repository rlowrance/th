RootMeanSquaredErrors <- function(a.cv.result) {
    # return vector of the root mean squared values from each fold
    nfolds <- length(a.cv.result)
    stopifnot(nfolds == 10)
    values <- 
        sapply(1 : nfolds,
               function(fold.index) {
                   evaluation <- a.cv.result[[fold.index]]
                   evaluation$rootMeanSquaredError
               }
               )
    values
}
