RootMedianSquaredErrors <- function(a.cv.result) {
    # return vector of the root median squared values from each fold
    nfolds <- length(a.cv.result)
    stopifnot(nfolds == 10)
    rootMedianSquaredError.values <- 
        sapply(1 : nfolds,
               function(fold.index) {
                   evaluation <- a.cv.result[[fold.index]]
                   evaluation$rootMedianSquaredError
               }
               )
    rootMedianSquaredError.values
}
