MeanWithin10 <- function(a.cv.result) {
    # return mean of the fraction.within.10.percent values in the folds
    nfolds <- length(a.cv.result)
    stopifnot(nfolds == 10)
    fraction.within.10.percent.values <- 
        sapply(1 : nfolds,
               function(fold.index) {
                   evaluation <- a.cv.result[[fold.index]]
                   evaluation$fraction.within.10.percent
               }
                               )
    result <- mean(fraction.within.10.percent.values)
    result
}
