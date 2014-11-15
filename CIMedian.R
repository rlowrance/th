CIMedian <- function(values, removeNAs = FALSE, debug = FALSE) {
    # return 95% confidence interval for the median of the vector of values
    # ref: R Cookbook, p 184
    if (debug) browser()
    if (removeNAs)
        values <- values[!is.na(values)]
    if (length(values) == 1) {
        # sample(values) ==> a very long vector, because sample.int is called
        # hence handle specially
        result <- list( lowest = values
                       ,highest = values
                       )
        return(result)
    }
    # length(values) > 1
    num.resamples <- 100  # TODO: set to 10,000
    num.resamples <- 10000
    sample.medians <- numeric(length = num.resamples)  # preallocate for speed
    lapply( 1:num.resamples
           ,function(index) sample.medians[[index]] <<- median(sample(values, replace = TRUE))
           )
    confidence.interval <- quantile( x = sample.medians
                                    ,probs = c(0.025, 0.975)
                                    )
    result <- list( lowest = confidence.interval[[1]]
                   ,highest = confidence.interval[[2]]
                   )
    result
}
