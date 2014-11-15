Chart9 <- function(my.control) {
    # return 3 results for chart 9
    feature.names <- readLines(con = paste0(my.control$working, 'e-features-lcv2.txt'))
    predictors.names <- sprintf('best%02d', 1:24)
    result <- Chart9And10( my.control = my.control
                          ,feature.names = feature.names
                          ,predictors.names = predictors.names
                          )
    result
}

