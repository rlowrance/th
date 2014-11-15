Chart10 <- function(my.control) {
    feature.names <- Predictors2( predictors.name = 'pca04'  # get all the features in order
                                 ,predictors.form = 'level'
                                 )
    predictors.names <- sprintf('pca%02d', 1:4)
    result <- Chart9And10( my.control = my.control
                          ,feature.names = feature.names
                          ,predictors.names = predictors.names
                          )
    result
}

