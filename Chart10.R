Chart10 <- function(my.control) {
    # return list $CellsUsed() $Chart() $Fixed()

    chart.9.and.10 <- Chart9And10()
    MyPath <- chart.9.and.10$MyPath
    PredictorsNames <- function() {
        # return vector of predictor names
        predictors.names <- sprintf('pca%02d', 1:4)
        predictors.names
    }
    CellsUsed <- function() {
        # return list of paths to cells used
        result <- list()
        for (predictorsName in PredictorsNames()) {
            path <- MyPath(predictorsName = predictorsName)
            result[[length(result) + 1]] <- path
        }
        result
    }
    Chart <- function(my.control) {
        feature.names <- Predictors2( predictors.name = 'pca04'  # get all the features in order
                                     ,predictors.form = 'level'
                                     )
        predictors.names <- PredictorsNames()
        result <- chart.9.and.10$Chart( my.control = my.control
                                       ,feature.names = feature.names
                                       ,predictors.names = predictors.names
                                       )
        result
    }
    result <- list( CellsUsed = chart.9.and.10$CellsUsed
                   ,Chart = Chart
                   ,Fixed = chart.9.and.10$Fixed
                   )
    result
}

