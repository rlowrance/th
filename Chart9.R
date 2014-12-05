Chart9 <- function(my.control) {
    # return list $CellsUsed() $Chart() $Fixed()

    chart.9.and.10 <- Chart9And10()
    MyPath <- chart.9.and.10$MyPath
    PredictorsNames <- function() {
        # return vector of predictor names
        predictors.names <- sprintf('best%02d', 1:24)
        predictors.names
    }
    CellsUsed <- function() {
        # return list of paths to cells used
        fixed <- chart.9.and.10$Fixed()
        result <- list()
        for (predictorsName in PredictorsNames()) {
            cell <- c( fixed
                      ,predictorsName = predictorsName
                      )
            result[[length(result) + 1]] <- cell
        }
        result
    }
        
    Chart <- function(my.control) {
        # return 3 results for chart 9

        feature.names <- readLines(con = paste0(my.control$working, 'e-features-lcv2.txt'))
        predictors.names <- PredictorsNames()
        result <- chart.9.and.10$Chart( my.control = my.control
                                             ,feature.names = feature.names
                                             ,predictors.names = predictors.names
                                             )
        result
    }
    result <- list( CellsUsed = CellsUsed
                   ,Chart = Chart
                   ,Fixed = chart.9.and.10$Fixed
                   )
    result
}

