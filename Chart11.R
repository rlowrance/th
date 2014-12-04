Chart11 <- function(my.control) {
    # return list $Fixed() $CellsUsed() $Chart()
    Fixed <- function() {
        # return list of fixed elements
        result <- list( scope = 'global'
                       ,model = 'linear'
                       ,timePeriod = '2003on'
                       ,scenario = 'avm'
                       ,response = 'logprice'
                       ,predictorsForm = 'level'
                       ,ndays = '30'
                       ,query = '1'
                       ,lambda = '0'
                       ,ntree = '0'
                       ,mtry = '0'
                       )
        result
    }
    fixed <- Fixed()
    Path <- CvCell()$Path
    MyPath <- function(predictorsName) {
        path <- Path( scope = fixed$scope
                     ,model = fixed$model
                     ,timePeriod = fixed$timePeriod
                     ,scenario = fixed$scenario
                     ,response = fixed$response
                     ,predictorsName = predictorsName
                     ,predictorsForm = fixed$predictorsForm
                     ,ndays = fixed$ndays
                     ,query = fixed$query
                     ,lambda = fixed$lambda
                     ,ntree = fixed$ntree
                     ,mtry = fixed$mtry
                     )
        path
    }
    Chart11PredictorsNames <- function() {
        # return vector of predictor names used in chart 10
        result <- c( 'best06'
                    ,'best15'
                    ,'pca03'
                    ,'pca04'
                    )
        result
    }
    CellsUsed <- function() {
        # return list of cell paths
        result <- list()
        for (predictorsName in Chart11PredictorsNames()) {
            path <- MyPath(predictorsName = predictorsName)
            result[[length(result) + 1]] <- path
        }
        result
    }
    Chart <- function(my.control) {
        # return 2 gg charts



        ACvResult <- function(predictorsName) {
            path.in <- MyPath(predictorsName = predictorsName)
            load(path.in)
            stopifnot(length(cv.result) == 1)
            a.cv.result <- cv.result[[1]]
            a.cv.result
        }
        Summarize <- function() {
            # return list $predictors.name $median.value $ci.lowest $ci.highest
            predictors.names <- Chart11PredictorsNames()
            n <- length(predictors.names)
            median.value <- double(n)
            ci.lowest <- double(n)
            ci.highest <- double(n)
            for (index in 1:n) {
                predictors.name <- predictors.names[[index]]
                a.cv.result <- ACvResult(predictors.name)
                rmse.values <- RootMedianSquaredErrors(a.cv.result)
                ci <- CIMedian(rmse.values)
                median.value[[index]] <- median(rmse.values)
                ci.lowest[[index]] <- ci$lowest
                ci.highest[[index]] <- ci$highest
            }
            result <- list( predictors.names = predictors.names
                           ,median.value  = median.value
                           ,ci.lowest     = ci.lowest
                           ,ci.highest    = ci.highest
                           )
            result
        }
        GraphChart <- function(summary, show.zero.value) {
            gg <- CIChart( axis.values = 'median rMedianSE across folds'
                          ,axis.names = 'predictor feature set names'
                          ,values = summary$median.value
                          ,values.low = summary$ci.lowest
                          ,values.high = summary$ci.highest
                          ,names = summary$predictors.names
                          ,show.zero.value = show.zero.value
                          )
            gg
        }
        summary <- Summarize()
        result <- list( gg1 = GraphChart(summary, show.zero.value = TRUE)
                       ,gg2 = GraphChart(summary, show.zero.value = FALSE)
                       )
        result
    }
    result <- list( CellsUsed = CellsUsed
                   ,Chart = Chart
                   ,Fixed = Fixed
                   )
}

