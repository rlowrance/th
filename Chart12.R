Chart12 <- function(my.control) {
    # return 2 gg charts

    cv.cell <- CvCell()
    fixed <- cv.cell$FixedCellValues('Chart12')
    Path <- cv.cell$Path

    C.To.Lambda <- cv.cell$C.To.Lambda

    ACvResult <- function(lambda) {
        path.in <- Path( scope = fixed$scope
                        ,model = fixed$model
                        ,timePeriod = fixed$timePeriod
                        ,scenario = fixed$scenario
                        ,response = fixed$response
                        ,predictorsName = fixed$predictorsName
                        ,predictorsForm = fixed$predictorsForm
                        ,ndays = fixed$ndays
                        ,query = fixed$query
                        ,lambda = lambda
                        ,ntree = fixed$ntree
                        ,mtry = fixed$mtry
                        )
        load(path.in)
        stopifnot(length(cv.result) == 1)
        a.cv.result <- cv.result[[1]]
        a.cv.result
    }
    Summarize <- function() {
        # return list $predictors.name $median.value $ci.lowest $ci.highest
        possible.lambda.values <- Chart12LambdaValues()
        n <- length(possible.lambda.values)
        median.value <- double(n)
        ci.lowest <- double(n)
        ci.highest <- double(n)
        c.values <- double(n)
        lambda.value.name <- character(n)
        for (index in 1:n) {
            lambda <- possible.lambda.values[[index]]
            a.cv.result <- ACvResult(lambda)
            rmse.values <- RootMedianSquaredErrors(a.cv.result)
            ci <- CIMedian(rmse.values)
            median.value[[index]] <- median(rmse.values)
            ci.lowest[[index]] <- ci$lowest
            ci.highest[[index]] <- ci$highest
            lambda.value.name[[index]] <- lambda
        }
        result <- list( lambda.value.name = as.character(as.numeric(lambda.value.name) / 100)
                       ,median.value  = median.value
                       ,ci.lowest     = ci.lowest
                       ,ci.highest    = ci.highest
                       )
        result
    }
    GraphChart <- function(summary, show.zero.value) {
        gg <- CIChart( axis.values = 'Estimated Generalization Errors'
                      ,axis.names = 'L2 regularizer value'
                      ,values = summary$median.value
                      ,values.low = summary$ci.lowest
                      ,values.high = summary$ci.highest
                      ,names = summary$lambda.value.name
                      ,show.zero.value = show.zero.value
                      )
        gg
    }
    TableChart <- function(summary) {
        txt <- CITable( axis.values = 'Estimated Generalization Errors from 10-fold Cross Validation'
                       ,axis.names = 'L2 regularizer value'
                       ,values = summary$median.value
                       ,values.low = summary$ci.lowest
                       ,values.high = summary$ci.highest
                       ,names = summary$lambda.value.name
                       )
        txt
    }
    summary <- Summarize()
    result <- list( gg1 = GraphChart(summary, show.zero.value = TRUE)
                   ,gg2 = GraphChart(summary, show.zero.value = FALSE)
                   ,txt = TableChart(summary)
                   )
    result
}

