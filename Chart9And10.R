source('CIChart.R')
source('CIMedian.R')
source('CvCell.R')
source('HeadersFixed.R')
source('RootMedianSquaredErrors.R')
source('Table9.R')

Chart9And10 <- function(my.control, feature.names, predictors.names) {
    # produce 3 charts for chart 9 and 10
    # ARGS
    # feature.names: chr vector of names of features
    # predictors.names: chr vector of names of predictors 
    #  (ex: best01, best02, ..., best24)
    #  (ex: pca01, pca02, ..., pca04)

    cv.cell <- CvCell()
    fixed <- cv.cell$FixedCellValues('Chart9')
    Path <- cv.cell$Path

    ACvResult <- function(num.features, query) {
        predictorsName <- predictors.names[[num.features]]
        path.in <- Path( scope = 'global'
                        ,model = 'linear'
                        ,timePeriod = '2003on'
                        ,scenario = 'avm'
                        ,response = 'logprice'
                        ,predictorsName = predictorsName
                        ,predictorsForm = 'level'
                        ,ndays = '30'
                        ,query = query
                        ,lambda = '0'
                        ,ntree = '0'
                        ,mtry = '0'
                        )
        if (file.exists(path.in)) {
            load(path.in)
            stopifnot(length(cv.result) == 1)
            a.cv.result <- cv.result[[1]]
            a.cv.result
        } else {
            cat('missing file: ', path.in, '\n')
            browser()
            a.cv.result <- NA
            a.cv.result
        }
    }
    Summarize <- function(query) {
        # return list $feature.name $median.value $ci.lowest $ci.highest
        n <- length(feature.names)
        median.value <- double(n)
        ci.lowest <- double(n)
        ci.highest <- double(n)
        for (feature.num in 1:n) {
            a.cv.result <- ACvResult(feature.num, query)
            if (length(a.cv.result) == 1) {
                # missing file
                browser()
                median.value[[feature.num]] <- NA
                ci.lowest[[feature.num]] <- NA
                ci.highest[[feature.num]] <- NA
            } else {
                rmse.values <- RootMedianSquaredErrors(a.cv.result)
                ci <- CIMedian(rmse.values)
                median.value[[feature.num]] <- median(rmse.values)
                ci.lowest[[feature.num]] <- ci$lowest
                ci.highest[[feature.num]] <- ci$highest
            }
        }
        result <- list( feature.names = feature.names
                       ,median.value  = median.value
                       ,ci.lowest     = ci.lowest
                       ,ci.highest    = ci.highest
                       )
        result
    }
    GraphChart <- function(summary, show.zero.value) {
        gg <- CIChart( axis.values = 'median rMedianSE across folds'
                      ,axis.names = 'cumulative feature names'
                      ,values = summary$median.value
                      ,values.low = summary$ci.lowest
                      ,values.high = summary$ci.highest
                      ,names = summary$feature.name
                      ,show.zero.value = show.zero.value
                      )
        gg
    }
    TextChart <- function(summary, query) {
        Header <- function(lines) {
            lines$Append('Estimated Generalization Errors from 10-fold Cross Validation')
            lines$Append('Using Root Median Squared Errors from Folds')
            lines$Append('Across Feature Sets')

            stopifnot(fixed$scope == 'global')
            stopifnot(fixed$model == 'linear')
            stopifnot(fixed$timePeriod == '2003on')
            stopifnot(fixed$predictorsForm == 'level')

            HeadersFixed(fixed, lines)
        }
        Body <- function(lines) {
            table <- Table9(lines)
            table$Header('num features', 'nth feature name', 'median RMSE', '95% confidence interval')
            lines$Append(' ')

            for (num.features in 1:length(summary$feature.name)) {
                table$Detail( num.features
                             ,summary$feature.name[[num.features]]
                             ,summary$median.value[[num.features]]
                             ,summary$ci.lowest[[num.features]]
                             ,summary$ci.highest[[num.features]]
                             )
            }
        }

        # execution starts here
        lines <- Lines()
        Header(lines)
        lines$Append(' ')
        Body(lines)
        result <- lines$Get()
        result
    }

    # Produce charts only for the 100% sample

    #summary.1 <- Summarize('100')  # do all the work for 1% query sample
    summary.100 <- Summarize('1')  # do all the work for 100% query sample

    result <- list( txt = TextChart(summary.100, '1')                        # 100% query sample
                   ,gg1 = GraphChart(summary.100, show.zero.value = TRUE)    # 100% query sample
                   ,gg2 = GraphChart(summary.100, show.zero.value = FALSE)   # 100% query sample
                   )
    result
}

