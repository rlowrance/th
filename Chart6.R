Chart6 <- function(my.control) {
    # return list
    # $horizontal: txt vector
    # $vertical: txt vector

    cv.cell <- CvCell()
    fixed <- cv.cell$FixedCellValues('Chart6')
    Path <- cv.cell$Path

    Header <- function(lines, query) {
        # append head info to Lines() object
        lines$Append('Median of Root Median Squared Errors from 10 Fold Cross Validation')

        stopifnot(fixed$scope == 'global')
        stopifnot(fixed$model == 'linear')
        stopifnot(fixed$timePeriod == '2003on')
        stopifnot(fixed$query == '100')

        HeadersFixed(fixed, lines)
        lines$Append(paste0( 'Percent of queries in each fold that were estimated: '
                            ,switch( query
                                    ,'1' = '100'
                                    ,'100' = '1'
                                    ,stop(paste0('bad query ', as.character(query)))
                                    )
                            )
        )

        lines$Append(' ')
        lines$Append('Column Use Cen (yes ==> use census data)')
    }

    AppendTableHorizontal <- function(lines, query) {
        table <- Table5And6Horizontal(lines)
        table$Blank()
        table$Header1('preds', 'Use', 'ndays')
        table$Header2( 'response', 'Form', 'Cen'
                      ,'30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360'
                      )

        DetailLine <- function(response, predictorsName, predictorsForm) {
            M <- function(ndays) {
                path.in <- Path( scope = fixed$scope
                                ,model = fixed$model
                                ,timePeriod = fixed$timePeriod
                                ,scenario = fixed$scenario
                                ,response = response
                                ,predictorsName = predictorsName
                                ,predictorsForm = predictorsForm
                                ,nday = ndays
                                ,query = query
                                ,lambda = fixed$lambda
                                ,ntree = fixed$ntree
                                ,mtry = fixed$mtry
                                )
                load(path.in)
                stopifnot(!is.null(cv.result))
                stopifnot(length(cv.result) == 1)
                median.RMSE <- MedianRMSE(cv.result[[1]])
                median.RMSE
            }

            use.assessment <- switch( predictorsName
                                     ,alwaysNoAssessment = 'yes'
                                     ,alwaysNoCensus = 'no'
                                     ,stop('bad predictorsName')
                                     )
            table$Detail( response, predictorsForm, use.assessment
                         ,M(30), M(60), M(90), M(120), M(150), M(180)
                         ,M(210), M(240), M(270), M(300), M(330), M(360)
                         )
        }

        for (response in c('price', 'logprice')) {
            for (predictorsForm in c('level', 'log')) {
                for (predictorsName in c('alwaysNoAssessment', 'alwaysNoCensus')) {
                    DetailLine( response = response
                               ,predictorsName = predictorsName
                               ,predictorsForm = predictorsForm
                               )
                }
            }
        }
        lines
    }


    AppendTableVertical <- function(lines, query) {
        table <- Table5And6Vertical(lines)
        lines$Append(' ')
        table$Header('response:', 'price', 'price', 'price', 'price', 'logprice', 'logprice', 'logprice', 'logprice')
        table$Header('predForm:', 'level', 'level', 'log', 'log', 'level', 'level', 'log', 'log')
        table$Header('use cen:', 'yes', 'no', 'yes', 'no', 'yes', 'no', 'yes', 'no')
        table$Header('ndays', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ')

        DetailLine <- function(ndays) {
            M <- function(response, pred.form, use.census) {
                # return median value
                predictorsName <- if (use.census == 'yes') 'alwaysNoAssessment' else 'alwaysNoCensus'
                path.in <- Path( scope = fixed$scope
                                ,model = fixed$model
                                ,timePeriod = fixed$timePeriod
                                ,scenario = fixed$scenario
                                ,response = response
                                ,predictorsName = predictorsName 
                                ,predictorsForm = pred.form
                                ,ndays = ndays
                                ,query = query
                                ,lambda = fixed$lambda
                                ,ntree = fixed$ntree
                                ,mtry = fixed$mtry
                                )
                if (file.exists(path.in)) {
                    load(path.in)
                    stopifnot(!is.null(cv.result))
                    stopifnot(length(cv.result) == 1)
                    median.RMSE <- MedianRMSE(cv.result[[1]])
                    median.RMSE
                } else {
                    cat('missing file: ', path.in, '\n')
                    median.RMSE <- NA
                    median.RMSE
                }
            }
            table$Detail(ndays
                         ,M(response = 'price', pred.form = 'level', use.census = 'yes')
                         ,M(response = 'price', pred.form = 'level', use.census = 'no')
                         ,M(response = 'price', pred.form = 'log', use.census = 'yes')
                         ,M(response = 'price', pred.form = 'log', use.census = 'no')
                         ,M(response = 'logprice', pred.form = 'level', use.census = 'yes')
                         ,M(response = 'logprice', pred.form = 'level', use.census = 'no')
                         ,M(response = 'logprice', pred.form = 'log', use.census = 'yes')
                         ,M(response = 'logprice', pred.form = 'log', use.census = 'no')
                         )
        }
        for (ndays in c('30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360')){
            DetailLine(ndays)
        }
    }

    Report <- function(AppendTable, query) {
        lines <- Lines()
        Header(lines, query)

        lines$Append(' ')
        AppendTable(lines, query)
        lines
    }
    ReportHorizontal <- function() {
        result <- Report( AppendTable = AppendTableHorizontal
                         ,query = '100'
                         )
        result
    }
    ReportVertical <- function(query) {
        result <- Report(AppendTableVertical, query)
        result
    }

    result <- list( horizontal   = ReportHorizontal()$Get()
                   ,vertical.1   = ReportVertical('100')$Get()  # 1% sample of queries
                   ,vertical.100 = ReportVertical('1')$Get()    # 100% sample of queries
                   )
    result
}
