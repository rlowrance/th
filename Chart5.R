Chart5 <- function() {
    # return list $Fixed() $CellsUsed() $Chart()
    Fixed <- function() {
        #return list of fixed elements in chart
        result <- list( scope = 'global'
                       ,model = 'linear'
                       ,timePeriod = '2008'
                       ,scenario = 'avm'
                       ,query = '1'
                       ,lambda = '0'
                       ,ntree = '0'
                       ,mtry = '0'
                       )
        result
    }

    fixed <- Fixed()
    Path <- CvCell()$Path
    MyPath <- function(response, predictorsForm, predictorsName, ndays) {
        my.path <- Path( scope = fixed$scope
                        ,model = fixed$model
                        ,timePeriod = fixed$timePeriod
                        ,scenario = fixed$scenario
                        ,response = response
                        ,predictorsName = predictorsName
                        ,predictorsForm = predictorsForm
                        ,nday = ndays
                        ,query = fixed$query
                        ,lambda = fixed$lambda
                        ,ntree = fixed$ntree
                        ,mtry = fixed$mtry
                        )
        my.path
    }
    CellsUsed <- function() {
        # return list of paths to cells used
        result <- list()
        for (ndays in CvCell()$PossibleNDays()){
            for (response in c('logprice', 'price')) {
                for (predictorsForm in c('level', 'log')) {
                    path <- MyPath( response = response
                                   ,predictorsName = predictorsName 
                                   ,predictorsForm = predictorsForm
                                   ,ndays = ndays
                                   )
                    result[[length(all) + 1]] <- path
                }
            }
        }
        result
    }
    Chart <- function(my.control) {
        # $horizontal: txt vector
        # $vertical: txt vector


        Header <- function(lines) {
            # append header to Lines() object
            lines$Append('Median of Root Median Squared Errors from 10 Fold Cross Validation')

            stopifnot(fixed$scope == 'global')
            stopifnot(fixed$model == 'linear')
            stopifnot(fixed$timePeriod == '2008')
            stopifnot(fixed$query == '1')

            HeadersFixed(fixed, lines)

            lines$Append(' ')
            lines$Append('Use Tax (yes ==> use tax assessment)')
        }

        AppendTableHorizontal <- function(lines) {
            table <- Table5And6Horizontal(lines)
            table$Blank()
            table$Header1('preds', 'Use', 'ndays')
            table$Header2( 'response', 'Form', 'Tax'
                          ,'30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360'
                          )

            DetailLine <- function(response, predictorsName, predictorsForm) {
                M <- function(ndays) {
                    path.in <- MyPath( response = response
                                      ,predictorsName = predictorsName
                                      ,predictorsForm = predictorsForm
                                      ,ndays = ndays
                                      )
                    load(path.in)
                    stopifnot(!is.null(cv.result))
                    stopifnot(length(cv.result) == 1)
                    median.RMSE <- MedianRMSE(cv.result[[1]])
                    median.RMSE
                }

                use.assessment <- switch( predictorsName
                                         ,always = 'yes'
                                         ,alwaysNoAssessment = 'no'
                                         )
                table$Detail( response, predictorsForm, use.assessment
                             ,M(30), M(60), M(90), M(120), M(150), M(180)
                             ,M(210), M(240), M(270), M(300), M(330), M(360)
                             )
            }

            for (response in c('price', 'logprice')) {
                for (predictorsForm in c('level', 'log')) {
                    for (predictorsName in c('always', 'alwaysNoAssessment')) {
                        DetailLine( response = response
                                   ,predictorsName = predictorsName
                                   ,predictorsForm = predictorsForm
                                   )
                    }
                }
            }
            lines
        }
        AppendTableVertical <- function(lines) {
            table <- Table5And6Vertical(lines)
            lines$Append(' ')
            table$Header('response:', 'price', 'price', 'price', 'price', 'logprice', 'logprice', 'logprice', 'logprice')
            table$Header('predForm:', 'level', 'level', 'log', 'log', 'level', 'level', 'log', 'log')
            table$Header('use tax:', 'yes', 'no', 'yes', 'no', 'yes', 'no', 'yes', 'no')
            table$Header('ndays', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ')

            DetailLine <- function(ndays) {
                M <- function(response, pred.form, use.tax) {
                    # return median value
                    predictorsName <- if (use.tax == 'yes') 'always' else 'alwaysNoAssessment'
                    path.in <- Path( scope = fixed$scope
                                    ,model = fixed$model
                                    ,timePeriod = fixed$timePeriod
                                    ,scenario = fixed$scenario
                                    ,response = response
                                    ,predictorsName = predictorsName 
                                    ,predictorsForm = pred.form
                                    ,ndays = ndays
                                    ,query = fixed$query
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
                table$Detail(ndays
                             ,M(response = 'price', pred.form = 'level', use.tax = 'yes')
                             ,M(response = 'price', pred.form = 'level', use.tax = 'no')
                             ,M(response = 'price', pred.form = 'log', use.tax = 'yes')
                             ,M(response = 'price', pred.form = 'log', use.tax = 'no')
                             ,M(response = 'logprice', pred.form = 'level', use.tax = 'yes')
                             ,M(response = 'logprice', pred.form = 'level', use.tax = 'no')
                             ,M(response = 'logprice', pred.form = 'log', use.tax = 'yes')
                             ,M(response = 'logprice', pred.form = 'log', use.tax = 'no')
                             )
            }
            for (ndays in c('30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360')){
                DetailLine(ndays)
            }




        }

        Report <- function(AppendTable) {
            lines <- Lines()
            Header(lines)

            lines$Append(' ')
            AppendTable(lines)
            lines
        }
        ReportHorizontal <- function() {
            result <- Report(AppendTableHorizontal)
            result
        }
        ReportVertical <- function() {
            result <- Report(AppendTableVertical)
            result
        }

        result <- list( horizontal = ReportHorizontal()$Get()
                       ,vertical   = ReportVertical()$Get()
                       )
        result
    }
    Fixed <- function() {
        result <-  list( scope = 'global'
                        ,model = 'linear'
                        ,timePeriod = '2008'
                        ,scenario = 'avm'
                        ,query = '1'
                        ,lambda = '0'
                        ,ntree = '0'
                        ,mtry = '0'
                        )
        result
    }
    result <- list( CellsUsed = CellsUsed
                   ,Chart = Chart
                   ,Fixed = Fixed
                   )

}
