Chart7 <- function() {
    # return list $CellsUsed() $Chart() $Fixed()
    Fixed <- function() {
        # return list of fixed cells
        result <- list( scope = 'global'
                       ,model = 'linear'
                       ,scenario = 'avm'
                       ,timePeriod = '2003on'
                       ,predictorsName = 'alwaysNoAssessment'
                       ,query = '1'  # 100%
                       ,lambda = '0'
                       ,ntree = '0'
                       ,mtry = '0'
                       )
        result
    }

    fixed <- Fixed()
    Path <- CvCell()$Path
    MyPath <- function(response, predictorsForm, ndays) {
        my.path <- Path( scope = fixed$scope
                        ,model = fixed$model
                        ,timePeriod = fixed$timePeriod
                        ,scenario = fixed$scenario
                        ,response = response
                        ,predictorsName = fixed$predictorsName
                        ,predictorsForm = predictorsForm
                        ,ndays = ndays
                        ,query = fixed$query
                        ,lambda = fixed$lambda
                        ,ntree = fixed$ntree
                        ,mtry = fixed$mtry
                        )
        my.path
    }
    CellsUsed <- function() {
        # return list of paths to cells used
        fixed <- Fixed()
        Path <- CvCell()$Path
        result <- list()
        for (ndays in CvCell()$PossibleNdays()) {
            for (response in c('price', 'logprice')) {
                for (predictorsForm in c('level', 'log')) {
                    cell <- c( fixed
                              ,ndays = ndays
                              ,response = response
                              ,predictorsForm = predictorsForm
                              )
                    result[[length(result) + 1]] <- cell
                }
            }
        }
        result
    }
    Chart <- function(my.control) {
        # return txt lines for chart 7


        ACvResult <- function(ndays, response, predictorsForm) {
            # return the single cv.result in the e-cv-cell for ndays
            path.in <- MyPath( response = response
                              ,predictorsForm = predictorsForm
                              ,ndays = ndays
                              )
            if (file.exists(path.in)) {
                load(path.in)
                stopifnot(!is.null(cv.result))
                stopifnot(length(cv.result) == 1)
                a.cv.result <- cv.result[[1]]
                a.cv.result
            } else {
                cat('missing file: ', path.in, '\n')
                a.cv.result <- NA
                a.cv.result
            }
        }
        MetricExplanation <- function(metric.name) {
            switch( metric.name
                   ,meanRMSE = 'Mean of Root Mean Squared Errors Across Folds'
                   ,medRMSE  = 'Median of Root Median Squared Errors Across Folds'
                   ,fctWI10  = 'Mean of Fraction of Predictions Within 10 Percent of Actual Across Folds'
                   ,meanMARE  = 'Mean of Mean Absolute Relative Error Across Folds'
                   ,medMARE   = 'Median of Median Absolute Relative Error Across Folds'
                   ,stop(paste0('bad metric.name ', metric.name))
                   )
        }
        Header <- function(lines, metric.names) {
            # mutate lines by appending the header
            lines$Append('Comparison of Metrics From from 10 Fold Cross Validation')
            for (metric.name in metric.names) {
                lines$Append(sprintf('%s (%s)', MetricExplanation(metric.name), metric.name))
            }

            stopifnot(fixed$scope == 'global')
            stopifnot(fixed$model == 'linear')
            stopifnot(fixed$timePeriod == '2003on')
            stopifnot(fixed$predictorsName == 'alwaysNoAssessment')

            HeadersFixed(fixed, lines)
        }
        AppendTableMetrics3 <- function(lines, metric.names, metric.functions) {
            # append to Lines() object
            table <- Table7Metrics3(lines)
            AppendTableHeader <- function(table) {
                table$Header( 'predictorsForm:'
                             ,'level', 'level', 'level'
                             , 'log', 'log', 'log'
                             )
                table$Header( 'metric:'
                             ,metric.names[[1]], metric.names[[2]], metric.names[[3]]
                             ,metric.names[[1]], metric.names[[2]], metric.names[[3]]
                             )
                table$Header('ndays'
                             ,' ', ' ', ' '
                             ,' ', ' ', ' '
                             )
            }

            AppendDetailLine <- function(table, response, ndays) {
                Value <- function(ndays, predictorsForm, metric.index) {
                    a.cv.result <- ACvResult( ndays = ndays
                                             ,response = response
                                             ,predictorsForm = predictorsForm
                                             )
                    result <-
                        if (length(a.cv.result) == 1) {
                            NA
                        } else {
                            MetricFunction <- metric.functions[[metric.index]]
                            MetricFunction(a.cv.result)
                        }

                    result
                }
                table$Detail( ndays

                             ,Value(ndays, 'level', 1)
                             ,Value(ndays, 'level', 2)
                             ,Value(ndays, 'level', 3)

                             ,Value(ndays, 'log', 1)
                             ,Value(ndays, 'log', 2)
                             ,Value(ndays, 'log', 3)
                             )
            }
            for (response in c('price', 'logprice')) {
                table$Panel(response)
                AppendTableHeader(table)
                for (ndays in CvCell()$PossibleNdays()) {
                    AppendDetailLine(table, response, ndays)
                }
                lines$Append(' ')
            }
            lines
        }
        AppendTableMetrics5 <- function(lines, metric.names, metric.functions) {
            # mutate lines
            table <-Table7Metrics5(lines)
            AppendTableHeader <- function(table) {
                table$Header( 'metric:'
                             ,metric.names[[1]]
                             ,metric.names[[2]]
                             ,metric.names[[3]]
                             ,metric.names[[4]]
                             ,metric.names[[5]]
                             )
                table$Header( 'ndays'
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             )
            }
            AppendDetailLine <- function(table, ndays, response, predictorsForm) {
                Value <- function(ndays, MetricFunction) {
                    a.cv.result <- ACvResult( ndays = ndays
                                             ,response = response
                                             ,predictorsForm = predictorsForm
                                             )
                    result <-
                        if (length(a.cv.result) == 1) {
                            NA
                        } else {
                            MetricFunction(a.cv.result)
                        }
                    result
                }

                table$Detail( ndays
                             ,Value(ndays, metric.functions[[1]])
                             ,Value(ndays, metric.functions[[2]])
                             ,Value(ndays, metric.functions[[3]])
                             ,Value(ndays, metric.functions[[4]])
                             ,Value(ndays, metric.functions[[5]])
                             )
            }
            panel.index <- 0
            panel.names <- c('A', 'B', 'C', 'D')
            for (response in c('price', 'logprice')) {
                for (predictorsForm in c('level', 'log')) {
                    panel.index <- panel.index + 1
                    lines$Append(' ')
                    lines$Append(sprintf('Panel %s: predicting %s using predictors in %s form'
                                         ,panel.names[[panel.index]]
                                         ,response
                                         ,predictorsForm
                                         )
                    )
                    lines$Append(' ')
                    AppendTableHeader(table)
                    for (ndays in CvCell()$PossibleNdays()) {
                        AppendDetailLine( table
                                         ,ndays = ndays
                                         ,response = response
                                         ,predictorsForm = predictorsForm)
                    }



                }
            }


        }
        AppendTable <- function(lines, metric.names, metric.functions) {
            # append to Lines() object
            stopifnot(length(metric.names) == length(metric.functions))
            if (length(metric.names) == 3)
                AppendTableMetrics3(lines, metric.names, metric.functions)
            else 
                AppendTableMetrics5(lines, metric.names, metric.functions)
        }
        Report <- function(metric.names, metric.functions) {
            # produce Lines() object with specified table
            lines <- Lines()
            Header( lines = lines
                   ,metric.names = metric.names
                   )

            lines$Append(' ')
            AppendTable( lines = lines
                        ,metric.names = metric.names
                        ,metric.functions = metric.functions
                        )
            lines
        }
        ReportMetrics3 <- function() {
            if (FALSE) {
                cat('STUB ReportMetric3\n');
                lines <- Lines()
                lines$Append('STUB ReportMetrics3\n')
                return(lines)
            }
            Report( metric.names = c('meanRMSE', 'medRMSE', 'fctWI10')
                   ,metric.functions = c(MeanRMSE, MedianRMSE, MeanWithin10)
                   )
        }
        ReportMetrics5 <- function() {
            Report( metric.names = c('meanRMSE', 'medRMSE', 'fctWI10', 'meanMARE', 'medMARE')
                   ,metric.functions = c(MeanRMSE, MedianRMSE, MeanWithin10, MeanMARE, MedianMARE)
                   )
        }


        result <- list( txt3 = ReportMetrics3()$Get()  # 3 Metrics: meanRMSE medRMSE fctWI10
                       ,txt5 = ReportMetrics5()$Get()  # + 2 metrics: meanARE medARE
                       )
        result
    }
    result <- list( CellsUsed = CellsUsed
                   ,Chart = Chart
                   ,Fixed = Fixed
                   )
    result
    
}

