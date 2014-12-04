Chart7 <- function() {
    # return list $CellsUsed() $Chart() $Fixed()
    Fixed <- function() {
        # return list of fixed cells
        result <- list( scope = 'global'
                       ,model = 'linear'
                       ,scenario = 'avm'
                       ,timePeriod = '2003on'
                       ,predictorsName = 'alwaysNoAssessment'
                       ,lambda = '0'
                       ,ntree = '0'
                       ,mtry = '0'
                       )
        result
    }

    fixed <- Fixed()
    Path <- CvCell()$Path
    MyPath <- function(response, predictorsForm, ndays, query) {
        my.path <- Path( scope = fixed$scope
                        ,model = fixed$model
                        ,timePeriod = fixed$timePeriod
                        ,scenario = fixed$scenario
                        ,response = response
                        ,predictorsName = fixed$predictorsName
                        ,predictorsForm = predictorsForm
                        ,ndays = ndays
                        ,query = query
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
            for (response in c('level', 'log')) {
                for (predictorsForm in c('level', 'log')) {
                    for (query in c('1', '100')) {
                    path <- MyPath( ndays = ndays
                                   ,respone = response
                                   ,predictorsForm = predictorsForm
                                   ,query = query
                                   )
                    result[[length(result) + 1]] <- path
                    }
                }
            }
        }
        result
    }
    Chart <- function(my.control) {
        # return txt lines for chart 7


        cv.cell <- CvCell()
        fixed <- cv.cell$FixedCellValues('Chart7')
        Path <- cv.cell$Path

        ACvResult <- function(ndays, response, predictorsForm, query) {
            # return the single cv.result in the e-cv-cell for ndays
            path.in <- MyPath( response = response
                              ,predictorsForm = predictorsForm
                              ,ndays = ndays
                              ,query = query
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
        Header <- function(lines, include.asterisk, query) {
            # mutate lines by appending the header
            lines$Append('Comparison of Metrics From from 10 Fold Cross Validation')
            lines$Append('Mean of Root Mean Squared Errors (meanRMSE) vs.')
            lines$Append('Median of Root Median Squared Errors (medRMSE) vs.')
            lines$Append('Mean of Fraction of Predictions Within 10 Percent of Actual Values (fctWI10)')
            if (include.asterisk)
                lines$Append('An asterisk (*) before a number indicates that it minimizes the row')

            stopifnot(fixed$scope == 'global')
            stopifnot(fixed$model == 'linear')
            stopifnot(fixed$timePeriod == '2003on')
            stopifnot(fixed$predictorsName == 'alwaysNoAssessment')
            stopifnot(fixed$query == '100')

            HeadersFixed(fixed, lines)
            lines$Append(sprintf('Percent of queries in each fold that were estimated: %d'
                                 ,switch( query
                                         ,'100' = 1
                                         ,'20'  = 5
                                         ,'1'   = 100
                                         ,stop(paste0('bad query:', as.character(query)))
                                         )
                                 )
            )
        }
        AppendTableVertical <- function(lines, query) {
            # append to Lines() object
            table <- Table7Vertical(lines)
            AppendTableHeader <- function(table) {
                table$Header( 'predictorsForm:'
                             ,'level', 'level', 'level'
                             , 'log', 'log', 'log'
                             )
                table$Header( 'metric:'
                             ,'meanRMSE', 'medRMSE', 'fctWI10'
                             ,'meanRMSE', 'medRMSE', 'fctWI10'
                             )
                table$Header('ndays'
                             ,' ', ' ', ' '
                             ,' ', ' ', ' '
                             )
            }

            DetailLine <- function(response, ndays) {
                Value <- function(ndays, predictorsForm, metricName) {
                    a.cv.result <- ACvResult( ndays = ndays
                                             ,response = response
                                             ,predictorsForm = predictorsForm
                                             ,query = query
                                             )
                    result <-
                        if (length(a.cv.result) == 1) {
                            NA
                        } else {
                            switch( metricName
                                   ,medRMSE = MedianRMSE(a.cv.result)
                                   ,fctWI10 = MeanWithin10(a.cv.result)
                                   ,meanRMSE = MeanRMSE(a.cv.result)
                                   ,stop('bad metricName')
                                   )
                        }

                    result
                }
                table$Detail( ndays

                             ,Value(ndays, 'level', 'meanRMSE')
                             ,Value(ndays, 'level', 'medRMSE')
                             ,Value(ndays, 'level', 'fctWI10')

                             ,Value(ndays, 'log', 'meanRMSE')
                             ,Value(ndays, 'log', 'medRMSE')
                             ,Value(ndays, 'log', 'fctWI10')
                             )
            }
            for (response in c('price', 'logprice')) {
                table$Panel(response)
                AppendTableHeader(table)
                for (ndays in c('30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360')) {
                    DetailLine(response, ndays)
                }
                lines$Append(' ')
            }
            lines
        }

        Report <- function(AppendTable, include.asterisk, query) {
            # produce Lines() object with specified table
            lines <- Lines()
            Header( lines = lines
                   ,query = query
                   ,include.asterisk = include.asterisk
                   )

            lines$Append(' ')
            AppendTable(lines, query)
            lines
        }
        ReportVertical <- function(query) {
            result <- Report( AppendTable = AppendTableVertical
                             ,query = query
                             ,include.asterisk = FALSE
                             )
            result
        }


        result <- list(txt = ReportVertical('1')$Get()    # 100% sample
                       )
        result
    }
    result <- list( CellsUsed = CellsUsed
                   ,Chart = Chart
                   ,Fixed = Fixed
                   )
    result
    
}

