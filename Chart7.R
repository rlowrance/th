#source('CvCell.R')
#source('HeadersFixed.R')
#source('MeanWithin10.R')
#source('MeanRMSE.R')
#source('MedianRMSE.R')
#source('RootMeanSquaredErrors.R')
#source('RootMedianSquaredErrors.R')
#source('Table7AHorizontal.R')

Chart7 <- function(my.control) {
    # return txt lines for chart 7


    cv.cell <- CvCell()
    fixed <- cv.cell$FixedCellValues('Chart7')
    Path <- cv.cell$Path

    ACvResult <- function(ndays, response, predictorsForm) {
        # return the single cv.result in the e-cv-cell for ndays
        path.in <- Path( scope = fixed$scope
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
        load(path.in)
        stopifnot(!is.null(cv.result))
        stopifnot(length(cv.result) == 1)
        a.cv.result <- cv.result[[1]]
        a.cv.result
    }
    Header <- function(lines, include.asterisk) {
        # mutate lines by appending the header
        lines$Append('Comparison of Metrics From from 10 Fold Cross Validation')
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
    }
    AppendTableHorizontal <- function(lines) {
        parts.a.and.b <- FALSE

        PartA <- function(lines) {
            table <- Table7AHorizontal(lines)
            if (parts.a.and.b)
                table$Append('Part A: All Results')
            table$Append(' ')
            table$Header1('preds', 'ndays')
            table$Header2('response', 'form', 'Metric',
                          '30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360'
                          )

            DetailLine <- function(response, predictorsForm, metricName) {
                Value <- function(ndays) {
                    cv.result <- ACvResult( ndays = ndays
                                           ,response = response
                                           ,predictorsForm = predictorsForm
                                           )
                    result <-
                        switch( metricName
                               ,medRMSE = MedianRMSE(cv.result)
                               ,fctWI10 = MeanWithin10(cv.result)
                               ,meanRMSE = MeanRMSE(cv.result)
                               ,stop('bad metricName')
                               )
                    result
                }

                Detail <- switch( metricName
                                 ,medRMSE = table$DetailWholeNumbers
                                 ,fctWI10 = table$DetailFractions
                                 ,meanRMSE = table$DetailWholeNumbers
                                 )

                Detail( response, predictorsForm, metricName
                       ,Value(30),   Value(60),  Value(90), Value(120), Value(150), Value(180)
                       ,Value(210), Value(240), Value(270), Value(300), Value(330), Value(360)
                       )
            }

            for (response in c('price', 'logprice')) {
                for (predictorsForm in c('level', 'log')) {
                    table$Append(' ')
                    for (metricName in c('meanRMSE', 'medRMSE', 'fctWI10')) {
                        DetailLine( response = response
                                   ,predictorsForm = predictorsForm
                                   ,metricName = metricName
                                   )
                    }
                }
            }
        }
        PartB <- function(lines) {
            table <- Table.7.B(lines)
            table$Append('Part B: Best result')
            table$Append(' ')
            table$Header(       ' ', 'pred',      ' ', 'Best',   'Best')
            table$Header('response', 'form', 'metric', 'ndays', 'value')

            DetailLine <- function(response, predictorsForm, metricName, table) {
                Value <- function(ndays) {
                    cv.result <- ACvResult( ndays = ndays
                                           ,response = response
                                           ,predictorsForm = predictorsForm
                                           )
                    result <-
                        switch( metricName
                               ,medRMSE = MedianRMSE(cv.result)
                               ,fctWI10 = MeanWithin10(cv.result)
                               ,stop('bad metricName')
                               )
                    result
                }
                DetailLineMedRMSE <- function() {
                    values <- c( Value('30'), Value('60'), Value('90')
                                ,Value('120'), Value('150'), Value('180')
                                ,Value('210'), Value('240'), Value('270')
                                ,Value('300'), Value('330'), Value('360'))
                    best.value <- min(values)
                    best.index <- which.min(values)
                    table$DetailWholeNumber( response, predictorsForm, metricName
                                            ,30 * best.index, best.value
                                            )
                }
                DetailLineFctWI10 <- function() {
                    values <- c( Value('30'), Value('60'), Value('90')
                                ,Value('120'), Value('150'), Value('180')
                                ,Value('210'), Value('240'), Value('270')
                                ,Value('300'), Value('330'), Value('360'))
                    best.value <- max(values)
                    best.index <- which.max(values)
                    table$DetailFraction( response, predictorsForm, metricName
                                         ,30 * best.index, best.value
                                         )
                }
                switch( metricName
                       ,medRMSE = DetailLineMedRMSE()
                       ,fctWI10 = DetailLineFctWI10()
                       ,stop('bad metricName')
                       )
            }

            for (metricName in c('medRMSE', 'fctWI10')) {
                for (response in c('price', 'logprice')) {
                    for (predictorsForm in c('level', 'log')) {
                        DetailLine( response = response
                                   ,predictorsForm = predictorsForm
                                   ,metricName = metricName
                                   ,table = table
                                   )
                    }
                }
            }
        }


        browser()
        lines$Append(' ')
        PartA(lines)
        if (parts.a.and.b) {
            lines$Append(' ')
            PartB(lines)
        }
    }
    AppendTableVertical <- function(lines) {
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
                                         )
                result <-
                    switch( metricName
                           ,medRMSE = MedianRMSE(a.cv.result)
                           ,fctWI10 = MeanWithin10(a.cv.result)
                           ,meanRMSE = MeanRMSE(a.cv.result)
                           ,stop('bad metricName')
                           )

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

    Report <- function(AppendTable, include.asterisk) {
        # produce Lines() object with specified table
        lines <- Lines()
        Header( lines = lines
               ,include.asterisk = include.asterisk
               )

        lines$Append(' ')
        AppendTable(lines)
        lines
    }
    ReportHorizontal <- function() {
        result <- Report( AppendTable = AppendTableHorizontal
                         ,include.asterisk = TRUE
                         )
        result
    }
    ReportVertical <- function() {
        result <- Report( AppendTable = AppendTableVertical
                         ,include.asterisk = FALSE
                         )
        result
    }


    browser()
    result <- list( horizontal = ReportHorizontal()$Get()
                   ,vertical = ReportVertical()$Get()
                   )
    result
}

