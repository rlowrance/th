Chart8 <- function(my.control) {
    # return list
    # $horizontal: txt lines for horizontal version of chart 8
    # $vertical  : txt lines for vertical version of chart 8
    # return txt lines for chart 8

    cv.cell <- CvCell()
    fixed <- cv.cell$FixedCellValues('Chart8')
    Path <- cv.cell$Path

    ACvResult <- function(ndays, response, predictorsForm) {
        # return the single cv.result in the e-cv-cell for ndays
        path.in <- Path( scope = 'global'
                        ,model = 'linear'
                        ,timePeriod = '2003on'
                        ,scenario = 'avm'
                        ,response = response
                        ,predictorsName = 'alwaysNoAssessment'
                        ,predictorsForm = predictorsForm
                        ,ndays = ndays
                        ,query = '100'  # use 1% sample
                        ,lambda = '0'
                        ,ntree = '0'
                        ,mtry = '0'
                        )
        load(path.in)
        stopifnot(!is.null(cv.result))
        stopifnot(length(cv.result) == 1)
        a.cv.result <- cv.result[[1]]
        a.cv.result
    }
    Header <- function(lines) {
        # mutate lines by appending the header
        lines$Append('Comparison of Estimated Generalization Errors From from 10 Fold Cross Validation')
        lines$Append('Using Root Median Squared Errors from Folds')
        lines$Append('Across Model Form and Length of Training Period')

        stopifnot(fixed$scope == 'global')
        stopifnot(fixed$model == 'linear')
        stopifnot(fixed$timePeriod == '2003on')
        stopifnot(fixed$predictorsName == 'alwaysNoAssessment')
        stopifnot(fixed$query == '100')

        HeadersFixed(fixed, lines)
    }
    AppendTableHorizontal <- function(lines) {
        # append lines for Table 8 to Lines object lines
        table <- Table8Horizontal(lines)
        table$Header1('pred', 'ndays')
        table$Header2( 'response', 'form'
                      ,'30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360'
                      )

        DetailLine <- function(response, predictorsForm) {
            Value <- function(ndays) {
                a.cv.result <- ACvResult( ndays = ndays
                                         ,response = response
                                         ,predictorsForm = predictorsForm
                                         )
                result <- MedianRMSE(a.cv.result)
                result
            }
            table$Detail( response, predictorsForm
                         ,Value(30),   Value(60),  Value(90), Value(120), Value(150), Value(180)
                         ,Value(210), Value(240), Value(270), Value(300), Value(330), Value(360)
                         )
        }


        # generate each detail line
        for (response in c('price', 'logprice')) {
            for (predictorsForm in c('level', 'log')) {
                DetailLine( response = response
                           ,predictorsForm = predictorsForm
                           )
            }
        }
    }
    AppendTableVertical <- function(lines) {
        # append to Lines() object
        table <- Table8Vertical(lines)
        table$Header('response:', 'price', 'price', 'logprice', 'logprice')
        table$Header('predictorsForm:', 'level', 'log', 'level', 'log')
        table$Header('ndays', ' ', ' ', ' ', ' ')

        DetailLine <- function(ndays) {
            Value <- function(ndays, response, predictorsForm) {
                a.cv.result <- ACvResult( ndays = ndays
                                         ,response = response
                                         ,predictorsForm = predictorsForm
                                         )
                result <- MedianRMSE(a.cv.result)
                result
            }
            table$Detail( ndays
                         ,Value(ndays, 'price', 'level')
                         ,Value(ndays, 'price', 'log')
                         ,Value(ndays, 'logprice', 'level')
                         ,Value(ndays, 'logprice', 'log')
                         )
        }
        for (ndays in c('30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360')) {
            DetailLine(ndays)
        }
        lines
    }

    Report <- function(AppendTable) {
        # produce Lines() object with specified table
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

    # body starts here

    result <- list( horizontal = ReportHorizontal()$Get()
                   ,vertical   = ReportVertical()$Get()
                   )
    result
}

