Chart8 <- function(my.control) {
    # return list $Fixed() $Chart() $CellsUsed()
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
        # return list
        # $horizontal: txt lines for horizontal version of chart 8
        # $vertical  : txt lines for vertical version of chart 8
        # return txt lines for chart 8

        cv.cell <- CvCell()
        fixed <- cv.cell$FixedCellValues('Chart8')
        Path <- cv.cell$Path

        ACvResult <- function(ndays, response, predictorsForm, query) {
            # return the single cv.result in the e-cv-cell for ndays
            path.in <- MyPath( response = response
                              ,predictorsForm = predictorsForm
                              ,ndays = ndays
                              ,query = query
                              )
            load(path.in)
            stopifnot(!is.null(cv.result))
            stopifnot(length(cv.result) == 1)
            a.cv.result <- cv.result[[1]]
            a.cv.result
        }
        Header <- function(lines, query) {
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
            table <- Table8Vertical(lines)
            table$Header('response:', 'price', 'price', 'logprice', 'logprice')
            table$Header('predictorsForm:', 'level', 'log', 'level', 'log')
            table$Header('ndays', ' ', ' ', ' ', ' ')

            DetailLine <- function(ndays) {
                Value <- function(ndays, response, predictorsForm) {
                    a.cv.result <- ACvResult( ndays = ndays
                                             ,response = response
                                             ,predictorsForm = predictorsForm
                                             ,query = query
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

        Report <- function(AppendTable, query) {
            # produce Lines() object with specified table
            lines <- Lines()
            Header(lines, query)

            lines$Append(' ')
            AppendTable(lines, query)
            lines
        }
        ReportVertical <- function(query) {
            result <- Report(AppendTableVertical, query)
            result
        }

        # body starts here

        result <- list( txt = ReportVertical('1')$Get()    # 100% sample
                       )
        result
    }
    result <- list( CellsUsed = CellsUsed
                   ,Chart = Chart
                   ,Fixed = Fixed
                   )
    result
}

