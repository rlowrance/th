Chart14 <- function(my.control) {
    # return named list
    # $a : txt lines in vertical form
    # $b:  2 panels, each with a matrix
    verbose <- TRUE

    cv.cell <- CvCell()
    fixed <- cv.cell$FixedCellValues('Chart14')
    Path <- cv.cell$Path

    AppendHeader <- function(report) {
        report$Append('Comparison of Estimated Generalization Errors')
        report$Append('From Random Forests')
        report$Append(' ')
        HeadersFixed(fixed, report)
    }
    MyPath <- function(predictorsName, ntree, mtry) {
        path <- Path( scope          = fixed$scope
                     ,model          = fixed$model
                     ,timePeriod     = fixed$timePeriod
                     ,scenario       = fixed$scenario
                     ,response       = fixed$response
                     ,predictorsName = predictorsName
                     ,predictorsForm = fixed$predictorsForm
                     ,ndays          = fixed$ndays
                     ,query          = fixed$query
                     ,lambda         = fixed$lambda
                     ,ntree          = ntree
                     ,mtry           = mtry
                     )
        path
    }
    ReportA <- function() {
        # return Lines() object

        Table <- function(lines) {
            format.header <- '%12s %12s %12s %6s %6s %6s'
            format.detail <- '%12s %12s %12s %6.0f %6.0f %6.0f'
            format.notfound <- '%12s %12s %12s %s'

            Header <- function(predictorsName, ntree, mtry, median, cilow, cihi) {
                line <- sprintf(format.header, predictorsName, ntree, mtry, median, cilow, cihi)
                lines$Append(line)
            }
            Detail <- function(predictorsName, ntree, mtry, median, cilow, cihi) {
                line <- sprintf(format.detail, predictorsName, ntree, mtry, median, cilow, cihi)
                lines$Append(line)
            }
            NotFound <- function(predictorsName, ntree, mtry, message) {
                line <- sprintf(format.notfound, predictorsName, ntree, mtry, message)
                lines$Append(line)
            }
            Blank <- function() {
                lines$Append(' ')
            }
            list( Header   = Header
                 ,Detail   = Detail
                 ,NotFound = NotFound
                 ,Blank    = Blank
                 )
        }
        report <- Lines()
        AppendHeader(report)

        # create table
        table <- Table(report)
        table$Header('predictors', ' ',     ' ',    ' ',        ' ',       ' ')
        table$Header('name',       'ntree', 'mtry', 'median' , 'ci95%.lo', 'ci95%.hi')
        table$Blank()

        DetailLine <- function(table, predictorsName, ntree, mtry) {
            path <- MyPath( predictorsName = predictorsName
                           ,ntree = ntree
                           ,mtry = mtry
                           )
            if (!file.exists(path)) {
                table$NotFound(predictorsName, ntree, mtry, 'file not found')
                return()
            }

            # retrieve cross validation results from cv-cell file
            loaded <- load(path)
            stopifnot(length(cv.result) == 1)
            a.cv.result <- cv.result[[1]]

            rmse.values <- RootMedianSquaredErrors(a.cv.result)
            ci <- CIMedian(rmse.values)
            table$Detail(predictorsName, ntree, mtry, median(rmse.values), ci$lowest, ci$highest)
        }

        for (predictorsName in c('always', 'best20')) {
            for (ntree in c('1', '100', '300', '1000')) {
                for (mtry in c('1', '2', '3', '4')) {
                    DetailLine(table, predictorsName, ntree, mtry)
                }
            }
        }
        report
    }
    ReportB <- function() {
        # return Lines() object with panels

        Value <- function(predictorsName, ntree, mtry) {
            # return median error across folds
            path <- MyPath( predictorsName = predictorsName
                           ,ntree = ntree
                           ,mtry = mtry
                           )
            loaded <- load(path)
            stopifnot(length(cv.result) == 1)
            a.cv.result <- cv.result[[1]]

            rmse.values <- RootMedianSquaredErrors(a.cv.result)
            result <- median(rmse.values)
            result
        }
        Table <- function(lines) {
            # return named list 
            # $Header() : write header to lines
            # $Detail(ntree, mtry1, mtry2, mtry3, mtry4): write detail line to lines
            format.header <- '%8s %8s %8s %8s %8s'
            format.detail <- '%8s %8.0f %8.0f %8.0f %8.0f'
            Header <- function() {
                # write header
                line <- sprintf(format.header, ' ', 'mtry', ' ', ' ', ' ')
                lines$Append(line)
                
                line <- sprintf(format.header, 'ntree', '1', '2', '3', '4')
                lines$Append(line)
            }
            Detail <- function(ntree, mtry1, mtry2, mtry3, mtry4) {
                # write detail line
                line <- sprintf(format.detail, ntree, mtry1, mtry2, mtry3, mtry4)
                lines$Append(line)
            }
            list( Header = Header
                 ,Detail = Detail
                 )
        }
        Panel <- function(report, tag, description, predictorsName) {
            # append table lines to report, return nothing
            report$Append(' ')
            report$Append(sprintf('Panel %s: %s', tag, description))
            report$Append(' ')
            table <- Table(report)
            table$Header()
            for (ntree in c('1', '100', '300', '1000')) {
                table$Detail( ntree = ntree
                             ,mtry1 = Value(predictorsName, ntree, '1')
                             ,mtry2 = Value(predictorsName, ntree, '2')
                             ,mtry3 = Value(predictorsName, ntree, '3')
                             ,mtry4 = Value(predictorsName, ntree, '4')
                             )
            }
        }
        
        report <- Lines()
        AppendHeader(report)
        Panel(report, 'A', 'using the best 20 predictors', 'best20')
        Panel(report, 'B', 'using all the predictors', 'always')
        report
    }


    report.a <- ReportA()$Get()
    report.b <- ReportB()$Get()
    if (verbose) {
        print(report.a)
        print(report.b)
    }

    result <- list( a = report.a
                   ,b = report.b
                   )
    result
}
