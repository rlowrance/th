Chart14 <- function(my.control) {
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

    verbose <- TRUE

    cv.cell <- CvCell()
    fixed <- cv.cell$FixedCellValues('Chart14')
    Path <- cv.cell$Path

    report <- Lines()
    report$Append('Comparison of Estimated Generalization Errors')
    report$Append('From Random Forests')
    report$Append(' ')
    HeadersFixed(fixed, report)

    # create table
    table <- Table(report)
    table$Header('predictors', ' ',     ' ',    ' ',        ' ',       ' ')
    table$Header('name',       'ntree', 'mtry', 'median' , 'ci95%.lo', 'ci95%.hi')
    table$Blank()

    DetailLine <- function(table, predictorsName, ntree, mtry) {
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
    result <- report$Get()
    if (verbose) print(result)
    result
}
