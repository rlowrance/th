Chart14 <- function(my.control) {
    # return named list
    # $a : txt lines in vertical form
    # $b:  2 panels, each with a matrix
    verbose <- TRUE

    cv.cell <- CvCell()
    fixed <- cv.cell$FixedCellValues('Chart14')
    Path <- cv.cell$Path

    AppendHeader <- function(lines, ndays) {
        lines$Append('Comparison of Estimated Generalization Errors')
        lines$Append('From Random Forests')
        HeadersFixed(fixed, lines)
        lines$Append(paste0('Number of training days: ', ndays))
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
    MyPath <- function(predictorsName, ntree, mtry, ndays) {
        path <- Path( scope          = fixed$scope
                     ,model          = fixed$model
                     ,timePeriod     = fixed$timePeriod
                     ,scenario       = fixed$scenario
                     ,response       = fixed$response
                     ,predictorsName = predictorsName
                     ,predictorsForm = fixed$predictorsForm
                     ,ndays          = ndays
                     ,query          = fixed$query
                     ,lambda         = fixed$lambda
                     ,ntree          = ntree
                     ,mtry           = mtry
                     )
        path
    }
    Value <- function(predictorsName, ntree, mtry, ndays) {
        # return median error across folds
        path <- MyPath( predictorsName = predictorsName
                       ,ntree = ntree
                       ,mtry = mtry
                       ,ndays = ndays
                       )
        if (file.exists(path)) {
            loaded <- load(path)
            stopifnot(length(cv.result) == 1)
            a.cv.result <- cv.result[[1]]

            rmse.values <- RootMedianSquaredErrors(a.cv.result)
            result <- median(rmse.values)
            result
        } else {
            cat('missing file: ', path, '\n')
            result <- NA
            result
        }
    }
    AppendPanel <- function(lines, tag, description, predictorsName, ndays) {
        # append table lines to lines, return nothing
        lines$Append(' ')
        lines$Append(sprintf('Panel %s: %s', tag, description))
        lines$Append(' ')
        table <- Table(lines)
        table$Header()
        for (ntree in c('1', '100', '300', '1000')) {
            table$Detail( ntree = ntree
                         ,mtry1 = Value(predictorsName, ntree, '1', ndays)
                         ,mtry2 = Value(predictorsName, ntree, '2', ndays)
                         ,mtry3 = Value(predictorsName, ntree, '3', ndays)
                         ,mtry4 = Value(predictorsName, ntree, '4', ndays)
                         )
        }
    }

    Report1Panel <- function(ndays, msg, predictorsName) {
        lines <- Lines()
        AppendHeader(lines, ndays)
        AppendPanel(lines, 'A', msg, predictorsName, ndays)
        lines
    }
    Report2Panels <- function(ndays, msg.1, predictorsName.1, msg.2, predictorsName.2) {
        lines <- Lines()
        AppendHeader(lines, ndays)
        AppendPanel(lines, 'A', msg.1, predictorsName.1, ndays)
        AppendPanel(lines, 'B', msg.2, predictorsName.2, ndays)
        lines
    }

    report.2.30 <- Report2Panels( ndays = '30'
                                 ,msg.1 = 'using the best 15 predictors'
                                 ,predictorsName.1 = 'best15'
                                 ,msg.2 = 'using all predictors except assessment'
                                 ,predictorsName.2 = 'alwaysNoAssessment'
                                 )
    report.1.60 <- Report1Panel( ndays = '60'
                                ,msg = 'using all predictors except assessment'
                                ,predictorsName = 'alwaysNoAssessment'
                                )

    result <- list( panels.2.30 = report.2.30$Get()
                   ,panels.1.60 = report.1.60$Get()
                   )
    result
}
