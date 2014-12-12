Chart14 <- function(my.control) {
    # return list $Fixed() $CellsUsed() $Chart()
    Fixed <- function() {
        result = list( scope = 'global'
                      ,model = 'rf'
                      ,scenario = 'avm'
                      ,timePeriod = '2003on'
                      ,response = 'logprice'
                      ,predictorsForm = 'level'
                      ,lambda = '0'
                      )
    }

    fixed <- Fixed()
    Path <- CvCell()$Path
    
    MyPath <- function(predictorsName, ntree, mtry, ndays, query) {
        path <- Path( scope          = fixed$scope
                     ,model          = fixed$model
                     ,timePeriod     = fixed$timePeriod
                     ,scenario       = fixed$scenario
                     ,response       = fixed$response
                     ,predictorsName = predictorsName
                     ,predictorsForm = fixed$predictorsForm
                     ,ndays          = ndays
                     ,query          = query
                     ,lambda         = fixed$lambda
                     ,ntree          = ntree
                     ,mtry           = mtry
                     )
        path
    }
    CellsUsed <- function() {
        # return list of paths to cells used
        verbose <- FALSE
        result <- list()
        
        # for the paper, which used randomly-generated cells
        path.in <- paste0(Directory('working'), 'randomforests-hyperparameters.csv')
        random <- read.csv(path.in)
        if (verbose)
            print(summary(random))
        for (row.index in 1:nrow(random)) {
            cell <- list( scope = fixed$scope
                         ,model = fixed$model
                         ,timePeriod = fixed$timePeriod
                         ,scenario = fixed$scenario
                         ,response = fixed$response
                         ,predictorsName = sprintf('best%02d', random$bestN[[row.index]])
                         ,predictorsForm = fixed$predictorsForm
                         ,ndays = sprintf('%2d', random$ndays[[row.index]])
                         ,query = '100'  # 1% sample
                         ,lambda = fixed$lambda
                         ,ntree = sprintf('%d', random$ntree[[row.index]])
                         ,mtry = sprintf('%d', random$mtry[[row.index]])
                         )
            result[[length(result) + 1]] <- cell
        }

        # for the dissertation, used fixed cells
        for (query in c('20', '100')) {
            for (ntree in c('1', '100', '300', '1000')) {
                for (mtry in c('1', '2', '3', '4')) {
                    for (ndays in c('30', '60')) {
                        cell <- c( fixed
                                  ,query = query
                                  ,ntree = ntree
                                  ,mtry = mtry
                                  ,ndays = ndays
                                  )
                        result[[length(result) + 1]] <- cell
                    }
                }
            }
        }

        result
    }
    Chart <- function(my.control) {
        # return named list
        # $a : txt lines in vertical form
        # $b:  2 panels, each with a matrix
        verbose <- TRUE

        AppendHeader <- function(lines, ndays, query) {
            lines$Append('Comparison of Estimated Generalization Errors')
            lines$Append('From Random Forests')
            HeadersFixed(fixed, lines)
            lines$Append(paste0('Number of training days: ', ndays))
            lines$Append(paste0('Percent of queries in each fold that were estimated: '
                                ,switch( query
                                        ,'100' = '1'
                                        ,'20' = '5'
                                        ,'1' = '100'
                                        )
                                )
            )
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
        Value <- function(predictorsName, ntree, mtry, ndays, query) {
            # return median error across folds
            path <- MyPath( predictorsName = predictorsName
                           ,ntree = ntree
                           ,mtry = mtry
                           ,ndays = ndays
                           ,query = query
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
        AppendPanel <- function(lines, tag, description, predictorsName, ndays, query) {
            # append table lines to lines, return nothing
            lines$Append(' ')
            lines$Append(sprintf('Panel %s: %s', tag, description))
            lines$Append(' ')
            table <- Table(lines)
            table$Header()
            for (ntree in c('1', '100', '300', '1000')) {
                table$Detail( ntree = ntree
                             ,mtry1 = Value(predictorsName, ntree, '1', ndays, query)
                             ,mtry2 = Value(predictorsName, ntree, '2', ndays, query)
                             ,mtry3 = Value(predictorsName, ntree, '3', ndays, query)
                             ,mtry4 = Value(predictorsName, ntree, '4', ndays, query)
                             )
            }
        }

        Report1Panel <- function(ndays, msg, predictorsName, query) {
            lines <- Lines()
            AppendHeader(lines, ndays, query)
            AppendPanel(lines, 'A', msg, predictorsName, ndays, query)
            lines
        }
        Report2Panels <- function(ndays, msg.1, predictorsName.1, msg.2, predictorsName.2, query) {
            lines <- Lines()
            AppendHeader(lines, ndays, query)
            AppendPanel(lines, 'A', msg.1, predictorsName.1, ndays, query)
            AppendPanel(lines, 'B', msg.2, predictorsName.2, ndays, query)
            lines
        }

        report.1.30 <- Report2Panels( ndays = '30'
                                     ,msg.1 = 'using the best 15 predictors'
                                     ,predictorsName.1 = 'best15'
                                     ,msg.2 = 'using all predictors except assessment'
                                     ,predictorsName.2 = 'alwaysNoAssessment'
                                     ,query = '100'
                                     )
        report.1.60 <- Report2Panels( ndays = '60'
                                     ,msg.1 = 'using the best 15 predictors'
                                     ,predictorsName.1 = 'best15'
                                     ,msg.2 = 'using all predictors except assessment'
                                     ,predictorsName.2 = 'alwaysNoAssessment'
                                     ,query = '100'
                                     )
        report.5.30 <- Report2Panels( ndays = '30'
                                     ,msg.1 = 'using the best 15 predictors'
                                     ,predictorsName.1 = 'best15'
                                     ,msg.2 = 'using all predictors except assessment'
                                     ,predictorsName.2 = 'alwaysNoAssessment'
                                     ,query = '20'
                                     )
        report.5.60 <- Report2Panels( ndays = '60'
                                     ,msg.1 = 'using the best 15 predictors'
                                     ,predictorsName.1 = 'best15'
                                     ,msg.2 = 'using all predictors except assessment'
                                     ,predictorsName.2 = 'alwaysNoAssessment'
                                     ,query = '20'
                                     )

        result <- list( txt.1.30 = report.1.30$Get()  # 1% sample for 30 days
                       ,txt.1.60 = report.1.60$Get()  # 1% sample for 60 days
                       ,txt.5.30 = report.5.30$Get()  # 5% sample for 30 days
                       ,txt.5.60 = report.5.60$Get()  # 5% sample for 60 days
                       )
        result
    }
    result <- list( CellsUsed = CellsUsed
                   ,Chart = Chart
                   ,Fixed = Fixed
                   )
}
