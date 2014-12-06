Chart13 <- function() {
    # return list $Fixed() $CellsUsed() $Chart()
    Fixed <- function() {
        # return list of fixed elements in chart
        result <- list( model = 'linL2'
                       ,timePeriod = '2003on'
                       ,scenario = 'avm'
                       ,response = 'logprice'
                       ,predictorsForm = 'level'
                       ,query = '1'
                       ,ndays = '30'
                       ,lambda = '5500'
                       ,ntree = '0'
                       ,mtry = '0'
                       )
        result
    }
    fixed <- Fixed()
    Path <- CvCell()$Path
    MyPath <- function(scope, predictorsName) {
        path <-Path( scope = scope
                    ,model = fixed$model
                    ,timePeriod = fixed$timePeriod
                    ,scenario = fixed$scenario
                    ,response = fixed$response
                    ,predictorsName = predictorsName
                    ,predictorsForm = fixed$predictorsForm
                    ,ndays = fixed$ndays
                    ,query = fixed$query
                    ,lambda = fixed$lambda
                    ,ntree = fixed$ntree
                    ,mtry = fixed$mtry
                    )
        path
    }
    # load file containing codes for submarkets
    loaded <- load(file = control$path.in.submarkets)
    # now we have variables codes.census.tract, codes.propety.city, codes.zip5


    CellsUsed <- function() {
        # return list of cells used
        result <- list()
        for (scope in c( 'global'
                        ,codes.census.tract
                        ,codes.property.city
                        ,codes.zip5)) {
            for (predictorsName in c( 'best15zip'
                                     ,'best15census'
                                     ,'best15city'
                                     )) {
                cell <- c( fixed
                          ,scope = scope
                          ,predictorsName = predictorsName
                          )
                result[[length(result) + 1]] <- cell
            }
        }
        result
    }

    Chart <- function(my.control) {
        # return list of txt vectors

        verbose <- TRUE
        testing <- FALSE

        debug <- TRUE  # only produce 30 medians per scope
        debug <- FALSE
        if (debug) cat('DEBUG CHART13\n')

        cv.cell <- CvCell()
        C.To.Lambda <- cv.cell$C.To.Lambda


        ACvResult <- function(path) {
            loaded <- load(path)
            stopifnot(length(cv.result) == 1)
            a.cv.result <- cv.result[[1]]
            a.cv.result
        }

        Indicators <- function(metric.name, FoldMetric, FoldSummary) {
            # return Lines() object
            IndicatorsPath <- function(predictorsName) {
                path <- MyPath( scope = 'global',
                               predictorsName = predictorsName
                               )
            }
            report <- Lines()
            report.format.header <- '%15s %12s %12s %12s'
            report.format.detail <- switch( metric.name
                                           ,medRMSE = '%15s %12.0f %12.0f %12.0f'
                                           ,medMARE = '%15s %12.3f %12.3f %12.3f'
                                           ,stop()
                                           )

            report$Append('Estimated Generalization Errors from 10-fold Cross Validation')
            report$Append(sprintf('Using %s from Folds'
                                  ,switch( metric.name
                                          ,medRMSE = 'Median of Root Median Squared Errors'
                                          ,medMARE = 'Median of Median Absolute Relative Errors'
                                          ,stop(paste('bad matric.name', metric.name))
                                          )
                                  )
            )
            report$Append('Across Indicator Models')
            report$Append(' ')
            HeadersFixed(fixed,report)

            line <- sprintf( report.format.header
                            ,'indicators for'
                            ,metric.name
                            ,'ci95%.lo'
                            ,'ci95%.hi'
                            )
            report$Append(' ')
            report$Append(line)

            IndicatorDetail <- function(path, report, indicators.for) {
                # add detail line to report
                # produce analysis for a.cv.result in path file
                if (file.exists(path)) {
                    a.cv.result <- ACvResult(path)
                    
                    fold.values <- FoldMetric(a.cv.result)
                    ci <- CIMedian(fold.values)
                    line <- sprintf( report.format.detail 
                                    ,indicators.for
                                    ,FoldSummary(fold.values)
                                    ,ci$lowest
                                    ,ci$highest
                                    )
                } else {
                    cat('missing file: ', path, '\n')
                    browser()
                    line <- sprintf( report.format.detail
                                    ,indicators.for
                                    ,NA
                                    ,NA
                                    ,NA
                                    )
                    stop()
                }
                report$Append(line)
            }
            IndicatorDetail( IndicatorsPath('best15')
                            ,report
                            ,'no indicators'
                            )
            for (predictorsName in c( 'best15zip'
                                     ,'best15census'
                                     ,'best15city'
                                     )) {
                path <- IndicatorsPath(predictorsName)
                indicators.for <- switch(predictorsName
                                         ,best15zip = 'zip 5 code'
                                         ,best15census = 'census tract'
                                         ,best15city = 'city'
                                         )
                IndicatorDetail(path, report, indicators.for)
            }
            result <- report
            result
        }
        Submarkets <- function(metric.name, FoldMetric, FoldSummary) {
            # return list of Lines() objects, those returned by Analyze()
            Analyze <- function() {
                # return a list of 4 reports
                # $summary               : Lines object with summary report (3 detail lines)
                # $details.census.tract  : Lines object with details for census.tract
                # $details.property.city : Lines object
                # $details.zip5          : Lines object
                # $examples.census.tract : Lines object
                # $examples.property.city: Lines object
                # $examples.zip5         : Lines object
                verbose <- TRUE

                loaded <- load(file = my.control$path.in.submarkets)
                analysis.census.tract  <-  AnalyzeScopes(codes.census.tract)
                analysis.property.city <-  AnalyzeScopes(codes.property.city)
                analysis.zip5          <-  AnalyzeScopes(codes.zip5)

                report <- Lines()

                report$Append('Estimated Generalization Errors from 10-fold Cross Validation')
                report$Append(sprintf('Using $s from Folds'
                                      ,switch( metric.name
                                              ,medRMSE = 'Median of Root Median Squared Errors'
                                              ,medMARE = 'Median of Median Absolute Relative Errors'
                                              ,stop()
                                              )
                                      )
                )
                report$Append('Across Submarket Models')

                # fix query, which should be 1 (not 100)
                HeadersFixed(fixed, report)

                report$Append(' ')
                report.format.header <- '%20s %8s %12s %12s %8s %8s %8s'
                report.format.detail <- switch( metric.name
                                               ,medRMSE = '%20s %8.0f %12.0f %12.0f %8.0f %8.0f %8.2f'
                                               ,medMARE = '%20s %8.3f %12.3f %12.3f %8.0f %8.0f %8.2f'
                                               ,stop())
                line <- sprintf( report.format.header
                                ,'scope name'
                                ,metric.name
                                ,'95%ci.low'
                                ,'95%ci.hi'
                                ,'nScope'
                                ,'nAlways'
                                ,'coverage'
                                )
                if (verbose) cat(line, '\n')
                report$Append(line)

                Detail <- function(scope.name, analysis, num.scopes, report) {
                    # append detail line to report
                    line <- sprintf( report.format.detail
                                    ,scope.name
                                    ,analysis$median
                                    ,analysis$median.ci.lowest
                                    ,analysis$median.ci.highest
                                    ,num.scopes
                                    ,analysis$num.10fold.medians.found
                                    ,analysis$num.10fold.medians.found / analysis$num.scope.files.found
                                    )
                    if (verbose) cat(line, '\n')
                    report$Append(line)
                }

                Detail('census.tract', analysis.census.tract, length(codes.census.tract), report)
                Detail('property.city', analysis.property.city, length(codes.property.city), report)
                Detail('zip5', analysis.zip5, length(codes.zip5), report)

                result <- list( summary = report
                               ,details.census.tract = analysis.census.tract$detail.report
                               ,details.property.city = analysis.property.city$detail.report
                               ,details.zip5 = analysis.zip5$detail.report
                               ,examples.census.tract = Examples( analysis.census.tract$scope.medians.named
                                                                 ,'Census Tract' 
                                                                 )
                               ,examples.property.city = Examples( analysis.property.city$scope.medians.named
                                                                  ,'Property City'
                                                                  )
                               ,examples.zip5 = Examples( analysis.zip5$scope.medians.named
                                                         ,'Zip5'
                                                         )
                               )
                result
            }
            Examples <- function(scope.medians, scope.name) {
                lines <- Lines()
                lines$Append('Example of Estimated Generalization Errors from 10-fold Cross Validation')
                lines$Append(sprintf('Using %s from Folds'
                                     ,switch( metric.name
                                             ,medRMSE = 'Median of Root Median Squared Errors'
                                             ,medMARE = 'Median of Median Absolute Relative Errors'
                                             ,stop()
                                             )
                                     )
                )
                lines$Append(paste0('Examples of Models for Scope ', scope.name))
                lines$Append(' ')

                format.header <- '%30s %6s'
                format.detail <- switch( metric.name
                                        ,medRMSE = '%30s %6.0f'
                                        ,medMARE = '%30s %6.3f'
                                        )
                Header <- function() {
                    line <- sprintf( format.header
                                    ,'scope name'
                                    ,metric.name
                                    )
                    lines$Append(line)
                }
                Detail <- function(scope.name, median.value) {
                    line <- sprintf(format.detail, scope.name, median.value)
                    lines$Append(line)
                }
                Details <- function(selection.text, scope.median.entries) {
                    lines$Append(' ')
                    lines$Append(selection.text)
                    for (scope.name in names(scope.median.entries)) {
                        Detail(scope.name, scope.median.entries[[scope.name]])
                    }
                }

                Header()
                sorted <- sort(scope.medians)
                n.scope.medians <- length(scope.medians)
                mid <- round(n.scope.medians / 2)
                Details('lowest 10 medians', sorted[1:10])
                Details('middle 10 medians', sorted[(mid - 5) : (mid + 4)])
                Details('highest 10 medians', sorted[(length(sorted) - 9): length(sorted)])
                lines
            }
            AnalyzeScopes <- function(scopes) {
                # return list
                # $detail.report: Lines() object for each scope
                # $median
                # $median.ci.lowest
                # $median.ci.highest
                # $num.scope.files.found
                # $num.10fold.medians.found
                # $scope.medians
                verbose <- TRUE
                report <- Lines()
                report.format.header                       <- '%20s %9s %6s %6s %6s'
                report.format.detail.file.exists           <- '%20s %9.3f %6d %6.0f %6.0f'
                report.format.detail.file.exists.no.median <- '%20s no median'
                report.format.detail.file.not.exist        <- '%20s scope file not found'
                median.list <- NULL

                header.line <- sprintf( report.format.header
                                       ,'scope'
                                       ,'median'
                                       ,'N'
                                       ,'ci.low'
                                       ,'ci.high'
                                       )
                report$Append(header.line)

                num.scope.files.found <- 0
                num.10fold.medians.found <- 0
                scope.medians <- NULL
                scope.medians.named <- NULL
                for (scope in scopes) {
                    maybe <- MaybeAnalyzeScope(scope)
                    if (maybe$ok) {
                        # scope file exists
                        value <- maybe$value
                        num.scope.files.found <- num.scope.files.found + 1 
                        if (value$num.rmse.values == 10) {
                            num.10fold.medians.found <- num.10fold.medians.found + 1 
                            scope.medians[[length(scope.medians) + 1]] <- value$median.of.medians
                            scope.medians.named[[scope]] <- value$median.of.medians
                            if (debug && length(scope.medians) >= 30) break
                        }
                        line <-
                            if (is.na(value$median.of.medians))
                                sprintf( report.format.detail.file.exists.no.median
                                        ,scope)
                            else
                                sprintf( report.format.detail.file.exists
                                        ,scope
                                        ,value$median.of.medians
                                        ,value$num.rmse.values
                                        ,value$median.lowest.ci
                                        ,value$median.highest.ci
                                        )
                        if (verbose) cat(line, '\n')
                        report$Append(line)
                    } else {
                        # scope file does not exist
                        line <- sprintf( report.format.detail.file.not.exist
                                        ,scope
                                        )
                        if (verbose) cat(line, '\n')
                        report$Append(line)
                    }
                }
                median.scope.medians <- median(scope.medians)
                ci <- CIMedian(scope.medians)
                result <- list( detail.report = report
                               ,median = median.scope.medians
                               ,median.ci.lowest = ci$lowest
                               ,median.ci.highest = ci$highest
                               ,num.scope.files.found = num.scope.files.found
                               ,num.10fold.medians.found = num.10fold.medians.found
                               ,scope.medians.named = scope.medians.named
                               )
                result
            }
            SumarketsPath <- function(scope) {
                # return path to cv cell in file system
                path <- MyPath( scope = scope
                               ,predictorsName = 'best15'
                               )
                path
            }
            MaybeAnalyzeScope <- function(scope) {
                # return list
                # $ok = TRUE (the scope has been computed) or FALSE (is has not)
                # $value: NA or a list $median.of.medians $num.rmse.values $median.ci.lowest $median.ci.highest
                path <- SumarketsPath(scope)
                if (file.exists(path))
                    list( ok = TRUE
                         ,value = AnalyzeScope(path)
                         )
                else
                    list( ok = FALSE
                         ,value = NA
                         )
            }
            AnalyzeScope <- function(path) {
                # return analysis of the scope as a list
                # $median.of.medians: NA or numeric
                # $num.rmse.values: num
                # $median.lowest.ci: num
                # $median.highest.ci: num

                # produce analysis for a.cv.result in path file
                # return median value for path
                a.cv.result <- ACvResult(path)
                rmse.values <- RootMedianSquaredErrors(a.cv.result)
                num.folds.with.medians <- sum(!is.na(rmse.values))
                if (num.folds.with.medians == 0)
                    list( median.of.medians = NA
                         ,num.rmse.values = num.folds.with.medians
                         ,median.lowest.ci = NA
                         ,median.highest.ci = NA
                         )
                else  {
                    fold.values <- FoldMetric(a.cv.result)
                    ci <- CIMedian(rmse.values, removeNAs = TRUE, debug = FALSE)
                    list( median.of.medians = FoldSummary(fold.values)
                         ,num.rmse.values = num.folds.with.medians
                         ,median.lowest.ci = ci$lowest
                         ,median.highest.ci = ci$highest
                         )
                }
            }

            # EXECUTION STARTS HERE
            result <- Analyze()
            result
        }


        Median <- function(v) median(v, na.rm = TRUE)

        AREErrors <- function(a.cv.result) {
            are.folds <- AbsoluteRelativeErrors(a.cv.result)
            median.are.folds <- sapply(are.folds, function(are) median(are, na.rm = TRUE))
            median.are.folds
        }


        submarkets.medRMSE <- Submarkets('medRMSE', RootMedianSquaredErrors, Median)
        submarkets.medMARE <- Submarkets('medMARE', AREErrors, Median)

        result <-
            list( indicators.medRMSE = Indicators('medRMSE', RootMedianSquaredErrors, Median)$Get()
                 ,indicators.medMARE = Indicators('medMARE', AREErrors, Median)$Get()

                 ,submarkets.summary.medRMSE = submarkets.medRMSE$summary$Get()
                 ,submarkets.summary.medMARE = submarkets.medMARE$summary$Get()

                 ,submarkets.census.medRMSE         = submarkets.medRMSE$details.census.tract$Get()
                 ,submarkets.census.medMARE         = submarkets.medMARE$details.census.tract$Get()

                 ,submarkets.property.city.medRMSE  = submarkets.medRMSE$details.property.city$Get()
                 ,submarkets.property.city.medMARE  = submarkets.medMARE$details.property.city$Get()

                 ,submarkets.zip5.medRMSE           = submarkets.medRMSE$details.zip5$Get()
                 ,submarkets.zip5.medMARE           = submarkets.medMARE$details.zip5$Get()

                 ,submarkets.examples.census.medRMSE = submarkets.medRMSE$examples.census.tract$Get()
                 ,submarkets.examples.census.medMARE = submarkets.medMARE$examples.census.tract$Get()

                 ,submarkets.examples.property.city.medRMSE = submarkets.medRMSE$examples.property.city$Get()
                 ,submarkets.examples.property.city.medMARE = submarkets.medMARE$examples.property.city$Get()

                 ,submarkets.examples.zip5.medRMSE          = submarkets.medRMSE$examples.zip5$Get()
                 ,submarkets.examples.zip5.medMARE          = submarkets.medMARE$examples.zip5$Get()
                 )
        result
    }


    result <- list( CellsUsed = CellsUsed
                   ,Chart = Chart
                   ,Fixed = Fixed
                   )
    result
}


