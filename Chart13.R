Chart13 <- function(my.control) {
    # return list of txt vectors

    verbose <- TRUE

    cv.cell <- CvCell()
    fixed <- cv.cell$FixedCellValues('Chart13')
    Path <- cv.cell$Path
    C.To.Lambda <- cv.cell$C.To.Lambda


    # for now, figure out what files have been created
    DetermineIndicators <- function() {
        created <- Lines()
        togo <- Lines()
        for (predictorsName in c( 'best20zip'
                                 ,'best20census'
                                 ,'best20city'
                                 )) {
            path <-Path( scope = 'global'
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
            if (file.exists(path)) 
                created$Append(path)
            else {
                cat('missing indicator cell: ', path, '\n')
                togo$Append(path)
            }
        }
        result <- list( created = created$Get()
                       ,togo = togo$Get()
                       )
        result
    }
    DetermineSubmarkets <- function() {
        created <- Lines()
        togo <- Lines()
        loaded <- load(file = control$path.in.submarkets)
        for (scope in c(codes.census.tract, codes.property.city, codes.zip5)) {
            path <-Path( scope = scope
                        ,model = fixed$model
                        ,timePeriod = fixed$timePeriod
                        ,scenario = fixed$scenario
                        ,response = fixed$response
                        ,predictorsName = 'best20'
                        ,predictorsForm = fixed$predictorsForm
                        ,ndays = fixed$ndays
                        ,query = '1'  # 100 percent sample (each scope tends to be small)
                        ,lambda = fixed$lambda
                        ,ntree = fixed$ntree
                        ,mtry = fixed$mtry
                        )
            if (file.exists(path)) 
                created$Append(path)
            else
                togo$Append(path)
        }
        result <- list( created = created$Get()
                       ,togo = togo$Get()
                       )
        result
    }
        
    indicators <- DetermineIndicators()
    Printf( 'created %d indicators; still have %d to go\n'
           ,length(indicators$created)
           ,length(indicators$togo)
           )
    submarkets <- DetermineSubmarkets()
    Printf( 'created %d submarkets; still have %d to go\n'
           ,length(submarkets$created)
           ,length(submarkets$togo)
           )

    ACvResult <- function(path) {
        loaded <- load(path)
        stopifnot(length(cv.result) == 1)
        a.cv.result <- cv.result[[1]]
        a.cv.result
    }

    Indicators.Txt <- function() {
        # return Lines() object
        Indicators.Path <- function(predictorsName) {
            path <-Path( scope = 'global'
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
        }
        report <- Lines()
        report.format.header <- '%15s %12s %12s %12s'
        report.format.detail <- '%15s %12.0f %12.0f %12.0f'
        report$Append('Comparison of Estimated Generalization Errors for Indicator Models')
        report$Append(' ')
        HeadersFixed(fixed,report)

        line <- sprintf( report.format.header
                        ,'indicators for'
                        ,'median'
                        ,'ci95%.lo'
                        ,'ci95%.hi'
                        )
        report$Append(' ')
        report$Append(line)

        Indicators.Detail <- function(path, report, indicators.for) {
            # add detail line to report
            # produce analysis for a.cv.result in path file
            a.cv.result <- ACvResult(path)
            rmse.values <- RootMedianSquaredErrors(a.cv.result)
            ci <- CIMedian(rmse.values)
            line <- sprintf( report.format.detail 
                            ,indicators.for
                            ,median(rmse.values)
                            ,ci$lowest
                            ,ci$highest
                            )
            report$Append(line)
        }
        for (predictorsName in c( 'best20zip'
                                 ,'best20census'
                                 ,'best20city'
                                 )) {
            path <- Indicators.Path(predictorsName)
            if (file.exists(path)) {
                indicators.for <- switch(predictorsName
                                         ,best20zip = 'zip 5 code'
                                         ,best20census = 'census tract'
                                         ,best20city = 'city'
                                         )
                Indicators.Detail(path, report, indicators.for)
            } else {
                cat('MISSING FILE CHART 11', path, '\n')
                #stop('all files should exist')
            }
        }
        result <- report
        result
    }
    Submarkets.Txt <- function() {
        # return list of Lines() objects
        # summary: summary aross census, city, zip
        # census : details for the census tracts
        # city   : details for the property cities
        # zip    : details for the zip5 code
        Analyze <- function() {
            # return a list of 4 reports
            # $summary              : Lines object with summary report (3 detail lines)
            # $details.census.tract : Lines object with details for census.tract
            # $details.property.city: Lines object
            # $details.zip5         : Lines object
            verbose <- TRUE

            loaded <- load(file = my.control$path.in.submarkets)
            analysis.census.tract <- Analyze.Scopes(codes.census.tract)
            analysis.property.city <-  Analyze.Scopes(codes.property.city)
            analysis.zip5 <- Analyze.Scopes(codes.zip5)

            report <- Lines()

            report$Append('Comparison of Estimated Generalization Errors for Various Submarkets')
            report$Append(' ')
            HeadersFixed(fixed, report)

            report$Append(' ')
            report.format.header <- '%20s %8s %12s %12s %8s %8s %8s %8s'
            report.format.detail <- '%20s %8.0f %12.0f %12.0f %8.0f %8.0f %8.0f %8.2f'
            line <- sprintf( report.format.header
                            ,'scope name'
                            ,'median'
                            ,'95%ci.low'
                            ,'95%ci.hi'
                            ,'nFound'
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
                                ,analysis$num.scope.files.found
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
                           )
            result
        }
        Analyze.Scopes <- function(scopes) {
            # return list
            # $detail.report: Lines() object for each scope
            # $median
            # $median.ci.lowest
            # $median.ci.highest
            # $num.scope.files.found
            # $num.10fold.medians.found
            debug <- FALSE
            if (debug) {
                cat('DEBUGGING\n')
                scopes <- scopes[1:40]
            }
            verbose <- TRUE
            report <- Lines()
            report.format.header                       <- '%20s %6s %6s %6s %6s'
            report.format.detail.file.exists           <- '%20s %6.0f %6d %6.0f %6.0f'
            report.format.detail.file.exists.no.median <- '%20s no median'
            report.format.detail.file.not.exist        <- '%20s scope file not found'

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
            for (scope in scopes) {
                maybe <- Maybe.Analyze.Scope(scope)
                if (maybe$ok) {
                    # scope file exists
                    value <- maybe$value
                    num.scope.files.found <- num.scope.files.found + 1 
                    if (value$num.rmse.values == 10) {
                        num.10fold.medians.found <- num.10fold.medians.found + 1 
                        scope.medians[[length(scope.medians) + 1]] <- value$median.of.medians
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
                           )
            result
        }
        Submarkets.Path <- function(scope) {
            # return path to cv cell in file system
            path <-Path( scope = scope
                        ,model = fixed$model
                        ,timePeriod = fixed$timePeriod
                        ,scenario = fixed$scenario
                        ,response = fixed$response
                        ,predictorsName = 'best20'
                        ,predictorsForm = fixed$predictorsForm
                        ,ndays = fixed$ndays
                        ,query = '1'  # 100 percent sample (each scope tends to be small)
                        ,lambda = fixed$lambda
                        ,ntree = fixed$ntree
                        ,mtry = fixed$mtry
                        )
            path
        }
        Maybe.Analyze.Scope <- function(scope) {
            # return list
            # $ok = TRUE (the scope has been computed) or FALSE (is has not)
            # $value: NA or a list $median.of.medians $num.rmse.values $median.ci.lowest $median.ci.highest
            path <- Submarkets.Path(scope)
            if (file.exists(path))
                list( ok = TRUE
                     ,value = Analyze.Scope(path)
                     )
            else
                list( ok = FALSE
                     ,value = NA
                     )
        }
        Analyze.Scope <- function(path) {
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
                ci <- CIMedian(rmse.values, removeNAs = TRUE, debug = FALSE)
                list( median.of.medians = median(rmse.values, na.rm = TRUE)
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



    indicators <- Indicators.Txt()
    submarkets <- Submarkets.Txt()
    print(indicators$Get())
    print(submarkets$summary$Get())


    result <- list( indicators = indicators$Get()
                   ,submarkets.summary = submarkets$summary$Get()
                   ,submarkets.census = submarkets$details.census.tract$Get()
                   ,submarkets.property.city = submarkets$details.property.city$Get()
                   ,submarkets.zip5 = submarkets$details.zip5$Get()
                   )
    return(result)
}

