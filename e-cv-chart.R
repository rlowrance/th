# e-cv-chart.R  
# main program
# Produce charts using input files e-cv_SCOPE_MODEL_TIMEPERIOD_SCENARIO_RESPONSE_PREDICTORSFORM_PREDICTORS_NAME_NDAYS_QUERY_C_NTREE_MTRY.RData
# output files have these names
# WORKING/e-cv-chart_SOMETHING.SUFFIX
# e-cv-chart-generated.makefile
#   describes all file dependencies for each chart
#
# Command line arguments:
# --makefile: FLAG, if present only create file e-cv-chart.makefile

source('Directory.R')
source('Libraries.R')

#source('CrossValidateCharts.R')
#source('CvApplyAllPossibilities.R')
source('CvCell.R')
source('Lines.R')
source('Predictors2.R')

library(boot)
library(ggplot2)
library(optparse)
library(memoise)

Control <- function(default.args) {
    # parse command line arguments in command.args
    opt <- ParseCommandArgs( command.args = commandArgs(trailingOnly = TRUE)
                            ,default.args
                            )

    me <- 'e-cv-chart' 

    log <- Directory('log')
    working <- Directory('working')
    cells <- paste0(working, 'e-cv-cells/')

    out.base <-
        sprintf('%s'
                ,me
                )
    in.base <- 
        sprintf('%s'
                ,'e-cv'
                )

    control <- list( path.in.base = paste0(working, in.base)
                    ,path.in.chart9.features = paste0(working, 'e-features-lcv2.txt')
                    ,path.in.submarkets = paste0(working, 'submarkets.RData')
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.makefile = paste0(me, '-generated.makefile')
                    #,path.out.chart.1 = paste0(working, out.base, '_chart1.txt')
                    #,path.out.chart.2 = paste0(working, out.base, '_chart2.txt')
                    #,path.out.chart.3 = paste0(working, out.base, '_chart3.txt')
                    #,path.out.chart.4 = paste0(working, out.base, '_chart4.txt')
                    ,path.out.chart.5 = paste0(working, out.base, '_chart5.txt')
                    ,path.out.chart.6 = paste0(working, out.base, '_chart6.txt')
                    ,path.out.chart.7 = paste0(working, out.base, '_chart7.txt')
                    ,path.out.chart.8 = paste0(working, out.base, '_chart8.txt')
                    ,path.out.chart.9.txt = paste0(working, out.base, '_chart9.txt')
                    ,path.out.chart.9.gg1 = paste0(working, out.base, '_chart9_1.pdf')
                    ,path.out.chart.9.gg2 = paste0(working, out.base, '_chart9_2.pdf')
                    ,path.out.chart.10.txt = paste0(working, out.base, '_chart10.txt')
                    ,path.out.chart.10.gg1 = paste0(working, out.base, '_chart10_1.pdf')
                    ,path.out.chart.10.gg2 = paste0(working, out.base, '_chart10_2.pdf')
                    ,path.out.chart.11.gg1 = paste0(working, out.base, '_chart11_1.pdf')
                    ,path.out.chart.11.gg2 = paste0(working, out.base, '_chart11_2.pdf')
                    ,path.out.chart.12.gg1 = paste0(working, out.base, '_chart12_1.pdf')
                    ,path.out.chart.12.gg2 = paste0(working, out.base, '_chart12_2.pdf')
                    ,path.out.chart.12.txt = paste0(working, out.base, '_chart12_3.txt')
                    ,path.out.chart.13.indicators.txt = paste0(working, out.base, '_chart13_indicators.txt')
                    ,path.out.chart.13.submarkets.summary.txt = 
                        paste0(working, out.base, '_chart13_submarkets_summary.txt')
                    ,path.out.chart.13.submarkets.census.txt = 
                        paste0(working, out.base, '_chart13_submarkets_census.txt')
                    ,path.out.chart.13.submarkets.property.city.txt = 
                        paste0(working, out.base, '_chart13_submarkets_property_city.stxt')
                    ,path.out.chart.13.submarkets.zip5.txt = 
                        paste0(working, out.base, '_chart13_submarkets_zip5.txt')
                    ,path.out.chart.14.txt = paste0(working, out.base, '_chart14.txt')
                    ,path.cells = cells
                    ,chart.width = 14  # inches
                    ,chart.height = 10 # inches
                    ,working = working
                    ,testing = FALSE
                    ,debug = FALSE
                    ,opt = opt
                    ,me = me
                    ,ndays.range = c(  '30',  '60',  '90', '120', '150', '180'
                                     ,'210', '240', '270', '300', '330', '360'
                                     )
                    )
    control
}
ParseCommandArgs <- function(command.args, default.args) {
    # return name list of values from the command args
    flag.makefile <- make_option( opt_str = c('--makefile')
                                 ,action = 'store_true'
                                 ,type = 'logical'
                                 ,default = default.args$makefile
                                 ,help = 'if present, only produce the makefile'
                                 )
    option.list <- list( flag.makefile
                        )
    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
    opt
}
MedianRMSE <- function(a.cv.result) {
    # return median of the rootMedianSquaredError values in the folds
    result <- median(RootMedianSquaredErrors(a.cv.result))
    result
}
RootMedianSquaredErrors <- function(a.cv.result) {
    # return vector of the root median squared values from each fold
    nfolds <- length(a.cv.result)
    stopifnot(nfolds == 10)
    rootMedianSquaredError.values <- 
        sapply(1 : nfolds,
               function(fold.index) {
                   evaluation <- a.cv.result[[fold.index]]
                   evaluation$rootMedianSquaredError
               }
               )
    rootMedianSquaredError.values
}
CIMedian <- function(values, removeNAs = FALSE, debug = FALSE) {
    # return 95% confidence interval for the median of the vector of values
    # ref: R Cookbook, p 184
    if (debug) browser()
    if (removeNAs)
        values <- values[!is.na(values)]
    if (length(values) == 1) {
        # sample(values) ==> a very long vector, because sample.int is called
        # hence handle specially
        result <- list( lowest = values
                       ,highest = values
                       )
        return(result)
    }
    # length(values) > 1
    num.resamples <- 100  # TODO: set to 10,000
    num.resamples <- 10000
    sample.medians <- numeric(length = num.resamples)  # preallocate for speed
    lapply( 1:num.resamples
           ,function(index) sample.medians[[index]] <<- median(sample(values, replace = TRUE))
           )
    confidence.interval <- quantile( x = sample.medians
                                    ,probs = c(0.025, 0.975)
                                    )
    result <- list( lowest = confidence.interval[[1]]
                   ,highest = confidence.interval[[2]]
                   )
    result
}
SeMedianRMSE <- function(a.cv.result) {
    # return standard error of the rootMedianSquaredError values in the folds
    # ref: Wikipedia
    # se := standard deviation of some sampling statistics
    browser()
    nfolds <- length(a.cv.result)
    stopifnot(nfolds == 10)
    rootMedianSquaredError.values <- 
        sapply(1 : nfolds,
               function(fold.index) {
                   evaluation <- a.cv.result[[fold.index]]
                   evaluation$rootMedianSquaredError
               }
               )
    standard.error <- sd(rootMedianSquaredError.values)  # NOTE: uses denominator n - 1

    # now use bootstap
    Median <- function(values, indices) {
        browser()
        median(values[indices])
    }
    booted <- boot( data = rootMedianSquaredError.values
                   ,statistic = Median
                   ,R = 100  # TODO: use 10000
                   )
    standard.error
}
MeanWithin10 <- function(a.cv.result) {
    # return mean of the fraction.within.10.percent values in the folds
    nfolds <- length(a.cv.result)
    stopifnot(nfolds == 10)
    fraction.within.10.percent.values <- 
        sapply(1 : nfolds,
               function(fold.index) {
                   evaluation <- a.cv.result[[fold.index]]
                   evaluation$fraction.within.10.percent
               }
                               )
    result <- mean(fraction.within.10.percent.values)
    result
}
Chart.1.2 <- function(my.control) {
    # class object
    # methods
    # $FileDependencies()
    # $Txt()

    # these format lines must be edited as a group (15 columns)
    # line fields: scenario / response / predictorsForm / 12 x ndays
    case   <- '%8s %8s %5s'
    header.format <- paste0(case, paste0(rep(' %6s', 12), collapse = ''))
    data.format   <- paste0(case, paste0(rep(' %6.0f', 12), collapse = ''))

    ndays.range <- c('30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360')

    PathIn <- function(scenario, response, predictorsName, predictorsForm, ndays) {
        path.in <- paste0( my.control$path.in.base
                          ,'_global'
                          ,'_linear'
                          ,'_2009'
                          ,'_', scenario
                          ,'_', response
                          ,'_', predictorsName
                          ,'_', predictorsForm
                          ,'_', ndays
                          ,'_1'
                          ,'_0'
                          ,'_0'
                          ,'_0'
                          ,'.RData'
                          )
        path.in
    }

    Accumulate <- function(f) {
        # accumulate lines across relevant combination of scenario,response, predictorsForm, predictorsName
        accumulator <- Lines()
        for (scenario in c('avm', 'mortgage')) {
            for (response in c('price', 'logprice')) {
                for (predictorsForm in c('level', 'log')) {
                    f( scenario = scenario
                      ,response = response
                      ,predictorsForm = predictorsForm
                      ,accumulator
                      )
                }
            }
        }
        result <- accumulator$Get()
        result
    }

    FileDependencies <- function(predictorsName) {
        FileDependency <- function(scenario, response, predictorsForm, accumulator) {
            #cat('FileDependency', scenario, response, predictorsForm, '\n'); browser()
            for (ndays in ndays.range) {
                line <- PathIn( scenario = scenario
                               ,response = response
                               ,predictorsName = predictorsName
                               ,predictorsForm = predictorsForm
                               ,ndays = ndays
                               )
                accumulator$Append(line)
            }
        }

        result <- Accumulate(FileDependency)
        result
    }

    DataRecords <- function(predictorsName) {
        DataRecord <- function(scenario, response, predictorsForm, accumulator) {
            M_RMSE <- function(ndays) {
                path.in <- PathIn( scenario = scenario
                                  ,response = response
                                  ,predictorsName = predictorsName
                                  ,predictorsForm = predictorsForm
                                  ,ndays = ndays
                                  )
                load(path.in)
                stopifnot(!is.null(cv.result))
                stopifnot(length(cv.result) == 1)
                median.RMSE <- MedianRMSE(cv.result[[1]])
                median.RMSE
            }
            line <- sprintf( data.format
                            ,scenario
                            ,response
                            ,predictorsForm
                            ,M_RMSE(30)
                            ,M_RMSE(60)
                            ,M_RMSE(90)
                            ,M_RMSE(120)
                            ,M_RMSE(150)
                            ,M_RMSE(180)
                            ,M_RMSE(210)
                            ,M_RMSE(240)
                            ,M_RMSE(270)
                            ,M_RMSE(300)
                            ,M_RMSE(330)
                            ,M_RMSE(360)
                            )
            accumulator$Append(line)
        }

        result <- Accumulate(DataRecord)
        result
    }

    HeaderRecords <- function(predictorsName) {
        lines <- Lines()
        lines$Append('Median of Root Median Squared Errors from 10 Fold Cross Validation')
        lines$Append('For global linear model')
        lines$Append('Data from late 2008 and 2009')
        lines$Append('Features Present in Every Transaction')
        lines$Append(switch( predictorsName
                            ,always = 'Using Assessments'
                            ,alwaysNoAssessment = 'Not Using Assessment'
                            ,stop('bad predictorsName')
                            )
        )
        lines$Append(' ')
        lines$Append(sprintf( header.format
                             ,' '
                             ,' '
                             ,'preds'  # abbreviate 'predictors' to fit into 6 columns
                             ,'ndays'
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             )
        )
        lines$Append(sprintf( header.format
                             ,'scenario'
                             ,'response'
                             ,'Form'
                             ,'30'
                             ,'60'
                             ,'90'
                             ,'120'
                             ,'150'
                             ,'180'
                             ,'210'
                             ,'240'
                             ,'270'
                             ,'300'
                             ,'330'
                             ,'360'
                             )
        )
        result <- lines$Get()
        result
    }

    Txt <- function(predictorsName) {
        lines <- Lines()

        for (line in HeaderRecords(predictorsName)) {
            lines$Append(line)
        }

        for (line in DataRecords(predictorsName)) {
            lines$Append(line)
        }

        result <- lines$Get()
        result
    }

    list( Txt = Txt
         ,FileDependencies = FileDependencies
         )
}
Chart.3 <- function(my.control) {
    # class object
    # methods
    # $FileDependencies()
    # $Txt()

    # these format lines must be edited as a group (15 columns)
    # line fields: scenario / response / predictorsForm / predictorsName / 12 x ndays

    # MAYBE: cut off after ndays that are no longer informative
    case   <- '%8s %8s %5s %3s'
    header.format <- paste0(case, paste0(rep(' %6s', 12), collapse = ''))
    data.format   <- paste0(case, paste0(rep(' %6.0f', 12), collapse = ''))

    ndays.range <- c('30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360')

    PathIn <- function(scenario, response, predictorsName, predictorsForm, ndays) {
        path.in <- paste0( my.control$path.in.base
                          ,'_global'
                          ,'_linear'
                          ,'_2008'
                          ,'_', scenario
                          ,'_', response
                          ,'_', predictorsName
                          ,'_', predictorsForm
                          ,'_', ndays
                          ,'_1'
                          ,'_0'
                          ,'_0'
                          ,'_0'
                          ,'.RData'
                          )
        path.in
    }

    Accumulate <- function(f) {
        # accumulate lines across relevant combination of scenario,response, predictorsForm, predictorsName
        accumulator <- Lines()
        for (scenario in c('avm', 'mortgage')) {
            for (response in c('price', 'logprice')) {
                for (predictorsForm in c('level', 'log')) {
                    for (predictorsName in c('always', 'alwaysNoAssessment')) {
                        f( scenario = scenario
                          ,response = response
                          ,predictorsName = predictorsName
                          ,predictorsForm = predictorsForm
                          ,accumulator
                          )
                    }
                }
            }
        }
        result <- accumulator$Get()
        result
    }

    FileDependencies <- function() {
        FileDependency <- function(scenario, response, predictorsName, predictorsForm, accumulator) {
            #cat('FileDependency', scenario, response, predictorsName, predictorsForm, '\n'); browser()
            for (ndays in ndays.range) {
                line <- PathIn( scenario = scenario
                               ,response = response
                               ,predictorsName = predictorsName
                               ,predictorsForm = predictorsForm
                               ,ndays = ndays
                               )
                accumulator$Append(line)
            }
        }

        result <- Accumulate(FileDependency)
        result
    }

    DataRecords <- function() {
        DataRecord <- function(scenario, response, predictorsName, predictorsForm, accumulator) {
            #cat('DataRecord', scenario, response, predictorsName, predictorsForm, '\n'); browser()
            M_RMSE <- function(ndays) {
                path.in <- PathIn( scenario = scenario
                                  ,response = response
                                  ,predictorsName = predictorsName
                                  ,predictorsForm = predictorsForm
                                  ,ndays = ndays
                                  )
                load(path.in)
                stopifnot(!is.null(cv.result))
                stopifnot(length(cv.result) == 1)
                median.RMSE <- MedianRMSE(cv.result[[1]])
                median.RMSE

            }
            line <- sprintf( data.format
                            ,scenario
                            ,response
                            ,predictorsForm
                            ,switch(predictorsName
                                    ,always = 'yes'
                                    ,alwaysNoAssessment = 'no'
                                    ,stop('bad predictorsName')
                                    )
                            ,M_RMSE(30)
                            ,M_RMSE(60)
                            ,M_RMSE(90)
                            ,M_RMSE(120)
                            ,M_RMSE(150)
                            ,M_RMSE(180)
                            ,M_RMSE(210)
                            ,M_RMSE(240)
                            ,M_RMSE(270)
                            ,M_RMSE(300)
                            ,M_RMSE(330)
                            ,M_RMSE(360)
                            )
            accumulator$Append(line)
        }

        result <- Accumulate(DataRecord)
        result
    }


    HeaderRecords <- function() {
        lines <- Lines()
        lines$Append('Median of Root Median Squared Errors from 10 Fold Cross Validation')
        lines$Append('For global linear model')
        lines$Append('Data from 2008')
        lines$Append('Features Present in Every Transaction')
        lines$Append('With and Without Tax Assessment Features (Column Use Tax)')
        lines$Append(' ')
        lines$Append(sprintf( header.format
                             ,' '
                             ,' '
                             ,'preds'  # abbreviate 'predictors' to fit into 6 columns
                             ,'Use'
                             ,'ndays'
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             )
        )
        lines$Append(sprintf( header.format
                             ,'scenario'
                             ,'response'
                             ,'Form'
                             ,'Tax'
                             ,'30'
                             ,'60'
                             ,'90'
                             ,'120'
                             ,'150'
                             ,'180'
                             ,'210'
                             ,'240'
                             ,'270'
                             ,'300'
                             ,'330'
                             ,'360'
                             )
        )
        result <- lines$Get()
        result
    }

    Txt <- function() {
        lines <- Lines()

        for (line in HeaderRecords()) {
            lines$Append(line)
        }

        for (line in DataRecords()) {
            lines$Append(line)
        }

        result <- lines$Get()
        result
    }

    list( Txt = Txt
         ,FileDependencies = FileDependencies
         )
}
Chart.4 <- function(my.control) {
    # class object
    # methods
    # $FileDependencies()
    # $Txt()

    # these format lines must be edited as a group (15 columns)
    # line fields: scenario / response / predictorsForm / predictorsName / 12 x ndays

    # MAYBE: cut off after ndays that are no longer informative
    case   <- '%8s %8s %5s %3s'
    header.format <- paste0(case, paste0(rep(' %6s', 12), collapse = ''))
    data.format   <- paste0(case, paste0(rep(' %6.0f', 12), collapse = ''))

    ndays.range <- c('30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360')

    PathIn <- function(scenario, response, predictorsName, predictorsForm, ndays) {
        path.in <- paste0( my.control$path.in.base
                          ,'_global'
                          ,'_linear'
                          ,'_2008'
                          ,'_', scenario
                          ,'_', response
                          ,'_', predictorsName
                          ,'_', predictorsForm
                          ,'_', ndays
                          ,'_1'
                          ,'_0'
                          ,'_0'
                          ,'_0'
                          ,'.RData'
                          )
        path.in
    }

    Accumulate <- function(f) {
        # accumulate lines across relevant combination of scenario,response, predictorsForm, predictorsName
        accumulator <- Lines()
        for (scenario in c('avm', 'mortgage')) {
            for (response in c('price', 'logprice')) {
                for (predictorsForm in c('level', 'log')) {
                    for (predictorsName in c('alwaysNoAssessment', 'alwaysNoCensus')) {
                        f( scenario = scenario
                          ,response = response
                          ,predictorsName = predictorsName
                          ,predictorsForm = predictorsForm
                          ,accumulator
                          )
                    }
                }
            }
        }
        result <- accumulator$Get()
        result
    }

    FileDependencies <- function() {
        FileDependency <- function(scenario, response, predictorsName, predictorsForm, accumulator) {
            cat('FileDependency', scenario, response, predictorsName, predictorsForm, '\n'); browser()
            for (ndays in ndays.range) {
                line <- PathIn( scenario = scenario
                               ,response = response
                               ,predictorsName = predictorsName
                               ,predictorsForm = predictorsForm
                               ,ndays = ndays
                               )
                accumulator$Append(line)
            }
        }

        result <- Accumulate(FileDependency)
        result
    }

    DataRecords <- function() {
        DataRecord <- function(scenario, response, predictorsName, predictorsForm, accumulator) {
            #cat('DataRecord', scenario, response, predictorsName, predictorsForm, '\n'); browser()
            M_RMSE <- function(ndays) {
                path.in <- PathIn( scenario = scenario
                                  ,response = response
                                  ,predictorsName = predictorsName
                                  ,predictorsForm = predictorsForm
                                  ,ndays = ndays
                                  )
                load(path.in)
                stopifnot(!is.null(cv.result))
                stopifnot(length(cv.result) == 1)
                median.RMSE <- MedianRMSE(cv.result[[1]])
                median.RMSE

            }
            line <- sprintf( data.format
                            ,scenario
                            ,response
                            ,predictorsForm
                            ,switch(predictorsName
                                    ,alwaysNoAssessment = 'yes'
                                    ,alwaysNoCensus     = 'no'
                                    ,stop('bad predictorsName')
                                    )
                            ,M_RMSE(30)
                            ,M_RMSE(60)
                            ,M_RMSE(90)
                            ,M_RMSE(120)
                            ,M_RMSE(150)
                            ,M_RMSE(180)
                            ,M_RMSE(210)
                            ,M_RMSE(240)
                            ,M_RMSE(270)
                            ,M_RMSE(300)
                            ,M_RMSE(330)
                            ,M_RMSE(360)
                            )
            accumulator$Append(line)
        }

        result <- Accumulate(DataRecord)
        result
    }


    HeaderRecords <- function() {
        lines <- Lines()
        lines$Append('Median of Root Median Squared Errors from 10 Fold Cross Validation')
        lines$Append('For global linear model')
        lines$Append('Data from 2003 Onward')
        lines$Append('Non Tax Assessment Features Present in Every Transaction')
        lines$Append('With and Without Census Tract Features (Column Use Cen)')
        lines$Append(' ')
        lines$Append(sprintf( header.format
                             ,' '
                             ,' '
                             ,'preds'  # abbreviate 'predictors' to fit into 6 columns
                             ,'Use'
                             ,'ndays'
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             )
        )
        lines$Append(sprintf( header.format
                             ,'scenario'
                             ,'response'
                             ,'Form'
                             ,'Cen'
                             ,'30'
                             ,'60'
                             ,'90'
                             ,'120'
                             ,'150'
                             ,'180'
                             ,'210'
                             ,'240'
                             ,'270'
                             ,'300'
                             ,'330'
                             ,'360'
                             )
        )
        result <- lines$Get()
        result
    }

    Txt <- function() {
        lines <- Lines()

        for (line in HeaderRecords()) {
            lines$Append(line)
        }

        for (line in DataRecords()) {
            lines$Append(line)
        }

        result <- lines$Get()
        result
    }

    list( Txt = Txt
         ,FileDependencies = FileDependencies
         )
}
Chart.5.Fixed.Cell.Values <- function() {
    list( scope = 'global'
         ,model = 'linear'
         ,timePeriod = '2008'
         ,scenario = 'avm'
         ,query = '1'
         ,lambda = '0'
         ,ntree = '0'
         ,mtry = '0'
         )
}
Chart.5.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 5 
    # predictorsName in {always, alwaysNoAssesment}
    # timePeriod 2008

    fixed <- Chart.5.Fixed.Cell.Values()
    result <- NULL
    for (response in c('price', 'logprice')) {
        for (predictorsName in c('always', 'alwaysNoAssessment')) {
            for (predictorsForm in c('level', 'log')) {
                for (ndays in CvCell()$Possible.Ndays()) {
                    element <- list( scope = fixed$scope
                                    ,model = fixed$model
                                    ,timePeriod = fixed$timePeriod
                                    ,scenario = fixed$scenario
                                    ,response = response
                                    ,predictorsName = predictorsName
                                    ,predictorsForm = predictorsForm
                                    ,ndays = ndays
                                    ,query = fixed$query
                                    ,lambda = fixed$lambda
                                    ,ntree = fixed$ntree
                                    ,mtry = fixed$mtry
                                    )
                    result[[length(result) + 1]] <- element
                }
            }
        }
    }
    result
}
Chart.6.Fixed.Cell.Values <- function() {
    # return list of selectors for cells that are fixed in chart 5
    list( scope = 'global'
         ,model = 'linear'
         ,timePeriod = '2003on'
         ,scenario = 'avm'
         ,query = '100'
         ,lambda = '0'
         ,ntree = '0'
         ,mtry = '0'
         )
}
Chart.6.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 6
    # predictorsName in {alwaysNoAssesment, alwaysNoCensus}
    # timePeriod 2003on

    fixed <- Chart.6.Fixed.Cell.Values()
    result <- NULL
    for (response in c('price', 'logprice')) {
        for (predictorsName in c('alwaysNoAssessment', 'alwaysNoCensus')) {
            for (predictorsForm in c('level', 'log')) {
                for (ndays in CvCell()$Possible.Ndays()) {
                    element <- list( scope = fixed$scope
                                    ,model = fixed$model
                                    ,timePeriod = fixed$timePeriod
                                    ,scenario = fixed$scenario
                                    ,response = response
                                    ,predictorsName = predictorsName
                                    ,predictorsForm = predictorsForm
                                    ,ndays = ndays
                                    ,query = fixed$query
                                    ,lambda = fixed$lambda
                                    ,ntree = fixed$ntree
                                    ,mtry = fixed$mtry
                                    )
                    result[[length(result) + 1]] <- element
                }
            }
        }
    }
    result
}
Chart.7.Fixed.Cell.Values <- function() {
    list( scope = 'global'
         ,model = 'linear'
         ,scenario = 'avm'
         ,timePeriod = '2003on'
         ,predictorsName = 'alwaysNoAssessment'
         ,query = '100'
         ,lambda = '0'
         ,ntree = '0'
         ,mtry = '0'
         )
}
Chart.7.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 7 
    fixed <- Chart.7.Fixed.Cell.Values()

    result <- NULL
    for (response in c('price', 'logprice')) {
        for (predictorsName in 'alwaysNoAssessment') {
            for (predictorsForm in c('level', 'log')) {
                for (ndays in CvCell()$Possible.Ndays()) {
                    element <- list( scope = fixed$scope
                                    ,model = fixed$model
                                    ,timePeriod = fixed$timePeriod
                                    ,scenario = fixed$scenario
                                    ,response = response
                                    ,predictorsName = predictorsName
                                    ,predictorsForm = predictorsForm
                                    ,ndays = ndays
                                    ,query = fixed$query
                                    ,lambda = fixed$lambda
                                    ,ntree = fixed$ntree
                                    ,mtry = fixed$mtry
                                    )
                    result[[length(result) + 1]] <- element
                }
            }
        }
    }
    result
}
Chart.8.Fixed.Cell.Values <- function() {
    Chart.7.Fixed.Cell.Values()
}
Chart.8.Fixed.Cell.Dependencies <- function() {
    Chart.7.Fixed.Cell.Dependences()
}
Chart.8.FileDependencies <- function(my.control) {
    result <- Chart.7.FileDependencies(my.control)
    result
}
Chart.9.PredictorsNames <- function(control) {
    # return list of predictor names for chart 9
    # these have the form best01, best02, ..., best24
    ordered.features <- readLines(con = control$path.in.chart9.features)
    cvcell.predictors.names <- CvCell()$Possible.PredictorsNames()
    stopifnot(length(ordered.features) != length(cvcell.predictors.names))
    predictorsNames <- sapply( 1:length(ordered.features)
                             ,function(n) 
                                 sprintf('best%02d', n)
                             )
    # verify that each of the predictors was expected
    lapply( predictorsNames
           ,function(predictorsName) is.element(predictorsName, cvcell.predictors.names)
           )
    predictorsNames
}
Chart.9.Fixed.Cell.Values <- function() {
    list( scope = 'global'
         ,model = 'linear'
         ,timePeriod = '2003on'
         ,scenario = 'avm'
         ,response = 'logprice'
         ,predictorsForm = 'level'
         ,ndays = '60'
         ,query = '100'
         ,lambda = '0'
         ,ntree = '0'
         ,mtry = '0'
         )
}
Chart.9.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 9
    # this is just for the best form and ndays
    # best for is log-level
    # best ndays is 60 
    fixed <- Chart.9.Fixed.Cell.Values()

    result <- NULL
    for (predictorsName in Chart.9.PredictorsNames(my.control)) {
        element <- list( scope = fixed$scope
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
        result[[length(result) + 1]] <- element
    }
    result
}
Chart.10.PredictorsNames <- function() {
    # return vector of predictor names used in chart 10
    result <- sprintf('pca%02d', 1:4)
    result
}
Chart.10.Fixed.Cell.Values <- function() {
    Chart.9.Fixed.Cell.Values()
}
Chart.10.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 10
    fixed <- Chart.10.Fixed.Cell.Values()

    result <- NULL
    for (predictorsName in Chart.10.PredictorsNames()) {
        element <- list( scope = fixed$scope
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
        result[[length(result) + 1]] <- element
    }
    result
}
Chart.11.PredictorsNames <- function() {
    # return vector of predictor names used in chart 10
    result <- c( 'best08'
                ,'best19'
                ,'best20'
                ,'best24'
                ,'pca01'
                ,'pca02'
                ,'pca03'
                ,'pca04'
                )
    result
}
Chart.11.Fixed.Cell.Values <- function() {
    Chart.9.Fixed.Cell.Values()
}
Chart.11.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 11
    fixed <- Chart.11.Fixed.Cell.Values()

    result <- NULL
    for (predictorsName in Chart.11.PredictorsNames()) {
        element <- list( scope = fixed$scope
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
        result[[length(result) + 1]] <- element
    }
    result
}
Chart.12.Lambda.Values <- function() {
    # return vector of lambda values that are used for regularization
    # lambda on command lines is 100 * lambda in regression
    lambda.in.regression <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30)
    lambda.on.command.line <- as.character(100 * lambda.in.regression)
    lambda.on.command.line
}
Chart.12.Fixed.Cell.Values <- function() {
    list( scope = 'global'
         ,model = 'linL2'
         ,timePeriod = '2003on'
         ,scenario = 'avm'
         ,response = 'logprice'
         ,predictorsName = 'best20'
         ,predictorsForm = 'level'
         ,ndays = '60'
         ,query = '100'
         ,ntree = '0'
         ,mtry = '0'
         )
}
Chart.12.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 11
    fixed <- Chart.12.Fixed.Cell.Values()

    result <- NULL
    for (lambda in Chart.12.Lambda.Values()) {
        element <- list( scope = fixed$scope
                        ,model = fixed$model
                        ,scenario = fixed$scenario
                        ,timePeriod = fixed$timePeriod
                        ,response = fixed$response
                        ,predictorsName = fixed$predictorsName
                        ,predictorsForm = fixed$predictorsForm
                        ,ndays = fixed$ndays
                        ,query = fixed$query
                        ,lambda = lambda
                        ,ntree = fixed$ntree
                        ,mtry = fixed$mtry
                        )
        result[[length(result) + 1]] <- element
    }
    result
}
Chart.13.Fixed.Cell.Values <- function() {
    list( model = 'linL2'
         ,timePeriod = '2003on'
         ,scenario = 'avm'
         ,response = 'logprice'
         ,predictorsForm = 'level'
         ,query = '100'
         ,ndays = '60'
         ,lambda = '400'
         ,ntree = '0'
         ,mtry = '0'
         )
}
Chart.13.Parameters <- function(control) {
    # return list of all combinations
    fixed <- Chart.13.Fixed.Cell.Values()

    result <- NULL
    GenerateIndicatorElements <- function() {
        for (predictorsName in c( 'best20zip'
                                 ,'best20census'
                                 ,'best20city'
                                 )) {
            element <-list( scope = 'global'
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
            result[[length(result) + 1]] <<- element
        }
    }
    GenerateIndicatorElements()

    GenerateSubmarketElements <- function() {
        loaded <- load(file = control$path.in.submarkets)
        for (scope in c(codes.census.tract, codes.property.city, codes.zip5)) {
            element <-list( scope = scope
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
            result[[length(result) + 1]] <<- element
        }
    }
    GenerateSubmarketElements()
    result
}
Chart.13.FileDependencies <- function(my.control) {
    result <- Chart.13.Parameters(my.control)
    result
}
Chart.14.Fixed.Cell.Values <- function() {
    list( scope = 'global'
         ,model = 'rf'
         ,scenario = 'avm'
         ,timePeriod = '2003on'
         ,response = 'logprice'
         ,predictorsForm = 'level'
         ,ndays = '60'
         ,query = '100'
         ,lambda = '0'
         )
}
Chart.14.Parameters <- function(control) {
    # return list of all combinations
    fixed <- Chart.14.Fixed.Cell.Values()

    result <- NULL
    # generate in order so that the longest to run are specified first
    # that's because the makefile runs the jobs in parallel, first to last
    for (predictorsName in c('always', 'best20')) {
        for (ntree in c('1000', '300', '100', '1')) {
            for (mtry in c('4', '3', '2', '1')) {
                element <-list( scope = fixed$scope
                               ,model = fixed$model
                               ,scenario = fixed$scenario
                               ,timePeriod = fixed$timePeriod
                               ,response = fixed$response
                               ,predictorsName = predictorsName
                               ,predictorsForm = fixed$predictorsForm
                               ,ndays = fixed$ndays
                               ,query = fixed$query
                               ,lambda = fixed$lambda
                               ,ntree = ntree
                               ,mtry = mtry
                               )
                result[[length(result) + 1]] <- element
            }
        }
    }
    result
}
Chart.14.FileDependencies <- function(my.control) {
    result <- Chart.14.Parameters(my.control)
    result
}
Table.5.6 <- function() {
    # return table function object $Header1() $Header2() $Detail() $Formated() $Blank() $Get()
    case   <- '%8s %5s %3s'
    header.format <- paste0(case, paste0(rep(' %6s', 12), collapse = ''))
    data.format   <- paste0(case, paste0(rep(' %6.0f', 12), collapse = ''))

    lines <- Lines()
    Header1 <- function(col2, col3, col4) {
        # append a header record with mostly blank columns
        line <- sprintf( header.format 
                        ,' '
                        ,col2, col3, col4   
                        ,' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' 
                        )
        lines$Append(line)
    }

    Header2 <- function(col1, col2, col3, col4, col5, col6, col7, col8,
                        col9, col10, col11, col12, col13, col14, col15) {
        line <- sprintf(header.format,
                         col1, col2, col3, col4, col5, col6, col7, col8,
                         col9, col10, col11, col12, col13, col14, col15
                         )
        lines$Append(line)
    }
    
    Blank <- function() {
        lines$Append(' ')
    }

    Detail <- function(col1, col2, col3, col4, col5, col6, col7, col8,
                       col9, col10, col11, col12, col13, col14, col15) {
        line <- sprintf(data.format,
                        col1, col2, col3, col4, col5, col6, col7, col8,
                        col9, col10, col11, col12, col13, col14, col15
                        )
        lines$Append(line)
    }

    Formatted <- function(additional.lines) {
        # append already-formatted lines (usually in the header)
        for (line in additional.lines) {
            lines$Append(line)
        }
    }

    Get <- function() {
       lines$Get()
    }

    list( Header1   = Header1
         ,Header2   = Header2
         ,Blank     = Blank
         ,Detail    = Detail
         ,Formatted = Formatted
         ,Get       = Get
         )
}
Header.Scope <- function(s) {
    paste0( 'Scope: '
           ,switch( s
                   ,global = 'entire market'
                   ,stop('bad scope')
                   )
           )
}
Header.Model <- function(s) {
    paste0( 'Model: '
           ,switch( s
                   ,linear = 'linear'
                   ,linL2  = 'linear with L2 regularizer'
                   ,rf     = 'random forest'
                   ,stop('bad model')
                   )
           )
}
Header.Ndays <- function(s) {
    paste0( 'Number of days in training period: ', s)
}
Header.PredictorsForm <- function(s) {
    paste0( 'Predictors form: '
           ,switch(s
                   ,level = 'level (natural units)'
                   ,log   = 'log(natural units)'
                   ,stop('bad predictorsForm')
                   )
           )
}
Header.PredictorsName <- function(s) {
    paste0( 'Predictors name: '
           ,switch(s
                   ,always = 'always in every transaction'
                   ,alwaysNoAssessment = 'always in every transaction and not in the tax assessment'
                   ,alwaysNoCensus = 'always in every transaction and not in the tax assessment or census'
                   ,stop('bad predictorsName')
                   )
           )
}
Header.Response <- function(s) {
    paste0( 'Response: '
           ,switch( s
                   ,price = 'price'
                   ,logprice = 'log(price)'
                   ,stop('bad response')
                   )
           )
}
Header.TimePeriod <- function(s) {
    paste0( 'Time period: '
           ,switch( s
                   ,'2003on' = '2003 on'
                   ,'2008'   = '2008'
                   ,stop('bad time period')
                   )
           )
}
Header.Query <- function(s) {
    paste0( 'Percent of queries in each fold that were estimated: '
           ,switch( s
                   ,'100' = '1'
                   ,'1'   = '100'
                   ,stop('bad query')
                   )
           )
}
Header.Lambda <- function(s) {
    if (s == '0')
        NULL
    else
        sprintf('Lambda: %.2f', as.numeric(s) / 100)
}
    
Headers.Fixed <- function(fixed, lines) {
    # append fixed header to lines
    stopifnot(!is.null(lines))
    lines$Append(' ')
    for (header.name in names(fixed)) {

        if (header.name == 'scenario') stopifnot(fixed$scenario == 'avm')
        else if (header.name == 'ntree') stopifnot(fixed$ntree == '0')
        else if (header.name == 'mtry') stopifnot(fixed$mtry == '0')
        else {
            value <- switch( header.name
                            ,scope = Header.Scope(fixed$scope)
                            ,model = Header.Model(fixed$model)
                            ,timePeriod = Header.TimePeriod(fixed$timePeriod)
                            ,response = Header.Response(fixed$response)
                            ,predictorsForm = Header.PredictorsForm(fixed$predictorsForm)
                            ,predictorsName = Header.PredictorsName(fixed$predictorsName)
                            ,ndays = Header.Ndays(fixed$ndays)
                            ,query = Header.Query(fixed$query)
                            ,lambda = Header.Lambda(fixed$lambda)
                            ,stop(paste('bad header.name', header.name))
                            )
            if (!is.null(value))
                lines$Append(value)
        }
    }
}
Chart.5 <- function(my.control) {
    # return list of lines, the txt table for chart 5

    fixed <- Chart.5.Fixed.Cell.Values()
    Path <- CvCell()$Path

    Header <- function() {
        lines <- Lines()
        lines$Append('Median of Root Median Squared Errors from 10 Fold Cross Validation')

        stopifnot(fixed$scope == 'global')
        stopifnot(fixed$model == 'linear')
        stopifnot(fixed$timePeriod == '2008')
        stopifnot(fixed$query == '1')

        Headers.Fixed(fixed, lines)

        lines$Append(' ')
        lines$Append('Column Use Tax (yes ==> use tax assessment)')
        result <- lines$Get()
        result
    }

    table <- Table.5.6()
    table$Formatted(Header())
    table$Blank()
    table$Header1('preds', 'Use', 'ndays')
    table$Header2( 'response', 'Form', 'Tax'
                  ,'30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360'
                  )

    DetailLine <- function(response, predictorsName, predictorsForm) {
        M <- function(ndays) {
            path.in <- Path( scope = fixed$scope
                            ,model = fixed$model
                            ,timePeriod = fixed$timePeriod
                            ,scenario = fixed$scenario
                            ,response = response
                            ,predictorsName = predictorsName
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
            median.RMSE <- MedianRMSE(cv.result[[1]])
            median.RMSE
        }

        use.assessment <- switch( predictorsName
                                 ,always = 'yes'
                                 ,alwaysNoAssessment = 'no'
                                 )
        table$Detail( response, predictorsForm, use.assessment
                     ,M(30), M(60), M(90), M(120), M(150), M(180)
                     ,M(210), M(240), M(270), M(300), M(330), M(360)
                     )
    }

    for (response in c('price', 'logprice')) {
        for (predictorsForm in c('level', 'log')) {
            for (predictorsName in c('always', 'alwaysNoAssessment')) {
                DetailLine( response = response
                           ,predictorsName = predictorsName
                           ,predictorsForm = predictorsForm
                           )
            }
        }
    }

    result <- table$Get()
    result
}
Chart.6 <- function(my.control) {
    # return txt lines for chart 6
    fixed <- Chart.6.Fixed.Cell.Values()
    Path <- CvCell()$Path

    Header <- function() {
        lines <- Lines()
        lines$Append('Median of Root Median Squared Errors from 10 Fold Cross Validation')

        stopifnot(fixed$scope == 'global')
        stopifnot(fixed$model == 'linear')
        stopifnot(fixed$timePeriod == '2003on')
        stopifnot(fixed$query == '100')

        Headers.Fixed(fixed, lines)

        lines$Append(' ')
        lines$Append('Column Use Cen (yes ==> use census data)')
        result <- lines$Get()
        result
    }

    table <- Table.5.6()
    table$Formatted(Header())
    table$Blank()
    table$Header1('preds', 'Use', 'ndays')
    table$Header2( 'response', 'Form', 'Cen'
                  ,'30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360'
                  )

    DetailLine <- function(response, predictorsName, predictorsForm) {
        M <- function(ndays) {
            path.in <- Path( scope = fixed$scope
                            ,model = fixed$model
                            ,timePeriod = fixed$timePeriod
                            ,scenario = fixed$scenario
                            ,response = response
                            ,predictorsName = predictorsName
                            ,predictorsForm = predictorsForm
                            ,nday = ndays
                            ,query = fixed$query
                            ,lambda = fixed$lambda
                            ,ntree = fixed$ntree
                            ,mtry = fixed$mtry
                            )
            load(path.in)
            stopifnot(!is.null(cv.result))
            stopifnot(length(cv.result) == 1)
            median.RMSE <- MedianRMSE(cv.result[[1]])
            median.RMSE
        }

        use.assessment <- switch( predictorsName
                                 ,alwaysNoAssessment = 'yes'
                                 ,alwaysNoCensus = 'no'
                                 ,stop('bad predictorsName')
                                 )
        table$Detail( response, predictorsForm, use.assessment
                     ,M(30), M(60), M(90), M(120), M(150), M(180)
                     ,M(210), M(240), M(270), M(300), M(330), M(360)
                     )
    }

    for (response in c('price', 'logprice')) {
        for (predictorsForm in c('level', 'log')) {
            for (predictorsName in c('alwaysNoAssessment', 'alwaysNoCensus')) {
                DetailLine( response = response
                           ,predictorsName = predictorsName
                           ,predictorsForm = predictorsForm
                           )
            }
        }
    }

    result <- table$Get()
    result
}
Table.7.A <- function(lines) {
    # return table function object $Header1() $Header2() $Detail() $Formated() $Blank() $Get()
    # each of which appends to Lines object line
    case   <- '%8s %8s %8s'
    header.format             <- paste0(case, paste0(rep(' %6s', 12), collapse = ''))
    data.format.whole.numbers <- paste0(case, paste0(rep(' %6.0f', 12), collapse = ''))
    data.format.fractions     <- paste0(case, paste0(rep(' %6.3f', 12), collapse = ''))

    Header1 <- function(predictorsForm, ndays30) {
        # append a header record with mostly blank columns
        Header2( response = ' '
                ,predictorsForm = predictorsForm
                ,metric = ' '
                ,ndays30 = ndays30
                ,ndays60 = ' '
                ,ndays90 = ' '
                ,ndays120 = ' '
                ,ndays150 = ' '
                ,ndays180 =  ' '
                ,ndays210 = ' '
                ,ndays240 = ' '
                ,ndays270 =  ' '
                ,ndays300 = ' '
                ,ndays330 =  ' '
                ,ndays360 = ' '
                )
    }

    Header2 <- function( response, predictorsForm, metric
                        ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                        ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                        ) {
        line <- sprintf( header.format
                        ,response, predictorsForm, metric
                        ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                        ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                        )
        lines$Append(line)
    }
    
    Blank <- function() {
        lines$Append(' ')
    }

    DetailEither <- function( response, predictorsForm, metricName
                             ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                             ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                             ,data.format
                             ) {
        line <- sprintf( data.format
                        ,response, predictorsForm, metricName
                        ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                        ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                        )
        lines$Append(line)
    }

    DetailWholeNumbers <- function( response, predictorsForm, metricName
                       ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                       ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                       ) {
        #cat('DetailWholeNumber\n'); browser()
        DetailEither( response, predictorsForm, metricName
                     ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                     ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                     ,data.format.whole.numbers
                     )
    }

    DetailFractions <- function( response, predictorsForm, metricName
                                ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                                ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                                ) {
        #cat('DetailFractions\n'); browser()
        DetailEither( response, predictorsForm, metricName
                     ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                     ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                     ,data.format.fractions
                     )
    }

    Append <- function(line) {
        lines$Append(line)
    }


    Get <- function() {
       lines$Get()
    }

    list( Header1            = Header1
         ,Header2            = Header2
         ,Blank              = Blank
         ,DetailWholeNumbers = DetailWholeNumbers
         ,DetailFractions    = DetailFractions
         ,Append             = Append
         ,Get                = Get
         )
}
Table.7.B <- function(lines) {
    # return table function object $Header1() $Header2() $Detail() $Formated() $Blank() $Get()
    # each of which appends to Lines object line
    case   <- '%8s %8s %8s'
    header.format            <- paste0(case, ' %5s', ' %6s')
    data.format.whole.number <- paste0(case, ' %5s', ' %6.f')
    data.format.fraction     <- paste0(case, ' %5s', ' %6.3f')

    Header <- function(response, predictorsForm, metric, ndays, value) {
        lines$Append(sprintf( header.format
                             ,response
                             ,predictorsForm
                             ,metric
                             ,ndays
                             ,value
                             )
        )
    }
    
    Blank <- function() {
        lines$Append(' ')
    }

    Detail <- function( response, predictorsForm, metricName
                       ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                       ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                       ,data.format
                       ) {
        line <- sprintf( data.format
                        ,response, predictorsForm, metricName
                        ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                        ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                        )
        lines$Append(line)
    }

    DetailWholeNumber <- function(response, predictorsForm, metricName, ndays, value) {
        lines$Append(sprintf( data.format.whole.number
                             ,response
                             ,predictorsForm
                             ,metricName
                             ,ndays
                             ,value
                             )
        )
    }

    DetailFraction <- function(response, predictorsForm, metricName, ndays, value) {
        lines$Append(sprintf( data.format.fraction
                             ,response
                             ,predictorsForm
                             ,metricName
                             ,ndays
                             ,value
                             )
        )
    }

    Append <- function(line) {
        lines$Append(line)
    }


    Get <- function() {
       lines$Get()
    }

    list( Header            = Header
         ,Blank             = Blank
         ,DetailWholeNumber = DetailWholeNumber
         ,DetailFraction    = DetailFraction
         ,Append             = Append
         ,Get                = Get
         )
}
Chart.7 <- function(my.control) {
    # return txt lines for chart 7
    parts.a.and.b <- FALSE
    fixed <- Chart.7.Fixed.Cell.Values()
    Path <- CvCell()$Path
    CvResult <- function(ndays, response, predictorsForm) {
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
    Header <- function(lines) {
        # mutate lines by appending the header
        lines$Append('Comparison of Metrics From from 10 Fold Cross Validation')
        lines$Append('Median of Root Median Squared Errors (medRMSE) vs.')
        lines$Append('Mean of Fraction of Predictions Within 10 Percent of Actual Values (fctWI10)')

        stopifnot(fixed$scope == 'global')
        stopifnot(fixed$model == 'linear')
        stopifnot(fixed$timePeriod == '2003on')
        stopifnot(fixed$predictorsName == 'alwaysNoAssessment')
        stopifnot(fixed$query == '100')

        Headers.Fixed(fixed, lines)
    }
    PartA <- function(lines) {
        table <- Table.7.A(lines)
        if (parts.a.and.b)
            table$Append('Part A: All Results')
        table$Append(' ')
        table$Header1('preds', 'ndays')
        table$Header2('response', 'form', 'Metric',
                      '30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360'
                      )

        DetailLine <- function(response, predictorsForm, metricName) {
            Value <- function(ndays) {
                cv.result <- CvResult( ndays = ndays
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

            Detail <- switch( metricName
                             ,medRMSE = table$DetailWholeNumbers
                             ,fctWI10 = table$DetailFractions
                             )

            Detail( response, predictorsForm, metricName
                   ,Value(30),   Value(60),  Value(90), Value(120), Value(150), Value(180)
                   ,Value(210), Value(240), Value(270), Value(300), Value(330), Value(360)
                   )
        }

        for (response in c('price', 'logprice')) {
            for (predictorsForm in c('level', 'log')) {
                table$Append(' ')
                for (metricName in c('medRMSE', 'fctWI10')) {
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
                cv.result <- CvResult( ndays = ndays
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


    lines <- Lines()
    Header(lines)
    lines$Append(' ')
    PartA(lines)
    if (parts.a.and.b) {
        lines$Append(' ')
        PartB(lines)
    }

    result <- lines$Get()
    result
}
Table.8 <- function(lines) {
    # return function object $Header1() $Header2() $Detail() $Get()
    header.format <- '%8s %8s %6s %6s %6s %6s %6s %6s %6s %6s %6s %6s %6s %6s'
    data.format   <- '%8s %8s %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f'

    Header1 <- function(predictorsForm, ndays30) {
        Header2( predictorsForm = predictorsForm
                ,ndays30 = ndays30
                )
    }
    Header2 <- function( response = ' ', predictorsForm = ' '
                        ,ndays30 = ' ', ndays60 = ' ', ndays90 = ' '
                        ,ndays120 = ' ', ndays150 = ' ', ndays180 = ' '
                        ,ndays210 = ' ', ndays240 = ' ', ndays270 = ' '
                        ,ndays300 = ' ', ndays330 = ' ', ndays360 = ' ') {
        line <- sprintf( header.format
                        ,response, predictorsForm
                        ,ndays30, ndays60, ndays90
                        ,ndays120, ndays150, ndays180
                        ,ndays210, ndays240, ndays270
                        ,ndays300, ndays330, ndays360
                        )
        lines$Append(line)
    }
    Detail <- function( response, predictorsForm
                       ,ndays30, ndays60, ndays90
                       ,ndays120, ndays150, ndays180
                       ,ndays210, ndays240, ndays270
                       ,ndays300, ndays330, ndays360
                       ) {
        line <- sprintf( data.format
                        ,response, predictorsForm
                        ,ndays30, ndays60, ndays90
                        ,ndays120, ndays150, ndays180
                        ,ndays210, ndays240, ndays270
                        ,ndays300, ndays330, ndays360
                        )
        lines$Append(line)
    }
    Get <- function() {
        lines$Get()
    }
    list( Header1 = Header1
         ,Header2 = Header2
         ,Detail  = Detail
         ,Get     = Get
         )
}
Chart.8 <- function(my.control) {
    # return txt lines for chart 8
    fixed <- Chart.8.Fixed.Cell.Values()
    Path <- CvCell()$Path
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
        lines$Append('Model form and Length of Training Period')

        stopifnot(fixed$scope == 'global')
        stopifnot(fixed$model == 'linear')
        stopifnot(fixed$timePeriod == '2003on')
        stopifnot(fixed$predictorsName == 'alwaysNoAssessment')
        stopifnot(fixed$query == '100')

        Headers.Fixed(fixed, lines)
    }
    Table <- function(lines) {
        # append lines for Table 8 to Lines object lines
        table <- Table.8(lines)
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

    # body starts here
    lines <- Lines()
    Header(lines)

    lines$Append(' ')
    Table(lines)

    result <- lines$Get()
    result
}
CI.Table <- function(axis.names, axis.values, names, values, values.low, values.high) {
    # return list of chr, a table
    format.header <- '%20s %15s %30s'
    format.data   <- '%20s %15.0f        [%6.0f,%6.0f]'
    lines <- Lines()
    lines$Append(axis.values)
    lines$Append(' ')
    lines$Append(sprintf(format.header, axis.names, 'median', '95% confidence interval'))
    lines$Append(' ')
    for (index in 1:length(values)) {
        lines$Append(sprintf( format.data
                             ,names[[index]]
                             ,values[[index]]
                             ,values.low[[index]]
                             ,values.high[[index]]))
    }
    result <- lines$Get()
    result
}
CI.Chart <- function(axis.names, axis.values, names, values, values.low, values.high
                     ,show.zero.value) {
    # Cleveland dot plot showing confidence intervals
    # ref: G Graphics Cookbook, p 42 and following
    df <- data.frame( stringsAsFactors = FALSE
                     ,names = names
                     ,values = values
                     ,values.low = values.low
                     ,values.high = values.high
                     )
    gg <- ggplot( df
                 ,aes( x = values
                      ,y = reorder(names, length(names):1)
                      )
                 )
    g1 <-
        gg +
        xlab(axis.values) +
        ylab(axis.names) +
        geom_point(aes(x = values), size = 3) +
        geom_point(aes(x = values.low), size = 2) +
        geom_point(aes(x = values.high), size = 2) +
        theme_bw() +
        theme( panel.grid.major.x = element_blank()
              ,panel.grid.minor.x = element_blank()
              ,panel.grid.major.y = element_line( colour = 'grey60'
                                                 ,linetype = 'dashed'
                                                 )
              )
    g <- if (show.zero.value) g1 + coord_cartesian(xlim = c(0, 1.1 * max(values.high))) else g1 
    g
}
Table.9 <- function(lines) {
    # append to Lines object lines
    header.format <- '%15s %30s %15s  %15s'
    data.format   <- '%15.0f %30s %15.0f  [%6.0f,%6.0f]'

    Header <- function(num.features, name, median, ci) {
        line <- sprintf(header.format, num.features, name, median, ci)
        lines$Append(line)
    }

    Detail <- function(num.features, name, median, ci.low, ci.high) {
        line <- sprintf(data.format, num.features, name, median, ci.low, ci.high)
        lines$Append(line)
    }

    list( Header = Header
         ,Detail = Detail
         )
}
Chart.9.10 <- function(my.control, feature.names, predictors.names) {
    # produce 3 charts for chart 9 and 10
    # ARGS
    # feature.names: chr vector of names of features
    # predictors.names: chr vector of names of predictors 
    #  (ex: best01, best02, ..., best24)
    #  (ex: pca01, pca02, ..., pca04)
    fixed <- Chart.9.Fixed.Cell.Values()
    Path <- CvCell()$Path
    ACvResult <- function(num.features) {
        predictorsName <- predictors.names[[num.features]]
        path.in <- Path( scope = 'global'
                        ,model = 'linear'
                        ,timePeriod = '2003on'
                        ,scenario = 'avm'
                        ,response = 'logprice'
                        ,predictorsName = predictorsName
                        ,predictorsForm = 'level'
                        ,ndays = '60'
                        ,query = '100'
                        ,lambda = '0'
                        ,ntree = '0'
                        ,mtry = '0'
                        )
        load(path.in)
        stopifnot(length(cv.result) == 1)
        a.cv.result <- cv.result[[1]]
        a.cv.result
    }
    Summarize <- function() {
        # return list $feature.name $median.value $ci.lowest $ci.highest
        n <- length(feature.names)
        median.value <- double(n)
        ci.lowest <- double(n)
        ci.highest <- double(n)
        for (feature.num in 1:n) {
            a.cv.result <- ACvResult(feature.num)
            rmse.values <- RootMedianSquaredErrors(a.cv.result)
            ci <- CIMedian(rmse.values)
            median.value[[feature.num]] <- median(rmse.values)
            ci.lowest[[feature.num]] <- ci$lowest
            ci.highest[[feature.num]] <- ci$highest
        }
        result <- list( feature.names = feature.names
                       ,median.value  = median.value
                       ,ci.lowest     = ci.lowest
                       ,ci.highest    = ci.highest
                       )
        result
        }
    GraphChart <- function(summary, show.zero.value) {
        gg <- CI.Chart( axis.values = 'median rMedianSE across folds'
                       ,axis.names = 'cumulative feature names'
                       ,values = summary$median.value
                       ,values.low = summary$ci.lowest
                       ,values.high = summary$ci.highest
                       ,names = summary$feature.name
                       ,show.zero.value = show.zero.value
                       )
        gg
    }
    TextChart <- function(summary) {
        Header <- function(lines) {
            lines$Append('Estimated Generalization Errors from 10-fold Cross Validation')

            stopifnot(fixed$scope == 'global')
            stopifnot(fixed$model == 'linear')
            stopifnot(fixed$timePeriod == '2003on')
            stopifnot(fixed$predictorsForm == 'level')
            stopifnot(fixed$ndays == '60')
            stopifnot(fixed$query == '100')

            Headers.Fixed(fixed, lines)
        }
        Body <- function(lines) {
            table <- Table.9(lines)
            table$Header('num features', 'nth feature name', 'median RMSE', '95% confidence interval')
            lines$Append(' ')

            for (num.features in 1:length(summary$feature.name)) {
                table$Detail( num.features
                             ,summary$feature.name[[num.features]]
                             ,summary$median.value[[num.features]]
                             ,summary$ci.lowest[[num.features]]
                             ,summary$ci.highest[[num.features]]
                             )
            }
        }

        # execution starts here
        lines <- Lines()
        Header(lines)
        lines$Append(' ')
        Body(lines)
        result <- lines$Get()
        result
    }

    summary <- Summarize()
    result <- list( txt = TextChart(summary)
                   ,gg1 = GraphChart(summary, show.zero.value = TRUE)
                   ,gg2 = GraphChart(summary, show.zero.value = FALSE)
                   )
    result
}
Chart.9 <- function(my.control) {
    # return 3 results for chart 9
    feature.names <- readLines(con = paste0(my.control$working, 'e-features-lcv2.txt'))
    predictors.names <- sprintf('best%02d', 1:24)
    result <- Chart.9.10( my.control = my.control
                         ,feature.names = feature.names
                         ,predictors.names = predictors.names
                         )
    result
}
Chart.10 <- function(my.control) {
    feature.names <- Predictors2( predictors.name = 'pca04'  # get all the features in order
                                 ,predictors.form = 'level'
                                 )
    predictors.names <- sprintf('pca%02d', 1:4)
    result <- Chart.9.10( my.control = my.control
                         ,feature.names = feature.names
                         ,predictors.names = predictors.names
                         )
    result
}
Chart.11 <- function(my.control) {
    # return 2 gg charts
    fixed <- Chart.11.Fixed.Cell.Values()

    Path <- CvCell()$Path
    ACvResult <- function(predictorsName) {
        path.in <- Path( scope = fixed$scope
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
        load(path.in)
        stopifnot(length(cv.result) == 1)
        a.cv.result <- cv.result[[1]]
        a.cv.result
    }
    Summarize <- function() {
        # return list $predictors.name $median.value $ci.lowest $ci.highest
        predictors.names <- Chart.11.PredictorsNames()
        n <- length(predictors.names)
        median.value <- double(n)
        ci.lowest <- double(n)
        ci.highest <- double(n)
        for (index in 1:n) {
            predictors.name <- predictors.names[[index]]
            a.cv.result <- ACvResult(predictors.name)
            rmse.values <- RootMedianSquaredErrors(a.cv.result)
            ci <- CIMedian(rmse.values)
            median.value[[index]] <- median(rmse.values)
            ci.lowest[[index]] <- ci$lowest
            ci.highest[[index]] <- ci$highest
        }
        result <- list( predictors.names = predictors.names
                       ,median.value  = median.value
                       ,ci.lowest     = ci.lowest
                       ,ci.highest    = ci.highest
                       )
        result
    }
    GraphChart <- function(summary, show.zero.value) {
        gg <- CI.Chart( axis.values = 'median rMedianSE across folds'
                       ,axis.names = 'predictor feature set names'
                       ,values = summary$median.value
                       ,values.low = summary$ci.lowest
                       ,values.high = summary$ci.highest
                       ,names = summary$predictors.names
                       ,show.zero.value = show.zero.value
                       )
        gg
    }
    summary <- Summarize()
    result <- list( gg1 = GraphChart(summary, show.zero.value = TRUE)
                   ,gg2 = GraphChart(summary, show.zero.value = FALSE)
                   )
    result
}
Chart.12 <- function(my.control) {
    # return 2 gg charts
    fixed <- Chart.12.Fixed.Cell.Values()


    cv.cell <- CvCell()
    Path <- cv.cell$Path
    C.To.Lambda <- cv.cell$C.To.Lambda

    ACvResult <- function(lambda) {
        path.in <- Path( scope = fixed$scope
                        ,model = fixed$model
                        ,timePeriod = fixed$timePeriod
                        ,scenario = fixed$scenario
                        ,response = fixed$response
                        ,predictorsName = fixed$predictorsName
                        ,predictorsForm = fixed$predictorsForm
                        ,ndays = fixed$ndays
                        ,query = fixed$query
                        ,lambda = lambda
                        ,ntree = fixed$ntree
                        ,mtry = fixed$mtry
                        )
        load(path.in)
        stopifnot(length(cv.result) == 1)
        a.cv.result <- cv.result[[1]]
        a.cv.result
    }
    Summarize <- function() {
        # return list $predictors.name $median.value $ci.lowest $ci.highest
        possible.lambda.values <- Chart.12.Lambda.Values()
        n <- length(possible.lambda.values)
        median.value <- double(n)
        ci.lowest <- double(n)
        ci.highest <- double(n)
        c.values <- double(n)
        lambda.value.name <- character(n)
        for (index in 1:n) {
            lambda <- possible.lambda.values[[index]]
            a.cv.result <- ACvResult(lambda)
            rmse.values <- RootMedianSquaredErrors(a.cv.result)
            ci <- CIMedian(rmse.values)
            median.value[[index]] <- median(rmse.values)
            ci.lowest[[index]] <- ci$lowest
            ci.highest[[index]] <- ci$highest
            lambda.value.name[[index]] <- lambda
        }
        result <- list( lambda.value.name = as.character(as.numeric(lambda.value.name) / 100)
                       ,median.value  = median.value
                       ,ci.lowest     = ci.lowest
                       ,ci.highest    = ci.highest
                       )
        result
    }
    GraphChart <- function(summary, show.zero.value) {
        gg <- CI.Chart( axis.values = 'median rMedianSE across folds'
                       ,axis.names = 'L2 regularizer value'
                       ,values = summary$median.value
                       ,values.low = summary$ci.lowest
                       ,values.high = summary$ci.highest
                       ,names = summary$lambda.value.name
                       ,show.zero.value = show.zero.value
                       )
        gg
    }
    TableChart <- function(summary) {
        txt <- CI.Table( axis.values = 'Median rMedianSE Across Folds'
                        ,axis.names = 'L2 regularizer value'
                        ,values = summary$median.value
                        ,values.low = summary$ci.lowest
                        ,values.high = summary$ci.highest
                        ,names = summary$lambda.value.name
                        )
        txt
    }
    summary <- Summarize()
    result <- list( gg1 = GraphChart(summary, show.zero.value = TRUE)
                   ,gg2 = GraphChart(summary, show.zero.value = FALSE)
                   ,txt = TableChart(summary)
                   )
    result
}
Chart.13 <- function(my.control) {
    # return list of txt vectors

    verbose <- TRUE

    fixed <- Chart.13.Fixed.Cell.Values()
    cv.cell <- CvCell()
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
            else
                togo$Append(path)
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
        Headers.Fixed(fixed,report)

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
                stop('all files should exist')
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
            Headers.Fixed(fixed, report)

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
Chart.14 <- function(my.control) {
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
    fixed <- Chart.14.Fixed.Cell.Values()
    Path <- CvCell()$Path

    report <- Lines()
    report$Append('Comparison of Estimated Generalization Errors')
    report$Append('From Random Forests')
    report$Append(' ')
    Headers.Fixed(fixed, report)

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
MakeMakefiles <- function(control) {
    # write one makefile for each chart that is being created

#    chart.1 <- Chart.1.2(control)
#    chart.2 <- Chart.1.2(control)
#    chart.3 <- Chart.3(control)
#    chart.4 <- Chart.4(control)

    Path <- CvCell()$Path
    Command <- CvCell()$Command

    targets.lines <- Lines()
    rules.lines <- Lines()

    
    num.threads <- 20  # 8 on J's system, 12 on R's system
    thread.number <- 0
    targets.lines <- Lines()
    all.dependency.file.names <- NULL
    M <- function(target.variable.name, dependency.file.names) {
        # append to all.dependency.file.names
        # build targets.lines
        for (dfn in dependency.file.names) {
            all.dependency.file.names[[length(all.dependency.file.names) + 1]] <<- dfn
            path <- Path( scope = dfn$scope
                         ,model = dfn$model
                         ,timePeriod = dfn$timePeriod
                         ,scenario = dfn$scenario
                         ,response = dfn$response
                         ,predictorsName = dfn$predictorsName
                         ,predictorsForm = dfn$predictorsForm
                         ,ndays = dfn$ndays
                         ,query = dfn$query
                         ,lambda = dfn$lambda
                         ,ntree = dfn$ntree
                         ,mtry = dfn$mtry
                         )

            # distribute the jobs over the threads
            thread.number <- thread.number + 1 
            if (thread.number > num.threads)
                thread.number <- 1 
            targets.lines$Append(sprintf('%s-thread-%d += %s'
                                         ,target.variable.name
                                         ,thread.number
                                         ,path
                                         ))
        }
        # assign 8 thread's to J's system and 12 thread's to R's
        CreateTarget <- function(tag, threads) {
            system.target.name <- sprintf('%s-target-%s'
                                          ,target.variable.name
                                          ,tag
                                          )
            targets.lines$Append(sprintf('.PHONY: %s'
                                         ,system.target.name
                                         ))
            uses <- ''
            for (thread.index in threads) {
                uses <- sprintf('%s $(%s)'
                                ,uses
                                ,sprintf('%s-thread-%d'
                                         ,target.variable.name
                                         ,thread.index
                                         )
                                )
            }
            targets.lines$Append(sprintf('%s : %s'
                                         ,system.target.name
                                         ,uses
                                         ))
        }
        CreateTarget('r', 1:12)  # R runs threads 1 - 12
        CreateTarget('j', 13:20) # J runs threads 13 - 20
        CreateTarget('all', 1:20)
        targets.lines
        # return nothing
        NULL
    }

    M( target.variable.name = 'e-cv-chart-chart5'
      ,dependency.file.names = Chart.5.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart6'
      ,dependency.file.names = Chart.6.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart7'
      ,dependency.file.names = Chart.7.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart8'
      ,dependency.file.names = Chart.8.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart9'
      ,dependency.file.names = Chart.9.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart10'
      ,dependency.file.names = Chart.10.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart11'
      ,dependency.file.names = Chart.11.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart12'
      ,dependency.file.names = Chart.12.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart13'
      ,dependency.file.names = Chart.13.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart14'
      ,dependency.file.names = Chart.14.FileDependencies(control)
      )

    RulesRecipes <- function(all.dependency.file.names) {
        # return Lines object containing unique rules and recipes
        # NOTE: all.dependency.file.name may contain the same value many times
        # because the same cell is used in many charts
        u <- unique(all.dependency.file.names)  # does this work on lists?
        #print(length(all.dependency.file.names))
        #print(length(u))
        stopifnot(length(u) < length(all.dependency.file.names))
        rules.recipes <- Lines()
        for (dfn in u) {
            path <- Path( scope = dfn$scope
                         ,model = dfn$model
                         ,timePeriod = dfn$timePeriod
                         ,scenario = dfn$scenario
                         ,response = dfn$response
                         ,predictorsName = dfn$predictorsName
                         ,predictorsForm = dfn$predictorsForm
                         ,ndays = dfn$ndays
                         ,query = dfn$query
                         ,lambda = dfn$lambda
                         ,ntree = dfn$ntree
                         ,mtry = dfn$mtry
                         )
            command <- Command( scope = dfn$scope
                               ,model = dfn$model
                               ,timePeriod = dfn$timePeriod
                               ,scenario = dfn$scenario
                               ,response = dfn$response
                               ,predictorsName = dfn$predictorsName
                               ,predictorsForm = dfn$predictorsForm
                               ,ndays = dfn$ndays
                               ,query = dfn$query
                               ,lambda = dfn$lambda
                               ,ntree = dfn$ntree
                               ,mtry = dfn$mtry
                               )
            rules.recipes$Append(paste0(path ,': e-cv.R $(e-cv-source) $(e-cv-data)'))
            rules.recipes$Append(paste0('\t', command))
        }
        rules.recipes
    }

    # targets.lines and all.dependency.file.names
    # now combine them into one makefile

    all.lines <- Lines()
    # comments for human readers
    all.lines$Append('# rules and targets to make cv cells needed by charts produced by e-cv-chart.r')
    all.lines$Append('# to generate files needed for Chart 12, execute')
    all.lines$Append('#   make -j 12 e-cv-chart-chart12')
    all.lines$Append(' ')
    all.lines$Append('# generated by Rscript e-cv-chart.R --makefile')
    all.lines$Append(paste0('# run ', Sys.time()))
    
    # rules and recipes
    all.lines$Append(' ')
    all.lines$Append('# rules and recipes')
    rules.recipes <- RulesRecipes(all.dependency.file.names)
    lapply(rules.recipes$Get(), function(line) all.lines$Append(line))
    
    # targets (to allow data for individual charts to be created and recreated)
    all.lines$Append(' ')
    all.lines$Append('# targets')
    lapply(targets.lines$Get(), function(line) all.lines$Append(line))

    writeLines( text = all.lines$Get()
               ,con = control$path.out.makefile
               )
}
MakeCharts <- function(control) {
    # write chart files:

    Make.Chart.5 <- function() {
        chart.5.txt <- Chart.5(control)
        writeLines( text = chart.5.txt
                   ,con = control$path.out.chart.5
                   )
    }
    Make.Chart.6 <- function() {
        chart.6.txt <- Chart.6(control)
        writeLines( text = chart.6.txt
                   ,con = control$path.out.chart.6
                   )
    }
    Make.Chart.7 <- function() {
        chart.7.txt <- Chart.7(control)
        writeLines( text = chart.7.txt
                   ,con = control$path.out.chart.7
                   )
    }
    Make.Chart.8 <- function() {
        chart.8.txt <- Chart.8(control)
        writeLines( text = chart.8.txt
                   ,con = control$path.out.chart.8
                   )
    }
    Make.Chart.9 <- function() {
        # Chart.9 returns list $txt $gg1 $gg2
        chart.9 <- Chart.9(control)
        writeLines( text = chart.9$txt
                   ,con = control$path.out.chart.9.txt
                   )

        pdf( file = control$path.out.chart.9.gg1
            ,width = control$chart.width
            ,height = control$chart.height
            )
        print(chart.9$gg1)
        dev.off()

        pdf( file = control$path.out.chart.9.gg2
            ,width = control$chart.width
            ,height = control$chart.height
            )
        print(chart.9$gg2)
        dev.off()
    }
    Make.Chart.10 <- function() {
        # Chart.10 returns list $txt $gg1 $gg2 (same charts as for chart 9)
        chart.10 <- Chart.10(control)
        writeLines( text = chart.10$txt
                   ,con = control$path.out.chart.10.txt
                   )

        pdf( file = control$path.out.chart.10.gg1
            ,width = control$chart.width
            ,height = control$chart.height
            )
        print(chart.10$gg1)
        dev.off()

        pdf( file = control$path.out.chart.10.gg2
            ,width = control$chart.width
            ,height = control$chart.height
            )
        print(chart.10$gg2)
        dev.off()
    }
    Make.Chart.11 <- function() {
        # Chart.11 returns list $txt $gg1 $gg2 (same charts as for chart 9)
        chart.11 <- Chart.11(control)
        #    writeLines( text = chart.11$txt
        #               ,con = control$path.out.chart.11.txt
        #               )

        pdf( file = control$path.out.chart.11.gg1
            ,width = control$chart.width
            ,height = control$chart.height
            )
        print(chart.11$gg1)
        dev.off()

        pdf( file = control$path.out.chart.11.gg2
            ,width = control$chart.width
            ,height = control$chart.height
            )
        print(chart.11$gg2)
        dev.off()
    }
    Make.Chart.12 <- function() {
        # Chart.12 returns list $txt $gg1 $gg2 (same charts as for chart 9)
        chart.12 <- Chart.12(control)
        writeLines( text = chart.12$txt
                   ,con = control$path.out.chart.12.txt
                   )

        pdf( file = control$path.out.chart.12.gg1
            ,width = control$chart.width
            ,height = control$chart.height
            )
        print(chart.12$gg1)
        dev.off()

        pdf( file = control$path.out.chart.12.gg2
            ,width = control$chart.width
            ,height = control$chart.height
            )
        print(chart.12$gg2)
        dev.off()
    }
    Make.Chart.13 <- function() {
        chart.13 <- Chart.13(control)
        writeLines( text = chart.13$indicators
                   ,con = control$path.out.chart.13.indicators.txt
                   )
        writeLines( text = chart.13$submarkets.summary
                   ,con = control$path.out.chart.13.submarkets.summary.txt
                   )
        writeLines( text = chart.13$submarkets.census
                   ,con = control$path.out.chart.13.submarkets.census.txt
                   )
        writeLines( text = chart.13$submarkets.property.city
                   ,con = control$path.out.chart.13.submarkets.property.city.txt
                   )
        writeLines( text = chart.13$submarkets.zip5
                   ,con = control$path.out.chart.13.submarkets.zip5.txt
                   )
    }
    Make.Chart.14 <- function() {
        browser()
        chart.14 <- Chart.14(control)
        writeLines( text = chart.14
                   ,con = control$path.out.chart.14.txt
                   )
    }
    
    developing <- FALSE
    if (!control$testing) {
        Make.Chart.5()
        Make.Chart.6()
        Make.Chart.7()
        Make.Chart.8()
        Make.Chart.9()
        Make.Chart.10()
        Make.Chart.11()
        Make.Chart.12()
        Make.Chart.13()
        Make.Chart.14()
    } else {
        # while developing
    }
}
Main <- function(control) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    # either produce the makefile or create the charts
    if (control$opt$makefile) {
        MakeMakefiles(control)  # write several generated makefiles
    } else {
        MakeCharts(control)
    }

    str(control)
}


### Execution starts here

default.args <- list( makefile = TRUE) 
default.args <- list( makefile = FALSE) 

control <- Control(default.args)

Main(control)
if (default.args$makefile)
    stop('FIX DEFAULT ARGS SO THAT PROGRAM PRODUCES CHARTS')
if (control$testing)
    cat('DISCARD RESULTS: TESTING\n')
cat('done\n')
