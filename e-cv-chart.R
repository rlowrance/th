# e-cv-chart.R  
# vim: foldmethod=manual
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
                    ,path.out.chart.5.generated.makefile = 'e-cv-chart-chart5-generated.makefile'
                    ,path.out.chart.6.generated.makefile = 'e-cv-chart-chart6-generated.makefile'
                    ,path.out.chart.7.generated.makefile = 'e-cv-chart-chart7-generated.makefile'
                    ,path.out.chart.8.generated.makefile = 'e-cv-chart-chart8-generated.makefile'
                    ,path.out.chart.9.generated.makefile = 'e-cv-chart-chart9-generated.makefile'
                    ,path.out.chart.10.generated.makefile = 'e-cv-chart-chart10-generated.makefile'
                    ,path.out.chart.11.generated.makefile = 'e-cv-chart-chart11-generated.makefile'
                    ,path.out.chart.12.generated.makefile = 'e-cv-chart-chart12-generated.makefile'
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
CIMedian <- function(values) {
    # return 95% confidence interval for the median of the vector of values
    # ref: R Cookbook, p 184
    num.resamples <- 100  # TODO: set to 10,000
    num.resamples <- 10000
    medians <- numeric(length = num.resamples)  # preallocate for speed
    lapply( 1:num.resamples
           ,function(index) medians[[index]] <<- median(sample(values, replace = TRUE))
           )
    confidence.interval <- quantile( x = medians
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
Chart.5.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 5 
    # predictorsName in {always, alwaysNoAssesment}
    # timePeriod 2008

    Path <- CvCell()$Path
    file.dependencies <- Lines()
    for (response in c('price', 'logprice')) {
        for (predictorsName in c('always', 'alwaysNoAssessment')) {
            for (predictorsForm in c('level', 'log')) {
                for (ndays in CvCell()$Possible.Ndays()) {
                    path <- 
                        Path( scope = 'global'
                             ,model = 'linear'
                             ,timePeriod = '2008'
                             ,scenario = 'avm'
                             ,response = response
                             ,predictorsName = predictorsName
                             ,predictorsForm = predictorsForm
                             ,ndays = ndays
                             ,query = '1'
                             ,c = '0'
                             ,ntree = '0'
                             ,mtry = '0'
                             )
                    file.dependencies$Append(path)
                }
            }
        }
    }

    result <- file.dependencies$Get()
    result
}
Chart.6.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 6
    # predictorsName in {alwaysNoAssesment, alwaysNoCensus}
    # timePeriod 2003on

    Path <- CvCell()$Path
    file.dependencies <- Lines()
    for (response in c('price', 'logprice')) {
        for (predictorsName in c('alwaysNoAssessment', 'alwaysNoCensus')) {
            for (predictorsForm in c('level', 'log')) {
                for (ndays in CvCell()$Possible.Ndays()) {
                    path <- 
                        Path( scope = 'global'
                             ,model = 'linear'
                             ,timePeriod = '2003on'
                             ,scenario = 'avm'
                             ,response = response
                             ,predictorsName = predictorsName
                             ,predictorsForm = predictorsForm
                             ,ndays = ndays
                             ,query = '100'
                             ,c = '0'
                             ,ntree = '0'
                             ,mtry = '0'
                             )
                    file.dependencies$Append(path)
                }
            }
        }
    }

    result <- file.dependencies$Get()
    result
}
Chart.7.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 7 

    Path <- CvCell()$Path
    file.names <- Lines()
    for (response in c('price', 'logprice')) {
        for (predictorsName in 'alwaysNoAssessment') {
            for (predictorsForm in c('level', 'log')) {
                for (ndays in CvCell()$Possible.Ndays()) {
                    path <- Path( scope = 'global'
                                 ,model = 'linear'
                                 ,timePeriod = '2003on'
                                 ,scenario = 'avm'
                                 ,response = response
                                 ,predictorsName = predictorsName
                                 ,predictorsForm = predictorsForm
                                 ,ndays = ndays
                                 ,query = '100'
                                 ,c = '0'
                                 ,ntree = '0'
                                 ,mtry = '0'
                                 )
                    file.names$Append(path)
                }
            }
        }
    }

    result <- file.names$Get()
    result
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
Chart.9.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 9
    # this is just for the best form and ndays
    # best for is log-level
    # best ndays is 60 

    Path <- CvCell()$Path
    file.names <- Lines()
    for (predictorsName in Chart.9.PredictorsNames(my.control)) {
        path <- 
            Path( scope = 'global'
                 ,model = 'linear'
                 ,timePeriod = '2003on'
                 ,scenario = 'avm'
                 ,response = 'logprice'
                 ,predictorsName = predictorsName
                 ,predictorsForm = 'level'
                 ,ndays = '60'
                 ,query = '100'
                 ,c = '0'
                 ,ntree = '0'
                 ,mtry = '0'
                 )
        file.names$Append(path)
    }
    result <- file.names$Get()
    result
}
Chart.10.PredictorsNames <- function() {
    # return vector of predictor names used in chart 10
    result <- sprintf('pca%02d', 1:4)
    result
}
Chart.10.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 10

    Path <- CvCell()$Path
    #debug(Path)
    file.names <- Lines()
    possible <- Chart.10.PredictorsNames()
    for (predictorsName in possible) {
        path <- 
            Path( scope = 'global'
                 ,model = 'linear'
                 ,timePeriod = '2003on'
                 ,scenario = 'avm'
                 ,response = 'logprice'
                 ,predictorsName = predictorsName
                 ,predictorsForm = 'level'
                 ,ndays = '60'
                 ,query = '100'
                 ,c = '0'
                 ,ntree = '0'
                 ,mtry = '0'
                 )
        file.names$Append(path)
    }
    result <- file.names$Get()
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
Chart.11.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 11

    Path <- CvCell()$Path
    file.names <- Lines()
    possible <- Chart.11.PredictorsNames()
    for (predictorsName in possible) {
        path <- 
            Path( scope = 'global'
                 ,model = 'linear'
                 ,timePeriod = '2003on'
                 ,scenario = 'avm'
                 ,response = 'logprice'
                 ,predictorsName = predictorsName
                 ,predictorsForm = 'level'
                 ,ndays = '60'
                 ,query = '100'
                 ,c = '0'
                 ,ntree = '0'
                 ,mtry = '0'
                 )
        file.names$Append(path)
    }
    result <- file.names$Get()
    result
}
Chart.12.C.Values <- function() {
    # return vector of lambda values that are used for regularization
    # lambda := C / 100
    lambda <- c( 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
    c.values <- round(100 * ifelse(lambda == 0, 0 , 1 / lambda))
    c.values
}
Chart.12.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 11

    Path <- CvCell()$Path
    Possible.C.Values <- CvCell()$Possible.C.Values
    file.names <- Lines()
    c.values <- Possible.C.Values()
    for (c.value in c.values) {
        path <- 
            Path( scope = 'global'
                 ,model = 'linL2'
                 ,timePeriod = '2003on'
                 ,scenario = 'avm'
                 ,response = 'logprice'
                 ,predictorsName = 'best20'
                 ,predictorsForm = 'level'
                 ,ndays = '60'
                 ,query = '100'
                 ,c = as.character(c.value)
                 ,ntree = '0'
                 ,mtry = '0'
                 )
        file.names$Append(path)
    }
    result <- file.names$Get()
    result
}
Table.5.6 <- function() {
    # return table function object $Header1() $Header2() $Detail() $Formated() $Blank() $Get()
    case   <- '%8s %8s %5s %3s'
    header.format <- paste0(case, paste0(rep(' %6s', 12), collapse = ''))
    data.format   <- paste0(case, paste0(rep(' %6.0f', 12), collapse = ''))

    lines <- Lines()
    Header1 <- function(col3, col4, col5) {
        # append a header record with mostly blank columns
        line <- sprintf(header.format, 
                        ' ', ' ',           # cols 1 - 2
                        col3, col4, col5,   # cols 3 - 5
                        ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' # cols 6 - 16
                        )
        lines$Append(line)
    }

    Header2 <- function(col1, col2, col3, col4, col5, col6, col7, col8,
                        col9, col10, col11, col12, col13, col14, col15, col16) {
        line <- sprintf(header.format,
                         col1, col2, col3, col4, col5, col6, col7, col8,
                         col9, col10, col11, col12, col13, col14, col15, col16
                         )
        lines$Append(line)
    }
    
    Blank <- function() {
        lines$Append(' ')
    }

    Detail <- function(col1, col2, col3, col4, col5, col6, col7, col8,
                       col9, col10, col11, col12, col13, col14, col15, col16) {
        line <- sprintf(data.format,
                        col1, col2, col3, col4, col5, col6, col7, col8,
                        col9, col10, col11, col12, col13, col14, col15, col16
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
Chart.5 <- function(my.control) {
    # return list of lines, the txt table for chart 5
    Header <- function() {
        lines <- Lines()
        lines$Append('Median of Root Median Squared Errors from 10 Fold Cross Validation')
        lines$Append('For global linear model')
        lines$Append('Data from 2008')
        lines$Append('Features Present in Every Transaction')
        lines$Append('With and Without Tax Assessment Features (Column Use Tax)')
        result <- lines$Get()
        result
    }

    table <- Table.5.6()
    table$Formatted(Header())
    table$Blank()
    table$Header1('preds', 'Use', 'ndays')
    table$Header2('scenario', 'response', 'Form', 'Tax',
                  '30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360'
                  )

    DetailLine <- function(scenario, response, predictorsName, predictorsForm) {
        M <- function(ndays) {
            path.in <- CvCell()$Path( scope = 'global'
                                     ,model = 'linear'
                                     ,timePeriod = '2008'
                                     ,scenario = scenario
                                     ,response = response
                                     ,predictorsName = predictorsName
                                     ,predictorsForm = predictorsForm
                                     ,ndays = ndays
                                     ,query = '1'
                                     ,c = '0'
                                     ,ntree = '0'
                                     ,mtry = '0'
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
        table$Detail( scenario, response, predictorsForm, use.assessment
                     ,M(30), M(60), M(90), M(120), M(150), M(180)
                     ,M(210), M(240), M(270), M(300), M(330), M(360)
                     )
    }

    for (scenario in c('avm')) {
        for (response in c('price', 'logprice')) {
            for (predictorsForm in c('level', 'log')) {
                for (predictorsName in c('always', 'alwaysNoAssessment')) {
                    DetailLine( scenario = scenario
                               ,response = response
                               ,predictorsName = predictorsName
                               ,predictorsForm = predictorsForm
                               )
                }
            }
        }
    }

    result <- table$Get()
    result
}
Chart.6 <- function(my.control) {
    # return txt lines for chart 6
    Header <- function() {
        lines <- Lines()
        lines$Append('Median of Root Median Squared Errors from 10 Fold Cross Validation')
        lines$Append('For global linear model')
        lines$Append('Data from 2003 Onward')
        lines$Append('Non Tax Assessment Features Present in Every Transaction')
        lines$Append('With and Without Census Tract Features (Column Use Cen)')
        result <- lines$Get()
        result
    }

    table <- Table.5.6()
    table$Formatted(Header())
    table$Blank()
    table$Header1('preds', 'Use', 'ndays')
    table$Header2('scenario', 'response', 'Form', 'Cen',
                  '30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360'
                  )

    Path <- CvCell()$Path
    DetailLine <- function(scenario, response, predictorsName, predictorsForm) {
        M <- function(ndays) {
            path.in <- Path( scope = 'global'
                             ,model = 'linear'
                             ,timePeriod = '2008'
                             ,scenario = scenario
                             ,response = response
                             ,predictorsName = predictorsName
                             ,predictorsForm = predictorsForm
                             ,nday = ndays
                             ,query = '1'
                             ,c = '0'
                             ,ntree = '0'
                             ,mtry = '0'
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
        table$Detail( scenario, response, predictorsForm, use.assessment
                     ,M(30), M(60), M(90), M(120), M(150), M(180)
                     ,M(210), M(240), M(270), M(300), M(330), M(360)
                     )
    }

    for (scenario in c('avm')) {
        for (response in c('price', 'logprice')) {
            for (predictorsForm in c('level', 'log')) {
                for (predictorsName in c('alwaysNoAssessment', 'alwaysNoCensus')) {
                    DetailLine( scenario = scenario
                               ,response = response
                               ,predictorsName = predictorsName
                               ,predictorsForm = predictorsForm
                               )
                }
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
    Path <- CvCell()$Path
    CvResult <- function(ndays, response, predictorsForm) {
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
                        ,c = '0'
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
        lines$Append('Comparison of Metrics From from 10 Fold Cross Validation')
        lines$Append('Median of Root Median Squared Errors (medRMSE) vs.')
        lines$Append('Mean of Fraction of Predictions Within 10 Percent of Actual Values (fctWI10)')
        lines$Append('For global linear model')
        lines$Append('Data from 2003 Onward')
        lines$Append('AVM scenario')
        lines$Append('Using random 1 percent sample from each validation fold')
    }
    PartA <- function(lines) {
        table <- Table.7.A(lines)
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
    lines$Append(' ')
    PartB(lines)

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
                        ,c = '0'
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
        lines$Append('For global linear model')
        lines$Append('Data from 2003 Onward')
        lines$Append('AVM scenario')
        lines$Append('Using random 1 percent sample from each validation fold')
        lines$Append('Metric = median of rMedianSE values from folds')
        lines$Append('Predictors: All Except Assessment')
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
                        ,c = '0'
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
            lines$Append('Model Form: log-linear')
            lines$Append('Training Period: 60 days')
            lines$Append('AVM Scenario')
            lines$Append('Metric: Median across folds of Root Median Squared Errors')
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

    Path <- CvCell()$Path
    ACvResult <- function(predictorsName) {
        path.in <- Path( scope = 'global'
                        ,model = 'linear'
                        ,timePeriod = '2003on'
                        ,scenario = 'avm'
                        ,response = 'logprice'
                        ,predictorsName = predictorsName
                        ,predictorsForm = 'level'
                        ,ndays = '60'
                        ,query = '100'
                        ,c = '0'
                        ,ntree = '0'
                        ,mtry = '0'
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
MakeMakefiles <- function(control) {
    # write one makefile for each chart that is being created

#    chart.1 <- Chart.1.2(control)
#    chart.2 <- Chart.1.2(control)
#    chart.3 <- Chart.3(control)
#    chart.4 <- Chart.4(control)

    MakeMakefile <- function(variable.name, dependency.file.names, path.out) {
        # create generated makefile that defines the variable and a target for the files
        lines <- Lines()
        for (dependency.file.name in dependency.file.names) {
            lines$Append(paste0( variable.name
                                ,' += '
                                ,dependency.file.name
                                )
            )
        }
        target.name <- paste0(variable.name, '-target')
        lines$Append(sprintf('.PHONY: %s', target.name))
        lines$Append(sprintf('%s: $(%s)', target.name, variable.name))
        writeLines( text = lines$Get()
                   ,con = path.out
                   )
        
        # return nothing
    }

    MakeMakefile( variable.name = 'e-cv-chart-chart5'
                 ,dependency.file.names = Chart.5.FileDependencies(control)
                 ,path.out = control$path.out.chart.5.generated.makefile
                 )
    MakeMakefile( variable.name = 'e-cv-chart-chart6'
                 ,dependency.file.names = Chart.6.FileDependencies(control)
                 ,path.out = control$path.out.chart.6.generated.makefile
                 )
    MakeMakefile( variable.name = 'e-cv-chart-chart7'
                 ,dependency.file.names = Chart.7.FileDependencies(control)
                 ,path.out = control$path.out.chart.7.generated.makefile
                 )
    MakeMakefile( variable.name = 'e-cv-chart-chart8'
                 ,dependency.file.names = Chart.8.FileDependencies(control)
                 ,path.out = control$path.out.chart.8.generated.makefile
                 )
    MakeMakefile( variable.name = 'e-cv-chart-chart9'
                 ,dependency.file.names = Chart.9.FileDependencies(control)
                 ,path.out = control$path.out.chart.9.generated.makefile
                 )
    MakeMakefile( variable.name = 'e-cv-chart-chart10'
                 ,dependency.file.names = Chart.10.FileDependencies(control)
                 ,path.out = control$path.out.chart.10.generated.makefile
                 )
    MakeMakefile( variable.name = 'e-cv-chart-chart11'
                 ,dependency.file.names = Chart.11.FileDependencies(control)
                 ,path.out = control$path.out.chart.11.generated.makefile
                 )
    MakeMakefile( variable.name = 'e-cv-chart-chart12'
                 ,dependency.file.names = Chart.12.FileDependencies(control)
                 ,path.out = control$path.out.chart.12.generated.makefile
                 )
}
MakeCharts <- function(control) {
    # write chart files:

    chart.5.txt <- Chart.5(control)
    writeLines( text = chart.5.txt
               ,con = control$path.out.chart.5
               )

    chart.6.txt <- Chart.6(control)
    writeLines( text = chart.6.txt
               ,con = control$path.out.chart.6
               )

    chart.7.txt <- Chart.7(control)
    writeLines( text = chart.7.txt
               ,con = control$path.out.chart.7
               )

    chart.8.txt <- Chart.8(control)
    writeLines( text = chart.8.txt
               ,con = control$path.out.chart.8
               )

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

    return()
    
    # example of creating a pdf
    pdf( file = my.control$path.out.chart3
        ,width = my.control$chart.width
        ,height = my.control$chart.height
        )
    print(charts$chart3)
    dev.off()

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
