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

library(ggplot2)
library(optparse)

Control <- function(default.args) {
    # parse command line arguments in command.args
    opt <- ParseCommandArgs( command.args = commandArgs(trailingOnly = TRUE)
                            ,default.args
                            )

    me <- 'e-cv-chart' 

    log <- Directory('log')
    working <- Directory('working')

    out.base <-
        sprintf('%s'
                ,me
                )
    in.base <- 
        sprintf('%s'
                ,'e-cv'
                )


    # for now, create only chart 3
    control <- list( path.in.base = paste0(working, in.base)
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.makefile = paste0(me, '-generated.makefile')
                    #,path.out.chart.1 = paste0(working, out.base, '_chart1.txt')
                    #,path.out.chart.2 = paste0(working, out.base, '_chart2.txt')
                    ,path.out.chart.3 = paste0(working, out.base, '_chart3.txt')
                    ,path.out.chart.4 = paste0(working, out.base, '_chart4.txt')
                    ,chart.width = 14  # inches
                    ,chart.height = 10 # inches
                    ,working = working
                    ,testing = TRUE
                    ,debug = FALSE
                    ,opt = opt
                    ,me = me
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
Lines <- function(max.size = 1000) {
    # list of lines
    lines <- rep('', max.size)
    last.index <- 0

    Append <- function(line, debug = FALSE) {
        if (debug) browser()
        last.index <<- last.index + 1
        lines[[last.index]] <<- line
    }

    Get <- function() {
        lines[1:last.index]
    }

    list( Append = Append
         ,Get = Get
         )
}
MedianRMSE <- function(a.cv.result) {
    # there is only one model in the file
    nfolds <- length(a.cv.result)
    stopifnot(nfolds == 10)
    rMedianSE.values <- sapply(1 : nfolds,
                               function(fold.index) {
                                   evaluation <- a.cv.result[[fold.index]]
                                   evaluation$rootMedianSquaredError
                               }
                               )
    result <- median(rMedianSE.values)
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
                browser()
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
        lines$Append('With and Without Tax Assessment Features')
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
        browser()
        DataRecord <- function(scenario, response, predictorsName, predictorsForm, accumulator) {
            cat('DataRecord', scenario, response, predictorsName, predictorsForm, '\n'); browser()
            M_RMSE <- function(ndays) {
                browser()
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
        lines$Append('With and Without Census Tract Features')
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
        browser()
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
MakeMakefile <- function(control) {
    # return Lines object with makefile content
    # for now, only create Chart3  

#    chart.1 <- Chart.1.2(control)
#    chart.2 <- Chart.1.2(control)
    chart.3 <- Chart.3(control)
    chart.4 <- Chart.4(control)

    AppendDependencies <- function(target.file.name, dependency.file.names, lines) {
        # append to lines
        for (dependency.file.name in dependency.file.names) {
            lines$Append(paste0( target.file.name
                                ,' += '
                                ,dependency.file.name
                                )
            )
        }
        # return nothing
    }

    lines <- Lines()
#    AppendDependencies( target.file.name = control$path.out.chart.1
#                       ,dependency.file.names = chart.1$FileDependencies('always')
#                       ,lines = lines
#                       )
#    AppendDependencies( target.file.name = control$path.out.chart.2
#                       ,dependency.file.names = chart.2$FileDependencies('alwaysNoAssessment')
#                       ,lines = lines
#                       )
    AppendDependencies( target.file.name = 'e-cv-chart_chart3-data'
                       ,dependency.file.names = chart.3$FileDependencies()
                       ,lines = lines
                       )
    AppendDependencies( target.file.name = 'e-cv-chart_chart4-data'
                       ,dependency.file.names = chart.4$FileDependencies()
                       ,lines = lines
                       )
    

    result <- lines$Get()
    result
}
Charts <- function(control) {
    # write chart files:
    # for now, only create chart3
    

#    chart1 <- Chart.1.2(control)
#    chart.1.txt <- chart1$Txt(predictorsName = 'always')
#    writeLines( text = chart.1.txt
#               ,con = control$path.out.chart.1
#               )
#
#    chart2 <- Chart.1.2(control)
#    chart.2.txt <- chart2$Txt(predictorsName = 'alwaysNoAssessment')
#    writeLines( text = chart.2.txt
#               ,con = control$path.out.chart.2
#               )

    chart3 <- Chart.3(control)
    chart.3.txt <- chart3$Txt()
    writeLines( text = chart.3.txt
               ,con = control$path.out.chart.3
               )

    chart4 <- Chart.4(control)
    chart.4.txt <- chart4$Txt()
    writeLines( text = chart.4.txt
               ,con = control$path.out.chart.4
               )

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
        makefile <- MakeMakefile(control)
        writeLines( text = makefile
                   ,con = control$path.out.makefile
                   )
    } else {
        Charts(control)
    }

    str(control)
}


### Execution starts here

default.args <- list( makefile = TRUE) 
#default.args <- list( makefile = FALSE) 

control <- Control(default.args)

Main(control)
if (control$testing)
    cat('DISCARD RESULTS: TESTING\n')
cat('done\n')
