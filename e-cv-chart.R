# e-cv-chart.R
# main program
# Produce charts using input files e-cv_SCOPE_MODEL_TIMEPERIOD_SCENARIO_RESPONSE_PREDICTORSFORM_PREDICTORS_NAME_NDAYS_QUERY_C_NTREE_MTRY.RData
# output files have these names
# WORKING/e-cv-chart_SOMETHING.SUFFIX
# e-cv-chart.makefile
#   describes all file dependencies for each chart
#
# Command line arguments:
# --makefile: FLAG, if present only create file e-cv-chart.makefile

source('Directory.R')
source('Libraries.R')

source('CrossValidateCharts.R')
source('CVApplyAllPossibilities.R')
source('CvPossible.R')

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


    control <- list( path.in.base = paste0(working, in.base)
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.makefile = paste0(me, '-generated.makefile')
                    ,path.out.chart.1 = paste0(working, out.base, '_chart1.txt')
                    ,path.out.chart.2 = paste0(working, out.base, '_chart2.txt')
                    ,chart.width = 14  # inches
                    ,chart.height = 10 # inches
                    ,working = working
                    ,testing = TRUE
                    ,debug = FALSE
                    ,possible = CvPossible()
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
Chart.1.2 <- function(my.control, predictorsName) {
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

    PathIn <- function(scenario, response, predictorsForm, ndays) {
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

    FileDependencies <- function() {
        files <- Lines()
        for (scenario in my.control$possible$scenario) {
            if (scenario == 'avm' ||
                scenario == 'mortgage') {
                for (response in my.control$possible$response) {
                    for (predictorsForm in my.control$possible$predictorsForm) {
                        for (ndays in ndays.range) {
                            path.in <- PathIn( scenario = scenario
                                              ,response = response
                                              ,predictorsForm = predictorsForm
                                              ,ndays = ndays
                                              )
                            files$Append(path.in)
                        }
                    }
                }
            }
        }
        result <- files$Get()
        result
    }

    DataRecords <- function() {
        data.records <- Lines()
        for (scenario in my.control$possible$scenario) {
            if (scenario == 'avm' ||
                scenario == 'mortgage') {
                for (response in my.control$possible$response) {
                    for (predictorsForm in my.control$possible$predictorsForm) {
                        data.records$Append(DataRecord(scenario, response, predictorsForm))
                    }
                }
            }
        }
        result <- data.records$Get()
        result
    }

    DataRecord <- function(scenario, response, predictorsForm) {
        M_RMSE <- function(ndays) {
            path.in <- PathIn( scenario = scenario
                              ,response = response
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
        line
    }

    HeaderRecords <- function() {
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
MakeMakefile <- function(control) {
    # return Lines object with makefile content

    chart1 <- Chart.1.2.New(control, 'always')
    chart2 <- Chart.1.2.New(control, 'alwaysNoAssessment')

    AppendDependencies <- function(target.file.name, dependency.file.names, lines) {
        # append to lines
        lines$Append(paste0(target.file.name, ': \\'))
        last.index <- length(dependency.file.names)
        for (index in 1:last.index) {
            dependency.file.name <- dependency.file.names[[index]]
            lines$Append(paste0( ' '
                                ,dependency.file.name
                                ,if (index == last.index) ' ' else ' \\'
                                )
            )
        }
        result <- lines
        result
    }

    lines.1 <- AppendDependencies( target.file.name = control$path.out.chart1
                                  ,dependency.file.names = chart1$FileDependencies()
                                  ,lines = Lines()
                                  )
    lines.2 <- AppendDependencies( target.file.name = control$path.out.chart2
                                  ,dependency.file.names = chart2$FileDependencies()
                                  ,lines = lines.1
                                  )
    result <- lines.2$Get()
    result
}
Charts <- function(control) {
    browser()
    # write chart files:
    # WORKING/e-cv-chart_chart1.txt
    # WORKING/e-cv-chart_chart2.txt
    
    chart1 <- Chart.1.2(control, 'always')
    chart2 <- Chart.1.2(control, 'alwaysNoAssessment')

    chart.1.txt <- chart1$Txt()
    writeLines( text = chart.1.txt
               ,con = control$path.out.chart.1
               )

    chart.2.txt <- chart2$Txt()
    writeLines( text = chart.2.txt
               ,con = control$path.out.chart.2
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
    browser()
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

default.args <- list( makefile = FALSE) 

control <- Control(default.args)

Main(control)
if (control$testing)
    cat('DISCARD RESULTS: TESTING\n')
cat('done\n')
