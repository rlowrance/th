# e-cv-chart.R
# main program
# Produce charts using input files e-cv_SCOPE_MODEL_TIMEPERIOD_SCENARIO_RESPONSE_PREDICTORSFORM_PREDICTORS_NAME_NDAYS_QUERY_C_NTREE_MTRY.RData
# output files have these names
# e-cv-chart_SOMETHING.SUFFIX
#
# Command line arguments: None

source('Directory.R')
source('Libraries.R')

source('CrossValidateCharts.R')
source('CVApplyAllPossibilities.R')
source('CvPossible.R')

library(ggplot2)
library(optparse)

Control <- function(default.args) {
    # parse command line arguments in command.args
#    opt <- ParseCommandArgs( command.args = commandArgs(trailingOnly = TRUE)
#                            ,default.args
#                            )

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
                    ,path.out.chart1.content = 
                        paste0(working, out.base, '_chart1_global_linear_2009_always.txt')
                    ,path.out.chart1.dependencies = 
                        paste0(out.base, '_chart1_dependencies.makefile')
                    ,chart.width = 14  # inches
                    ,chart.height = 10 # inches
                    ,working = working
                    ,testing = FALSE
                    ,debug = FALSE
                    ,possible = CvPossible()
                    )
    control
}
ParseCommandArgs <- function(command.args, default.args) {
    # return name list of values from the command args
    opt.query <- make_option( opt_str = c('--query')
                             ,action = 'store'
                             ,type = 'double'
                             ,default = default.args$query
                             ,help = 'fraction of samples used as queries'
                             )
    option.list <- list( opt.query
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
    nfolds <- length(a.cv.result)  # this is where something is wrong; examine the cv.result structure to fix
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
ReadAndCalculate <- function() {
    # keep track of input files read and perform Median RMSE calculations
    file.names.read <- Lines()
    
    MedianRootMedianSE <- function(path.in.base, scenario, response, predictorsForm, ndays) {
        path.in <- paste0( path.in.base
                          ,'_global'
                          ,'_linear'
                          ,'_2009'
                          ,'_', scenario
                          ,'_', response
                          ,'_always'
                          ,'_', predictorsForm
                          ,'_', ndays
                          ,'_1'
                          ,'_0'
                          ,'_0'
                          ,'_0'
                          ,'.RData'
                          )
        file.names.read$Append(path.in)
        loaded <- load(path.in)
        stopifnot(!is.null(control))
        stopifnot(!is.null(cv.result))
        stopifnot(!is.null(model.name))

        # check for just one model in file
        stopifnot(length(model.name) == 1)
        stopifnot(length(cv.result) == 1)

        median.RMSE <- MedianRMSE(cv.result[[1]])
    }

    GetFileNamesRead <- function() {
        file.names.read$Get()
    }

    list( MedianRootMedianSE = MedianRootMedianSE
         ,GetFileNamesRead = GetFileNamesRead
         )
}
MakeDependencies <- function(control, file.names.read) {
    # return makefile lines for chart1 dependencies
    # this is a dependency only rule (without a recipe)
    lines <- Lines()

    lines$Append(paste0(control$path.out.chart1.content, ' :\\'))
    for (index in 1:length(file.names.read)) {
        line <- paste0( file.names.read[[index]]
                       ,if (index == length(file.names.read)) ' ' else ' \\'
                       )
        lines$Append(line)
    }
    result <- lines$Get()
    browser()
    result
}
Chart1 <- function(my.control) {
    # produce summary txt chart for global linear 2009 always
    # Do not include the assessor scenario
    # return list $text $dependencies

    # these format lines must be edited as a group (15 columns)
    # line fields: scenario / response / predictorsForm / 12 x ndays
    case   <- '%8s %8s %5s'
    header.format <- paste0(case, paste0(rep(' %6s', 12), collapse = ''))
    data.format   <- paste0(case, paste0(rep(' %6.0f', 12), collapse = ''))

    # accumulate into this object
    lines <- Lines()

    TableHeader <- function() {
        lines$Append('Median of Root Median Squared Errors from Folds')
        lines$Append('For global linear model')
        lines$Append('Data from late 2008 and 2009')
        lines$Append('Features Present in Every Transaction')
        lines$Append(' ')
    }
    DataHeaders <- function() {
        # append headers to lines
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
    }
    browser()
    read.and.calculate <- ReadAndCalculate()
    DataRecord <- function(scenario, response, predictorsForm) {
        # return one data record
        MedianRMSE <-function(ndays) {
            read.and.calculate$MedianRootMedianSE( control$path.in.base
                                                  ,scenario
                                                  ,response
                                                  ,predictorsForm
                                                  ,ndays
                                                  )
        }
        line <- sprintf( data.format
                        ,scenario
                        ,response
                        ,predictorsForm
                        ,MedianRMSE(30)
                        ,MedianRMSE(60)
                        ,MedianRMSE(90)
                        ,MedianRMSE(120)
                        ,MedianRMSE(150)
                        ,MedianRMSE(180)
                        ,MedianRMSE(210)
                        ,MedianRMSE(240)
                        ,MedianRMSE(270)
                        ,MedianRMSE(300)
                        ,MedianRMSE(330)
                        ,MedianRMSE(360)
                        )
        line
    }

    # BODY STARTS HERE

    lines$Append(TableHeader())

    lines$Append(DataHeaders())

    # append data records
    for (scenario in my.control$possible$scenario) {
        if (scenario == 'avm' ||
            scenario == 'mortgage') {
            for (response in my.control$possible$response) {
                for (predictorsForm in my.control$possible$predictorsForm) {
                    cat('case', scenario, response, predictorsForm, '\n')
                    data.record <- DataRecord(scenario, response, predictorsForm)
                    lines$Append(data.record)
                }
            }
        }
    }
    
    browser()
    file.names.read <- read.and.calculate$GetFileNamesRead()
    dependencies <- MakeDependencies(control, file.names.read)

    # return the accumulated lines
    browser()
    result <- list( text = lines$Get()
                   ,dependencies = dependencies
                   )
    result
}
Charts <- function(my.control) {
    # produce all the charts
    #cat('starting Charts\n'); browser()

    chart1 <- Chart1(my.control)
    browser()
    writeLines( text = chart1$text
               ,con = my.control$path.out.chart1.content
               )
    writeLines( text = chart1$dependencies
               ,con = my.control$path.out.chart1.dependencies
               )
    return()


    # OLD BELOW ME

    # recover cetain values from the predictions
    cv.result <- NULL
    model.name <- NULL
    loaded <- load(file = my.control$path.in)
    str(loaded)  # NOTE: control has been replaced
    stopifnot(!is.null(cv.result))
    stopifnot(!is.null(model.name))

    charts <- CrossValidateCharts(control, cv.result, model.name)

    writeLines( text = charts$chart1
               ,con = my.control$path.out.chart1
               )

    writeLines( text = charts$chart2
               ,con = my.control$path.out.chart2
               )

    pdf( file = my.control$path.out.chart3
        ,width = my.control$chart.width
        ,height = my.control$chart.height
        )
    print(charts$chart3)
    dev.off()

    pdf( file = my.control$path.out.chart4
        ,width = my.control$chart.width
        ,height = my.control$chart.height
        )
    print(charts$chart4)
    dev.off()
    
    pdf( file = my.control$path.out.chart5
        ,width = my.control$chart.width
        ,height = my.control$chart.height
        )
    print(charts$chart5)
    dev.off()
    
    pdf( file = my.control$path.out.chart6
        ,width = my.control$chart.width
        ,height = my.control$chart.height
        )
    print(charts$chart6)
    dev.off()
}
Main <- function(control) {
    browser()
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    Charts(control)
    str(control)
}


### Execution starts here

default.args <- NULL  # for now, no command line

control <- Control(default.args)

Main(control)
if (control$testing)
    cat('DISCARD RESULTS: TESTING\n')
cat('done\n')
