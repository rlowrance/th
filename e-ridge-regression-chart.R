# e-ridge-regression-chart.R
# main program
# Produce charts using input file e-ridge-regression--query-99999999.RData
# output files have these names
# e-ridge-regression-chart--query-99999999_N.SUFFIX
#
# Command line arguments
# --query             : number
#                       1 / fraction of samples in test period to use

source('Directory.R')
source('Libraries.R')

source('CrossValidateCharts.R')
source('ParseCommandArgsERidgeRegression.R')

library(ggplot2)
library(optparse)

Control <- function(default.args) {
    opt <- ParseCommandArgsERidgeRegression( command.args = commandArgs(trailingOnly = TRUE)
                                            ,default.args
                                            )

    me <- 'e-ridge-regression-chart' 

    log <- Directory('log')
    working <- Directory('working')

    testing <- FALSE
    #testing <- TRUE
    out.base <-
        sprintf('%s--query-%d--lambdaSet-%s'
                ,me
                ,opt$query
                ,opt$lambdaSet
                )
    in.file <- 
        sprintf('%s--query-%d--lambdaSet-%s.RData'
                ,'e-ridge-regression'
                ,opt$query
                ,opt$lambdaSet
                )

    control <- list( path.in = paste0(working, in.file)
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.chart1 = paste0(working, out.base, '_1.txt')
                    ,path.out.chart2 = paste0(working, out.base, '_2.txt')
                    ,path.out.chart3 = paste0(working, out.base, '_3.pdf')
                    ,path.out.chart4 = paste0(working, out.base, '_4.pdf')
                    ,path.out.chart5 = paste0(working, out.base, '_5.pdf')
                    ,path.out.chart6 = paste0(working, out.base, '_6.pdf')
                    ,path.out.chart7 = paste0(working, out.base, '_7.pdf')
                    ,chart.width = 14  # inches
                    ,chart.height = 10 # inches
                    ,testing = testing
                    ,debug = FALSE
                    )
    control
}
ParseCommandArgs <- function(command.args) {
    # return name list of values from the command args
    opt.query <- make_option( opt_str = c('--query')
                             ,action = 'store'
                             ,type = 'double'
                             ,default = .01
                             ,help = 'fraction of samples used as queries'
                             )
    option.list <- list( opt.query
                        )
    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
}
Charts <- function(my.control) {
    # produce all the charts

    # recover cetain values from the predictions
    cv.result <- NULL
    model.name <- NULL
    loaded <- load(file = my.control$path.in)
    str(loaded)  # NOTE: control has been replaced
    browser()
    stopifnot(!is.null(cv.result))
    stopifnot(!is.null(model.name))

    charts <- CrossValidateCharts( control = control
                                  ,cv.result = cv.result
                                  ,model.name = model.name
                                  ,chart7 = list(lambda = control$lambda)
                                  )

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

    pdf( file = my.control$path.out.chart7
        ,width = my.control$chart.width
        ,height = my.control$chart.height
        )
    print(charts$chart7)
    dev.off()
}
Main <- function(control) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    Charts(control)

    str(control)
    if (control$testing)
        cat('TESTING: DISCARD RESULTS\n')
    
}

############### Execution Starts Here

just.testing <- FALSE
default.args <-
    if (just.testing) {
        list( query = 10000
             ,lambdaSet = 'one'
             )
    } else {
        list( query = 100
             ,lambdaSet = 'd'
             )
    }

control <- Control(default.args)

Main(control)
if (control$testing)
    cat('TESTING: DISCARD RESULT\n')
cat('done\n')
