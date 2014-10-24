# e-submarkets-chart.R
# main program
# Produce charts using input file e-submarkets--query-99999999.RData
# output files have these names
# e-submarkets-chart--query-99999999_N.SUFFIX
#
# Command line arguments
# --query             : number
#                       1 / fraction of samples in test period to use

source('Directory.R')
source('Libraries.R')

source('CrossValidateCharts.R')

library(ggplot2)
library(optparse)

Control <- function(default.args) {
    # parse command line arguments in command.args
    opt <- ParseCommandArgs( command.args = commandArgs(trailingOnly = TRUE)
                            ,default.args
                            )

    me <- 'e-submarkets-chart' 

    log <- Directory('log')
    working <- Directory('working')

    testing <- FALSE
    #testing <- TRUE
    out.base <-
        sprintf('%s--query-%d'
                ,me
                ,opt$query
                )
    in.file <- 
        sprintf('%s--query-%d.RData'
                ,'e-submarkets'
                ,opt$query
                )

    control <- list( path.in = paste0(working, in.file)
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.chart1 = paste0(working, out.base, '_1.txt')
                    ,path.out.chart2 = paste0(working, out.base, '_2.txt')
                    ,path.out.chart3 = paste0(working, out.base, '_3.pdf')
                    ,path.out.chart4 = paste0(working, out.base, '_4.pdf')
                    ,path.out.chart5 = paste0(working, out.base, '_5.pdf')
                    ,path.out.chart6 = paste0(working, out.base, '_6.pdf')
                    ,chart.width = 14  # inches
                    ,chart.height = 10 # inches
                    ,testing = testing
                    ,debug = FALSE
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
Charts <- function(my.control) {
    # produce all the charts
    #cat('starting Charts\n'); browser()

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
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    Charts(control)

    str(control)
    if (control$testing)
        cat('TESTING: DISCARD RESULTS\n')
    
}


### Execution starts here

default.args <- list(query = 100)

control <- Control(default.args)

Main(control)
cat('done\n')
