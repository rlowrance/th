# e-features-lcv-chart.R
# main program
# Produce charts using as input file e-features-lcv--predictors-PREDICTORS--query-fraction-99999999.RData
# output files have these names
# e-features-lcv-chart.N.SUFFIX
#
# Command line arguments
# -- predictors       : one of {chopra, all, always}
#                       chopra == 11 predictors based on Chopa's work
#                       all    == every predictors for which we have splits
#                                 this fails if approach == lcv
#                       always == 25 predictors that are always present in every observation
# --query.fraction    : number
#                       fraction of samples in test period to use

source('Directory.R')
source('Libraries.R')

source('ModelLinearLocal.R')
source('Predictors.R')
source('ReadTransactionSplits.R')

library(ggplot2)
library(optparse)

Control <- function(command.args) {
    # parse command line arguments in command.args
    opt <- ParseCommandArgs(command.args)

    me <- 'e-features-lcv-chart' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    testing <- FALSE
    #testing <- TRUE
    out.base <-
        sprintf('%s--predictors-%s--query.fraction-%f'
                ,me
                ,opt$predictors
                ,opt$query.fraction
                )
    in.file <- 
        sprintf('%s--predictors-%s--query.fraction-%f.RData'
                ,'e-features-lcv'
                ,opt$predictors
                ,opt$query.fraction
                )

    control <- list( path.in = paste0(working, in.file)
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.chart1 = paste0(working, out.base, '.1.txt')
                    ,path.out.chart2 = paste0(working, out.base, '.2.txt')
                    ,path.out.chart3 = paste0(working, out.base, '.3.pdf')
                    ,path.out.chart4 = paste0(working, out.base, '.4.pdf')
                    ,chart.width = 14  # inches
                    ,chart.height = 10 # inches
                    ,testing = testing
                    ,debug = FALSE
                    )
    control
}
ParseCommandArgs <- function(command.args) {
    # return name list of values from the command args
    opt.predictors <- make_option( opt_str = c('--predictors')
                                  ,action = 'store'
                                  ,type = 'character'
                                  ,help = 'name of feature set to use'
                                  )
    opt.query.fraction <- make_option( opt_str = c('--query.fraction')
                                      ,action = 'store'
                                      ,type = 'double'
                                      ,default = .01
                                      ,help = 'fraction of samples used as queries'
                                      )
    option.list <- list( opt.predictors
                        ,opt.query.fraction
                        )
    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
}
Heading.1.and.2 <- function(control) {
    result <- c( sprintf( 'cv.result of %f Percent Random Sample of Training Transactions'
                        ,control$query.sample * 100
                        )
                ,'AVM scenario'
                ,'Log-linear form'
                ,sprintf( 'Testing period: %s through %s'
                         ,control$testing.period$first.date
                         ,control$testing.period$last.date
                         )
                ,sprintf( 'Number of training days: %d'
                         ,control$num.training.days
                         )
                )
    result
}
Chart1 <- function(control, cv.result, ordered.features) {
    # text file with variables in order
    Body <- function() {
        # return a vector lines, the body of chart 1
        FeatureNames <- function() {
            c( 'Feature names ordered by importance'
              ,'1 == most important'
              ,sapply( 1:length(ordered.features)
                      ,function(index) 
                          sprintf(' %2d: %s'
                                  ,index
                                  ,ordered.features[[index]]
                                  )
                      )
              )
        }
        result <- FeatureNames()
        result
    }
    result <- c( Heading.1.and.2(control)
                ,' '
                ,Body()
                )
    result
}
Chart2 <- function(control, cv.result, ordered.features) {
    # text file with median of rootMedianSquaredErrors
    Body <- function() {
        RMedianSE <- function(model.index) {
            model.result <- cv.result[[model.index]]
            rMedianSE.values <- sapply( 1:length(model.result)
                                       ,function(fold.index) {
                                           evaluate <- model.result[[fold.index]]
                                           evaluate$rootMedianSquaredError
                                       }
                                       )
            result <- median(rMedianSE.values)
            result
        }
        result <- sapply( 1:length(cv.result)
                         ,function(model.index) {
                             sprintf( ' RMedianSE for features 1 through %2d = %f'
                                     ,model.index
                                     ,RMedianSE(model.index)
                                     )
                         }
                         )
        result
    }
    result <- c( Heading.1.and.2(control)
                ,' '
                ,Body()
                )
    result
}
Chart3 <- function(control, cv.result, ordered.features) {
    # box chart showing all rMedianSE values in each fold
    df <- NULL
    for (cv.result.index in 1:length(cv.result)) {
        one.cv <- cv.result[[cv.result.index]]
        for (fold.index in 1:length(one.cv)) {
            next.df <- data.frame( num.vars = cv.result.index
                                  ,rootMedianSquaredError = one.cv[[fold.index]]$rootMedianSquaredError
                                  )
            df <- rbind(df, next.df)
        }
    }
    df$num.vars <- factor(df$num.vars)
    gg <- ggplot( df
                 ,aes( x = num.vars
                      ,y = rootMedianSquaredError
                      )
                )
    g <- gg + geom_boxplot()
    g
}
Chart4 <- function(control, cv.result, ordered.features) {
    # line graph showing rMedianSE values in each fold with error bars
    # ref: R Graphics Cookbook p158 (adding error bars to line graph)
    # ref: R Graphics Cookbook, recipe 15.18 (calculated standard errors)
    # NOTE: se is the standard deviation / sqrt(sample size)
    # NOTE: a 95% confidence interval for the mean is +- 1.96 * se
    df <- NULL
    for (cv.result.index in 1:length(cv.result)) {
        one.cv <- cv.result[[cv.result.index]]
        for (fold.index in 1:length(one.cv)) {
            next.df <- data.frame( num.vars = cv.result.index
                                  ,rootMedianSquaredError = one.cv[[fold.index]]$rootMedianSquaredError
                                  )
            df <- rbind(df, next.df)
        }
    }

    # compute median results and standard errors
    ResultsForModel <- function(model.index) {
        values <- df[df$num.vars == model.index, 'rootMedianSquaredError']
        values
    }
    MeanRMedianSE <- function(model.index) {
        values <- ResultsForModel(model.index)
        values.mean <- mean(values)
        values.mean
    }
    SE <- function(model.index) {
        # standard error of the mean
        values <- ResultsForModel(model.index)
        values.sd <- sd(values)
        values.se <- values.sd / length(values)
        values.se
    }

    browser()
    model.index <- 1:length(cv.result)
    meanRMedianSE <- sapply(1:length(cv.result), MeanRMedianSE)
    se <- sapply(1:length(cv.result), SE)

    df2 <- data.frame( model.index = model.index
                      ,meanRMedianSE = meanRMedianSE
                      ,se = se
                      )
    

    gg <- ggplot( df2
                 ,aes( x = model.index
                      ,y = meanRMedianSE
                      )
                 )
    g <- 
        gg + 
        geom_line(aes(group = 1)) + 
        geom_point(size = 4) + 
        geom_errorbar( aes( ymin = meanRMedianSE - se
                           ,ymax = meanRMedianSE + se
                           )
                      ,width = 0.2
                      )
        

        
    g
}
Charts <- function(my.control) {
    # produce all the charts
    #cat('starting Charts\n'); browser()

    # recover cetain values from the predictions
    cv.result <- NULL
    loaded <- load(file = my.control$path.in)
    str(loaded)  # NOTE: control has been replaced
    stopifnot(!is.null(cv.result))
    stopifnot(!is.null(ordered.features))

    debug(Chart4)

    chart1 <- Chart1(control, cv.result, ordered.features)
    writeLines( text = chart1
               ,con = my.control$path.out.chart1
               )

    chart2 <- Chart2(control, cv.result, ordered.features)
    writeLines( text = chart2
               ,con = my.control$path.out.chart2
               )

    chart3 <- Chart3(control, cv.result, ordered.features)
    pdf( file = my.control$path.out.chart3
        ,width = control$chart.width
        ,height = control$chart.height
        )
    print(chart3)
    dev.off()

    chart4 <- Chart4(control, cv.result, ordered.features)
    pdf( file = my.control$path.out.chart4
        ,width = control$chart.width
        ,height = control$chart.height
        )
    print(chart4)
    dev.off()
}
Main <- function(control) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    debug(LCVCharts)
    Charts(control)

    str(control)
    if (control$testing)
        cat('TESTING: DISCARD RESULTS\n')
    
}

#debug(Control)
default.args <- NULL  # synthesize the command line that will be used in the Makefile
default.args <- list('--predictors', 'chopra', '--query.fraction', '.0001')

command.args <- if (is.null(default.args)) commandArgs(trailingOnly = TRUE) else default.args
control <- Control(command.args)

debug(Main)
Main(control)
cat('done\n')
