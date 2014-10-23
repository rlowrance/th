# e-reduced-features-chart.R
# main program
# Produce charts using as input file e-reduced-features--predictors-PREDICTORS--query-fraction-99999999.RData
# output files have these names
# e-reduced-features-chart.N.SUFFIX
#
# Command line arguments
# --query             : number
#                       1 / fraction of samples in test period to use

source('Directory.R')
source('Libraries.R')

source('CrossValidateCharts.R')

library(ggplot2)
library(optparse)

Control <- function(command.args) {
    # parse command line arguments in command.args
    opt <- ParseCommandArgs(command.args)

    me <- 'e-reduced-features-chart' 

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
                ,'e-reduced-features'
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
    # median of RMedianSE
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
    MedianRMedianSE <- function(model.index) {
        values <- ResultsForModel(model.index)
        values.median <- median(values)
        values.median
    }
    SE <- function(model.index) {
        # standard error of the mean
        values <- ResultsForModel(model.index)
        values.sd <- sd(values)
        values.se <- values.sd / length(values)
        values.se
    }

    model.index <- 1:length(cv.result)
    medianRMedianSE <- sapply(1:length(cv.result), MedianRMedianSE)
    se <- sapply(1:length(cv.result), SE)

    df2 <- data.frame( model.index = model.index
                      ,medianRMedianSE = medianRMedianSE
                      ,se = se
                      )
    

    gg <- ggplot( df2
                 ,aes( x = model.index
                      ,y = medianRMedianSE
                      )
                 )
    g <- 
        gg + 
        geom_line(aes(group = 1)) + 
        geom_point(size = 4) + 
        geom_errorbar( aes( ymin = medianRMedianSE - se
                           ,ymax = medianRMedianSE + se
                           )
                      ,width = 0.2
                      )
        

        
    g
}
Chart5 <- function(control, cv.result, ordered.features) {
    # mean of rootMedianSquaredError
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
    cat('starting Charts\n'); browser()

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
        ,width = control$chart.width
        ,height = control$chart.height
        )
    print(charts$chart3)
    dev.off()

    pdf( file = my.control$path.out.chart4
        ,width = control$chart.width
        ,height = control$chart.height
        )
    print(charts$chart4)
    dev.off()
    
    pdf( file = my.control$path.out.chart5
        ,width = control$chart.width
        ,height = control$chart.height
        )
    print(charts$chart5)
    dev.off()

    pdf( file = my.control$path.out.chart6
        ,width = control$chart.width
        ,height = control$chart.height
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

#debug(Control)
default.args <- NULL  # synthesize the command line that will be used in the Makefile
#default.args <- list('--query', '100')

command.args <- if (is.null(default.args)) commandArgs(trailingOnly = TRUE) else default.args
control <- Control(command.args)

#debug(Main)
Main(control)
cat('done\n')
