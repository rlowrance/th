# e-city-chart.R
# main program
# Produce charts using input file e-city--query-99999999.RData
# output files have these names
# e-city-chart--query-99999999_N.SUFFIX
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
    #opt <- ParseCommandArgs(command.args)

    me <- 'e-city-chart' 

    log <- Directory('log')
    working <- Directory('working')

    testing <- FALSE
    #testing <- TRUE
    out.base <-
        sprintf('%s'
                ,me
                )
    in.file <- 
        sprintf('%s.RData'
                ,'e-city'
                )

    control <- list( path.in = paste0(working, in.file)
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.chart1 = paste0(working, out.base, '_1.txt')
                    ,path.out.chart2 = paste0(working, out.base, '_2.txt')
                    ,path.out.chart3 = paste0(working, out.base, '_3.txt')
                    ,path.out.chart4 = paste0(working, out.base, '_4.txt')
                    #,path.out.chart5 = paste0(working, out.base, '_5.txt')
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
CityName <- function(s) {
    # return suffix after remove property.city'
    x <- 'property.city'
    substr(s, 1 + nchar(x), nchar(s))
}
PropertyCity <- function(s) {
    # return TRUE iff prefix is property.city
    x <- 'property.city'
    substr(s, 1, nchar(x)) == x
}
Chart1 <- function(control, coefs) {
    # coefficient values, except for property.city
    #cat('Chart1\n'); browser()

    Header <- function() {
        c( 'SUBMARKET MODEL FOR PROPERTY.CITY'
          ,'VALUES OF NON-CITY COEFFICIENTS'
          )
    }

    Titles <- function() {
        sprintf('%30s   %10s %8s', 'feature', 'estimate', 'p-value')
    }

    Body <- function() {
        result <- NULL
        ordered <- order(coefs$value, decreasing = TRUE)  # sort indices
        for (index in ordered) {
            name <- coefs$name[[index]]
            value <- coefs$value[[index]]
            p.value <- coefs$p.value[[index]]
            if (!PropertyCity(name))
                result <- c(result, sprintf('%30s = %+10.6f %8.3f', name, value, p.value))
        }
        result
    }
    
    result <- c( Header()
                ,' '
                ,Titles()
                ,' '
                ,Body()
                )
    result
}
Chart2 <- function(control, coefs) {
    # coefficient values, except for property.city
    #cat('Chart2\n'); browser()

    Header <- function() {
        c( 'SUBMARKET MODEL FOR PROPERTY.CITY'
          ,'VALUES OF CITY COEFFICIENTS'
          ,'WHEN P-VALUE <= 0.05'
          )
    }

    Titles <- function() {
        sprintf('%30s   %10s %8s', 'feature', 'estimate', 'p-value')
    }

    Body <- function() {
        result <- NULL
        ordered <- order(coefs$value, decreasing = TRUE)  # sort indices
        for (index in ordered) {
            name <- coefs$name[[index]]
            value <- coefs$value[[index]]
            p.value <- coefs$p.value[[index]]
            if (PropertyCity(name) && (p.value <= 0.05))
                result <- c(result, sprintf('%30s = %+10.6f %8.3f', CityName(name), value, p.value))
        }
        result
    }
    
    result <- c( Header()
                ,' '
                ,Titles()
                ,' '
                ,Body()
                )
    result
}
Chart3 <- function(control, coefs) {
    # coefficient values, except for property.city
    #cat('Chart3\n'); browser()

    Header <- function() {
        c( 'SUBMARKET MODEL FOR PROPERTY.CITY'
          ,'VALUES OF CITY COEFFICIENTS'
          ,'WHEN P-VALUE <= 0.05'
          ,'20 ESTIMATED COEFFICIENTS WITH HIGHEST VALUES'
          )
    }

    Titles <- function() {
        sprintf('%30s   %10s %8s', 'feature', 'estimate', 'p-value')
    }

    Body <- function() {
        result <- NULL
        ordered <- order(coefs$value, decreasing = TRUE)  # sort indices
        for (n in 1:20) {
            index <- ordered[[n]]
            name <- coefs$name[[index]]
            value <- coefs$value[[index]]
            p.value <- coefs$p.value[[index]]
            if (PropertyCity(name) && (p.value <= 0.05))
                result <- c(result, sprintf('%30s = %+10.6f %8.3f', CityName(name), value, p.value))
        }
        result
    }
    
    result <- c( Header()
                ,' '
                ,Titles()
                ,' '
                ,Body()
                )
    #cat('review result\n'); browser()
    result
}
Chart4 <- function(control, coefs) {
    # coefficient values, except for property.city
    #cat('Chart4\n'); browser()

    Header <- function() {
        c( 'SUBMARKET MODEL FOR PROPERTY.CITY'
          ,'VALUES OF CITY COEFFICIENTS'
          ,'WHEN P-VALUE <= 0.05'
          ,'20 ESTIMATED COEFFICIENTS WITH LOWEST VALUES'
          )
    }

    Titles <- function() {
        sprintf('%30s   %10s %8s', 'feature', 'estimate', 'p-value')
    }

    Body <- function() {
        result <- NULL
        ordered <- order(coefs$value, decreasing = TRUE)  # sort indices
        for (n in 1:20) {
            index <- ordered[[length(ordered) - n + 1]]
            name <- coefs$name[[index]]
            value <- coefs$value[[index]]
            p.value <- coefs$p.value[[index]]
            if (PropertyCity(name) && (p.value <= 0.05))
                result <- c(result, sprintf('%30s = %+10.6f %8.3f', CityName(name), value, p.value))
        }
        result
    }
    
    result <- c( Header()
                ,' '
                ,Titles()
                ,' '
                ,Body()
                )
    #cat('review result\n'); browser()
    result
}
Charts <- function(my.control, control, fitted, summary.fitted) {
    # produce all the charts
    #cat('starting Charts\n'); browser()

    # extract coefficient names, values, and p-values
    c <- summary.fitted$coefficients
    coefs <- list( name = rownames(c)
                  ,value = c[, 'Estimate']
                  ,p.value = c[,'Pr(>|t|)']
                  )

    # build and write the charts


    writeLines( text = Chart1(control, coefs)
               ,con = my.control$path.out.chart1
               )

    writeLines( text = Chart2(control, coefs)
               ,con = my.control$path.out.chart2
               )

    writeLines( text = Chart3(control, coefs)
               ,con = my.control$path.out.chart3
               )

    writeLines( text = Chart4(control, coefs)
               ,con = my.control$path.out.chart4
               )
}
Main <- function(my.control, control, fitted, summary.fitted) {
    InitializeR(duplex.output.to = my.control$path.out.log)
    str(my.control)

    Charts(my.control, control, fitted, summary.fitted)

    str(my.control)
    if (my.control$testing)
        cat('TESTING: DISCARD RESULTS\n')
    
}

#debug(Control)
default.args <- NULL  # synthesize the command line that will be used in the Makefile
#default.args <- list('--query', '100')

command.args <- if (is.null(default.args)) commandArgs(trailingOnly = TRUE) else default.args
my.control <- Control(command.args)

if(!exists('fitted') || !exists('summary.fitted')) {
    loaded <- load(file = my.control$path.in)
    stopifnot(!is.null(control))
    stopifnot(!is.null(fitted))
    stopifnot(!is.null(summary.fitted))
}

Main(my.control, control, fitted, summary.fitted)
cat('done\n')
