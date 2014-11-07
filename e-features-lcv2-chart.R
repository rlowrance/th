# e-features-lcv2-chart.R
# main program
# Produce charts using as input file e-features-lcv--predictors-PREDICTORS--query-fraction-99999999.RData
# output files have these names
# e-feature-lcv2-chart.N.SUFFIX
#
# Command line arguments
# --query             : number
#                       1 / fraction of samples in test period to use

source('Directory.R')
source('Libraries.R')

source('Lines.R')

library(ggplot2)
library(optparse)

Control <- function(default.args) {
    # parse command line arguments in command.args
    stopifnot(is.null(default.args))

    me <- 'e-features-lcv2-chart' 

    log <- Directory('log')
    working <- Directory('working')

    control <- list( path.in = paste0(working, 'e-features-lcv2.txt')
                    ,path.out.log = paste0(log, me, '.log')
                    ,path.out.chart1 = paste0(working, me, '_1.txt')
                    ,testing = FALSE
                    ,debug = FALSE
                    )
    control
}
Chart1.Table <- function(lines) {
  # function object
  format.header <- '%20s %30s %30s'
  data.header   <- '%20d %30s %30s'
  Header <- function(rank, location, house) {
    line <- sprintf( format.header
                    ,rank
                    ,house
                    ,location
                    )
    lines$Append(line)
  }
  Detail <- function(rank, location = ' ', house = ' ') {
    line <- sprintf( data.header
                    ,rank
                    ,house
                    ,location
                    )
    lines$Append(line)
  }
  Get <- function() {
    lines$Get()
  }
  list( Header = Header
       ,Detail = Detail
       ,Get    = Get
       )
}
Chart1 <- function(control, input) {
  # return lines for chart 1, a txt file
  IsLocationFeature <- function(feature) {
    result <- 
      feature == 'median.household.income' |
      feature == 'avg.commute.time' |
      feature == 'fraction.owner.occupied' |
      feature == 'zip5.has.industry' |
      feature == 'census.tract.has.industry' |
      feature == 'census.tract.has.retail' |
      feature == 'zip5.has.school' |
      feature == 'census.tract.has.school' |
      feature == 'census.tract.has.park' |
      feature == 'zip5.has.park' |
      feature == 'zip5.has.retail'
    result
  }
  Header <- function(lines) {
    lines$Append('Most important features')
    lines$Append('As found through L1 regularization')
  }
  Body <- function(lines) {
    table <- Chart1.Table(lines)
    table$Header( rank = 'importance rank'
                 ,location = 'location feature'
                 ,house = 'house feature'
                 )

                 c
    for (feature.index in 1:length(input)) {
      feature <- input[[feature.index]]
      if (IsLocationFeature(feature)) {
        table$Detail( rank = feature.index
                     ,location = feature
                     )
      } else {
        table$Detail( rank = feature.index
                     ,house = feature
                     )
      }
    }
  }
  
  lines <- Lines()
  Header(lines)
  lines$Append(' ')
  Body(lines)
  result <- lines$Get()
  result
}
Charts <- function(control, input) {
    # produce all the charts
    #cat('starting Charts\n'); browser()
   
    lines <- Chart1(control, input)
    writeLines( text = lines
               ,con = control$path.out.chart1
               )
}
Main <- function(control) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)
    
    input <- readLines(con = control$path.in)

    Charts(control, input)
}

default.args <- NULL
control <- Control(default.args)

Main(control)
if (control$testing)
    cat('DISCARD RESULTS: TESTING')
cat('done\n')
