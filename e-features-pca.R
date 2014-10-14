# e-features.R
# main program
# determine best features to use using PCA.
#
# Command line arguments
# -- predictors       : one of {chopra, all, always}
#                       chopra == 11 predictors based on Chopa's work
#                       all    == every predictors for which we have splits
#                       always == 25 predictors that are always present in every observation

source('Directory.R')
source('Libraries.R')

source('Predictors.R')
source('ReadTransactionSplits.R')

library(memoise)
library(optparse)

Control <- function(command.args) {
    # parse command line arguments in command.args
    opt <- ParseCommandArgs(command.args)

    me <- 'e-features-pca' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    # defines the splits that we use
    predictors <- switch( opt$predictors
                         ,chopra = Predictors('chopra.level')
                         ,all = Predictors('all.level')
                         ,always = Predictors('always.level')
                         ,stop('bad opt$predictors')
                         )
    other.names <- c(# dates
                    'saleDate'
                    ,'recordingDate'
                    # prices
                    ,'price'   # NOTE: MUST HAVE THE PRICE
                    ,'price.log'
                    # apn
                    ,'apn'
                    )
    response <- 'price.log'
    formula <- Formula( predictors = predictors
                       ,response = response
                       )
    testing <- FALSE
    #testing <- TRUE
    out.base <-
        sprintf('%s--predictors-%s'
                ,me
                ,opt$predictors
                )

    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,query.fraction = opt$query.fraction
                    ,num.training.days = 90
                    ,predictors = predictors
                    ,response = response
                    ,formula = formula
                    ,split.names = unique(c(predictors, other.names))
                    ,nfolds = 10
                    ,testing.period = list( first.date = as.Date('1984-02-01')
                                           ,last.date = as.Date('2009-03-31')
                                           )
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
    option.list <- list( opt.predictors
                        )
    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
}
PCAAnalysis <- function(control, data) {
    # use principle components analysis to determine most important features
    # ref: R Cookbook p. 338
    formula <- Formula( response = NULL
                       ,predictors = control$predictors
                       )
    # recode variables that are TRUE/FALSE to numeric
    if ('census.tract.has.industry' %in% control$predictors)
        data$census.tract.has.industry <- as.numeric(data$census.tract.has.industry)
    if ('census.tract.has.park' %in% control$predictors)
        data$census.tract.has.park <- as.numeric(data$census.tract.has.park)
    if ('census.tract.has.retail' %in% control$predictors)
        data$census.tract.has.retail <- as.numeric(data$census.tract.has.retail)
    if ('census.tract.has.school' %in% control$predictors)
        data$census.tract.has.school <- as.numeric(data$census.tract.has.school)

    if ('factor.is.new.construction' %in% control$predictors)
        data$factor.is.new.construction <- as.numeric(data$factor.is.new.construction)
    if ('factor.has.pool' %in% control$predictors)
        data$factor.has.pool <- as.numeric(data$factor.has.pool)

    if ('zip5.has.industry' %in% control$predictors)
        data$zip5.has.industry <- as.numeric(data$zip5.has.industry)
    if ('zip5.has.park' %in% control$predictors)
        data$zip5.has.park <- as.numeric(data$zip5.has.park)
    if ('zip5.has.retail' %in% control$predictors)
        data$zip5.has.retail <- as.numeric(data$zip5.has.retail)
    if ('zip5.has.school' %in% control$predictors)
        data$zip5.has.school <- as.numeric(data$zip5.has.school)
    
    prcomp.result <- prcomp( formula = formula
                            ,data = data
                            )
    print(prcomp.result)
    print(summary(prcomp.result))

    save( control
         ,prcomp.result
         ,file = control$path.out.rdata
         )
}
Main <- function(control, transaction.data) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    PCAAnalysis(control, transaction.data)

    str(control)
    if (control$testing)
        cat('TESTING: DISCARD RESULTS\n')
    
}

#debug(Control)
default.args <- NULL  # synthesize the command line that will be used in the Makefile
default.args <- list('--predictors', 'always')

command.args <- if (is.null(default.args)) commandArgs(trailingOnly = TRUE) else default.args
control <- Control(command.args)


# cache transaction.data
if (!exists('transaction.data')) {
    transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                              ,split.names = control$split.names
                                              )
}

Main(control, transaction.data)
cat('done\n')
