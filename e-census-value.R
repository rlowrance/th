# e-census-value.R
# main program
# Determine which reduced model is best using 10-fold cross validation.
# command line
# --query INT: 1 / INT fraction of query transactions are used

source('Directory.R')
source('Libraries.R')

source('ModelLinearLocal.R')
source('Predictors.R')
source('ReadTransactionSplits.R')

library(elasticnet)
library(memoise)
library(optparse)

Control <- function(command.args) {
    # parse command line arguments in command.args
    opt <- ParseCommandArgs(command.args)

    me <- 'e-census-value' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    # define the splits that we use
    prices <- Predictors('prices')
    identification <- Predictors('identification')
    response <- 'price.log'
    testing <- FALSE
    #testing <- TRUE
    out.base <-
        sprintf( '%s--query-%d'
                ,me
                ,opt$query
                )

    always.level <- Predictors('always.level')
    always.level.no.census <- Predictors('always.level.no.census')
    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,num.training.days = 90
                    ,response = response
                    ,split.names = unique(c(always.level, prices, identification))
                    ,nfolds = 10
                    ,testing = testing
                    ,debug = FALSE
                    ,verbose.CrossValidate = TRUE
                    ,feature.set = list( always.level
                                        ,always.level.no.census
                                        )
                    ,model.name = list( sprintf( 'with census data (%d features)'
                                                ,length(always.level)
                                                )
                                       ,sprintf( 'without census data (%d features)'
                                                ,length(always.level.no.census)
                                                )
                                       )
                    ,query.fraction = (1 / opt$query)
                    )
    control
}
ParseCommandArgs <- function(command.args) {
    # return name list of values from the command args
    opt.query <- make_option( opt_str = c('--query')
                             ,action = 'store'
                             ,type = 'double'
                             ,default = .01
                             ,help = '1 / fraction of samples used as queries'
                             )
    option.list <- list( opt.query
                        )
    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
}
EvaluatePredictions <- function(prediction, actual) {
    # return list of evaluations
    # ARGS
    # prediction: vector of predictions or NA
    # actual    : vector of actual values
    #cat('start EvaluatePredictions\n'); browser()

    # most evaluations compare only where predictions are available
    is.prediction <- !is.na(prediction)
    num.predictions <- sum(is.prediction)
    price <- actual[is.prediction]
    coverage <- length(price) / length(actual)

    error <- prediction[is.prediction] - price
    error2 <- error * error
    root.mean.squared.error <- sqrt(mean(error2))
    root.median.squared.error <- sqrt(median(error2))
    abs.relative.error <- abs(error / price)
    mean.price <- mean(price)
    median.price <- median(price)

    Fraction <- function(max.abs.relative.error) {
        is.within.x.percent <- abs.relative.error <= max.abs.relative.error
        fraction.within.x.percent <- sum(is.within.x.percent) / num.predictions
    }
        
    result <- list( rootMeanSquaredError = root.mean.squared.error
                   ,rootMedianSquaredError = root.median.squared.error
                   ,coverage = coverage
                   ,fraction.within.10.percent = Fraction(.10)
                   ,fraction.within.20.percent = Fraction(.20)
                   ,fraction.within.30.percent = Fraction(.30)
                   ,mean.price = mean.price
                   ,median.price = median.price
                   )
    result
}
EvaluateFeatures <- function(features, data, is.testing, is.training, control) {
    # reduce to call to ModelLinearLocal followed by call to evaluate preditions
    # determine which transactions will be the query transactions
    #cat('EvaluateFeatures\n'); browser()


    data.training <- data[is.training,]
    InTraining <- function(saleDate) {
        # return indicated of data.training transactions within num.training.days of saleDate
        # NOTE: this function has data.training as an implicit argument!
        # the implicit data.training must be the same data frame passed to ModelLinearLocal
        #cat('InTraining\n'); browser()
        last.transaction.date <- saleDate - 1
        first.transaction.date <- last.transaction.date - control$num.training.days
        in.training <- 
            data.training$saleDate <= last.transaction.date &
            data.training$saleDate >= first.transaction.date
        in.training
    }

    # determine query transactions
    data.testing <- data[is.testing,]
    num.test.transactions <- round(nrow(data.testing) * control$query.fraction)
    which.test <- sample.int( n = nrow(data.testing)
                             ,size = num.test.transactions
                             ,replace = FALSE)
    queries <- data.testing[which.test,]
    Printf('will sample %d of %d test transactions\n', nrow(queries), nrow(data.testing))

    mll <- ModelLinearLocal( InTraining = InTraining
                            ,queries = queries
                            ,data.training = data.training
                            ,formula = Formula( response = control$response
                                               ,predictors = features
                                               )
                            )
    predictions <- exp(mll$predictions)    # possible NA
    num.training <- mll$num.training       # probably not used
    
    actuals <- queries$price
    evaluate <- EvaluatePredictions( prediction = predictions
                         ,actual = actuals)
    evaluate
}
Main <- function(control, transaction.data.all.years) {
    #cat('Main\n'); browser()
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)


    # discard all data before 2003
    jan.1.2003 <- as.Date('2003-01-01')
    is.in.2003.or.later <- transaction.data.all.years$saleDate >= jan.1.2003
    transaction.data <- transaction.data.all.years[is.in.2003.or.later,]
    
    num.models <- length(control$feature.set)
    EvaluateModel <- sapply(1:num.models
                            ,function(n) {
                                # return function that implements CrossValidation API
                                force(n)
                                function(data, is.testing, is.training, control)
                                    EvaluateFeatures( control$feature.set[[n]]
                                                     ,data
                                                     ,is.testing
                                                     ,is.training
                                                     ,control)
                            }
                            )
    model.name <- control$model.name

    model.name <- sapply( 1:num.models
                         ,function(n)
                             sprintf( '%s using %d features'
                                     ,if (n <=5 )  'LCV' else 'PCA'
                                     ,length(control$feature.set[[n]])
                                     )
                         )

    clock <- Clock()
    cv.result <- CrossValidate( data = transaction.data
                               ,nfolds = control$nfolds
                               ,EvaluateModel = EvaluateModel
                               ,model.name = model.name
                               ,control = control
                               ,verbose = control$verbose.CrossValidate
                               )
    Printf( 'cross validate for query.fraction %f took %f CPU seconds'
           ,control$query.fraction
           ,clock$Cpu()
           )

    save( control
         ,cv.result
         ,model.name
         ,file = control$path.out.rdata
         )

    str(control)
    if (control$testing)
        cat('TESTING: DISCARD RESULTS\n')
    
}

#debug(Control)
default.args <- NULL  # synthesize the command line that will be used in the Makefile
default.args <- list('--query', '100')

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
