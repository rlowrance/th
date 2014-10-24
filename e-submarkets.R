# e-submarkets.R
# main program
# compare OLS for entire market to 2 variants of OLS for submarket
# variant 1: there is a census tract indicator
# variant 2: there is a model for every census tract
#  NOTE: variant 2 fails, because the training data sets are too small
#        having few observations than features
#        For now, this program implements only variant 1.
#
# command line
# --query INT: 1 / INT fraction of query transactions are used

source('Directory.R')
source('Libraries.R')

source('After2002.R')
source('ModelLinearLocal.R')
source('ModelLinearSubmarketLocal.R')
source('Predictors.R')
source('PredictorsBest.R')
source('ReadTransactionSplits.R')

library(memoise)
library(optparse)

Control <- function(default.args) {
    # parse command line arguments in command.args
    opt <- ParseCommandArgs( command.args = commandArgs(trailingOnly = TRUE)
                            ,default.args = default.args
                            )

    me <- 'e-submarkets' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    # define the splits that we use
    predictors <- PredictorsBest()
    identification <- Predictors('identification')
    prices <- Predictors('prices')

    submarket.feature.names <- c( 'census.tract'
                                 ,'property.city'
                                 ,'zip5'
                                 )
    response <- 'price.log'

    testing <- FALSE
    #testing <- TRUE

    out.base <-
        sprintf( '%s--query-%d'
                ,me
                ,opt$query
                )

    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,num.training.days = 90
                    ,response = response
                    ,predictors = predictors
                    ,split.names = unique(c( predictors
                                            ,identification
                                            ,prices
                                            ,submarket.feature.names
                                            )
                    )
                    ,nfolds = if (testing) 2 else 10
                    ,testing = testing
                    ,debug = FALSE
                    ,verbose.CrossValidate = TRUE
                    ,submarket.feature.names = submarket.feature.names
                    ,query.fraction = (1 / opt$query)
                    )
    control
}
ParseCommandArgs <- function(command.args, default.args) {
    # return name list of values from the command args
    opt.query <- make_option( opt_str = c('--query')
                             ,action = 'store'
                             ,type = 'double'
                             ,default = default.args$query
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
EvaluateModelHp <- function(hp, data, is.testing, is.training, control) {
    # evaluate model with specified hyperparameters hp on one fold of data
    
    # reduce to call to ModelLinearLocal followed by call to evaluate preditions
    # determine which transactions will be the query transactions

    #cat('EvaluateModelHp\n'); print(hp); browser()

    data.training <- data[is.training,]
    InTraining <- function(saleDate) {
        # return indicated of data.training transactions within num.training.days of saleDate
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

    # 3 models
    NoSubmarket <- function() {
        ModelLinearLocal( InTraining = InTraining
                         ,queries = queries
                         ,data.training = data.training
                         ,formula = Formula( response = control$response
                                            ,predictors = control$predictors
                                            )
                         )
    }
    SubmarketIndicator <- function(submarket.feature.name) {
        #cat('SubmarketIndicator', submarket.feature.name, '\n'); browser()
        ModelLinearLocal( InTraining = InTraining
                         ,queries = queries
                         ,data.training = data.training
                         ,formula = Formula( response = control$response
                                            ,predictors = c( control$predictors
                                                            ,submarket.feature.name
                                                            )
                                            )
                         )
    }
    SubmarketModel <-function(submarket.feature.name) {
        #cat('SubmarketModel', submarket.feature.name, '\n'); browser()
        ModelLinearSubmarketLocal( InTraining = InTraining
                                  ,queries = queries
                                  ,data.training = data.training
                                  ,formula = Formula( response = control$response
                                                     ,predictors = control$predictors
                                                     )
                                  ,submarket.feature.name = submarket.feature.name
                                  )
    }

    mll <-
        if (hp$use.submarket) {
            if (hp$use.indicator)
                SubmarketIndicator(hp$submarket.feature.name)
            else
                SubmarketModel(hp$submarket.feature.name)
        }
        else NoSubmarket()


    predictions <- exp(mll$predictions)    # possible NA
    num.training <- mll$num.training       # probably not used
    
    actuals <- queries$price
    evaluate <- EvaluatePredictions( prediction = predictions
                         ,actual = actuals)
    evaluate
}
Main <- function(control, transaction.data.all.years) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    transaction.data <- After2002(transaction.data.all.years)

    hps <- ListAppend(NULL, list(use.submarket = FALSE))
    for (submarket.feature.name in control$submarket.feature.names) {
        hps <- ListAppend(hps, list(use.submarket = TRUE
                                    ,use.indicator = TRUE
                                    ,submarket.feature.name = submarket.feature.name
                                    )
        )
        # comment out, because all of these models fail
        # the reason for the failure is that each model has too few observations to be trained
        # example: one fold has 6 training samples
#        hps <- ListAppend(hps, list(use.submarket = TRUE
#                                    ,use.indicator = FALSE
#                                    ,submarket.feature.name = submarket.feature.name
#                                    )
#        )
    }

    EvaluateModel <- sapply( hps
                            ,function(hp) {
                                # return function that implements CrossValidation API
                                force(hp)
                                function(data, is.testing, is.training, control)
                                    EvaluateModelHp( hp
                                                    ,data
                                                    ,is.testing
                                                    ,is.training
                                                    ,control)
                            }
                            )

    model.name <- sapply( hps
                         ,function(hp)
                             if (hp$use.submarket) {
                                 sprintf('indicator %s', hp$submarket.feature.name)
                             } else {
                                 'global model'
                             }
                         )

    cv.result <- CrossValidate( data = transaction.data
                               ,nfolds = control$nfolds
                               ,EvaluateModel = EvaluateModel
                               ,model.name = model.name
                               ,control = control
                               ,verbose = control$verbose.CrossValidate
                               )

    save( control
         ,cv.result
         ,model.name
         ,file = control$path.out.rdata
         )

    str(control)
}

################## Main program
clock <- Clock()

just.testing <- FALSE
default.args <-
    if (just.testing) {
        list(query = 10000)
    } else {
        list(query = 100)
    }

control <- Control(default.args)

# cache transaction.data
if (!exists('transaction.data')) {
    transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                              ,split.names = control$split.names
                                              )
}

Main(control, transaction.data)
if (control$testing)
    cat('TESTING: DISCARD RESULTS\n')
Printf('took %f CPU minutes\n', clock$Cpu() / 60)
cat('done\n')
