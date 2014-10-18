# e-ridge-regression.R
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

    me <- 'e-ridge-regression' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    # define the splits that we use
    predictors <- Predictors('all.level')  # we need all but 2 of these, to take them all
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
                    ,split.names = unique(c(predictors, other.names))
                    ,nfolds = 10
                    ,testing.period = list( first.date = as.Date('1984-02-01')
                                           ,last.date = as.Date('2009-03-31')
                                           )
                    ,testing = testing
                    ,debug = FALSE
                    ,verbose.CrossValidate = TRUE
                    ,feature.set = list( FeatureSetLCV1()
                                        ,FeatureSetLCV2()
                                        ,FeatureSetLCV3()
                                        ,FeatureSetLCV4()
                                        ,FeatureSetPCA1()
                                        ,FeatureSetPCA2()
                                        ,FeatureSetPCA3()
                                        )
                    ,query.fraction = (1 / opt$query)
                    )
    control
}
FeatureSetLCV1 <- function() {
    # return the first 9 of Chopra's features from the LCV experiments
    result <- c( 'living.area'
                ,'median.household.income'
                ,'avg.commute.time'
                ,'fraction.owner.occupied'
                ,'parking.spaces'
                ,'factor.has.pool'
                ,'land.square.footage'
                ,'year.built'
                ,'bedrooms'
                )
    result
}
FeatureSetLCV2 <- function() {
    # return the first 4 of Chopra's features from the LCV experiments
    result <- c( 'living.area'
                ,'median.household.income'
                ,'avg.commute.time'
                ,'fraction.owner.occupied'
                )
    result
}
FeatureSetLCV3 <- function() {
    # return the first 23 of the expanded feature set from the LCV experiments
    result <- c( 'living.area'
                ,'median.household.income'
                ,'fireplace.number'
                ,'avg.commute.time'
                ,'fraction.owner.occupied'
                ,'effective.year.built'
                ,'zip5.has.industry'
                ,'total.rooms'
                ,'census.tract.has.industry'
                ,'parking.spaces'
                ,'land.square.footage'
                ,'factor.has.pool'
                ,'zip5.has.school'
                ,'stories.number'
                ,'census.tract.has.retail'
                ,'zip5.has.park'
                ,'bedrooms'
                ,'bathrooms'
                ,'factor.is.new.construction'
                ,'census.tract.has.school'
                ,'basement.square.feet'
                )
    result
}
FeatureSetLCV4 <- function() {
    # return the first 5 of the expanded feature set from the LCV experiments
    result <- c( 'living.area'
                ,'median.household.income'
                ,'fireplace.number'
                ,'avg.commute.time'
                ,'fraction.owner.occupied'
                )
    result
}
FeatureSetPCA1 <- function() {
    # return list of features from PCA experiments
    result <- c( 'median.household.income'
                ,'land.square.footage'
                ,'basement.square.feet'
                )
    result
}
FeatureSetPCA2 <- function() {
    # return list of features from PCA experiments
    result <- c( 'median.household.income'
                ,'land.square.footage'
                )
    result
}
FeatureSetPCA3 <- function() {
    # return list of features from PCA experiments
    result <- c( 'median.household.income'
                )
    result
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
Main <- function(control, transaction.data) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)


    
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

    model.name <- sapply( 1:num.models
                         ,function(n)
                             sprintf( '%s using %d features'
                                     ,if (n <=4)  'LCV' else 'PCA'
                                     ,length(control$feature.set[[n]])
                                     )
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
    if (control$testing)
        cat('TESTING: DISCARD RESULTS\n')
    
}

#debug(Control)
default.args <- NULL  # synthesize the command line that will be used in the Makefile
#default.args <- list('--query', '10000')

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
