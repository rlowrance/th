# e-ridge-regression.R
# main program
# compare the best of the reduced model: OLS vs. ridge regression
# command line
# --query INT: 1 / INT fraction of query transactions are used
# --lambdaSet NAME : name of a set of lambda parameters

source('Directory.R')
source('Libraries.R')

source('ModelLinearRidgeLocal.R')
source('Predictors.R')
source('ReadTransactionSplits.R')

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
    predictors <- c(#the 23 most-important predicators from the LCV experiment
                     'living.area'
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
                    ,'year.built'
                    ,'census.tract.has.park'
                    ,'basement.square.feet'
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
    testing <- FALSE
    #testing <- TRUE
    out.base <-
        sprintf( '%s--query-%d'
                ,me
                ,opt$query
                )

    lambdaSets <-
        list(one = c(0,100,10,1,.1,.01,.001))
    lambda <- lambdaSets[[opt$lambdaSet]]
    stopifnot(!is.null(lambda))

    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,num.training.days = 90
                    ,response = response
                    ,predictors = predictors
                    ,split.names = unique(c(predictors, other.names))
                    ,nfolds = if (testing) 2 else 10
                    ,testing.period = list( first.date = as.Date('1984-02-01')
                                           ,last.date = as.Date('2009-03-31')
                                           )
                    ,testing = testing
                    ,debug = FALSE
                    ,verbose.CrossValidate = TRUE
                    ,lambda = lambda
                    ,query.fraction = (1 / opt$query)
                    )
    control
}
ParseCommandArgs <- function(command.args) {
    # return name list of values from the command args
    opt.query <- make_option( opt_str = c('--query')
                             ,action = 'store'
                             ,type = 'double'
                             ,default = 100
                             ,help = '1 / fraction of samples used as queries'
                             )
    opt.lambdaSet <- make_option( opt_str = c('--lambdaSet')
                                 ,action = 'store'
                                 ,type = 'character'
                                 ,default = 'one'
                                 ,help = 'for now, "one"'
                                 )
    option.list <- list( opt.query
                        ,opt.lambdaSet
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
EvaluateFeatures <- function(lambda, data, is.testing, is.training, control) {
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

    mll <- ModelLinearRidgeLocal( InTraining = InTraining
                                 ,queries = queries
                                 ,data.training = data.training
                                 ,formula = Formula( response = control$response
                                                    ,predictors = control$predictors
                                                    )
                                 ,lambda = lambda
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


    
    num.models <- length(control$lambda)
    EvaluateModel <- sapply(1:num.models
                            ,function(n) {
                                # return function that implements CrossValidation API
                                force(n)
                                function(data, is.testing, is.training, control)
                                    EvaluateFeatures( control$lambda[[n]]
                                                     ,data
                                                     ,is.testing
                                                     ,is.training
                                                     ,control)
                            }
                            )

    model.name <- sapply( 1:num.models
                         ,function(n)
                             sprintf( 'ridge regression lambda = %f'
                                     ,control$lambda[[n]]
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
default.args <- list('--query', '100', '--lambdaSet', 'one' )

command.args <- if (is.null(default.args)) commandArgs(trailingOnly = TRUE) else default.args
control <- Control(command.args)


# cache transaction.data
if (!exists('transaction.data')) {
    transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                              ,split.names = control$split.names
                                              )
}

if (!is.null(default.args))
    cat('USING DEFAULT ARGS\n')

Main(control, transaction.data)
cat('done\n')