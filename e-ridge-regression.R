# e-ridge-regression.R
# main program
# compare the best of the reduced model: OLS vs. ridge regression
# command line
# --query INT     : int, 1 / INT fraction of query transactions are used
# --lambdaSet NAME: chr, name of a set of lambda parameters

source('Directory.R')
source('Libraries.R')

source('After2002.R')
source('ParseCommandArgsERidgeRegression.R')
source('ModelLinearRidgeLocal.R')
source('Predictors.R')
source('PredictorsBest.R')
source('ReadTransactionSplits.R')

library(memoise)
library(optparse)

Control <- function(default.args) {
    # parse command line arguments in command.args
    opt <- ParseCommandArgsERidgeRegression( command.args = commandArgs(trailingOnly = TRUE)
                                            ,default.args
                                            )

    me <- 'e-ridge-regression' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    # define the splits that we use
    predictors <- PredictorsBest()
    identification <- Predictors('identification')
    prices <- Predictors('prices')

    response <- 'price.log'

    testing <- FALSE
    #testing <- TRUE

    out.base <-
        sprintf( '%s--query-%d--lambdaSet-%s'
                ,me
                ,opt$query
                ,opt$lambdaSet
                )

    lambdaSets <-
        list( a = c(0, 0.1, 0.3, 1, 10, 30, 100)
             ,b = c(0, .01, .03, .10, .30, 1, 3, 10, 30, 100)
             ,c = c(0, .001, .003, .010, .03, .10, .30, 1.0)
             ,d = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1.000)
             ,e = c(.01, .02, .03, .04, .05, .06, .07, .08, .09, .10)
             ,just_001 = c(.001)
             )
    lambda <- lambdaSets[[opt$lambdaSet]]
    stopifnot(!is.null(lambda))

    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,num.training.days = 90
                    ,response = response
                    ,predictors = predictors
                    ,split.names = unique(c(predictors, identification, prices))
                    ,nfolds = if (testing) 2 else 10
                    ,testing = testing
                    ,debug = FALSE
                    ,verbose.CrossValidate = TRUE
                    ,lambda = lambda
                    ,query.fraction = (1 / opt$query)
                    ,opt = opt
                    )
    control
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

    str(control$opt)

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
Main <- function(control, transaction.data.all.years) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    transaction.data <- After2002(transaction.data.all.years)
    
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
}

############# Execution Starts Here

clock <- Clock()

just.testing <- FALSE
default.args <-
    if (just.testing) {
        list( query = 10000
             ,lambdaSet = 'one'
             )
    } else {
        list( query = 100
             ,lambdaSet = 'one'
             )
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
    cat('TESTING: DISCARD RESULT\n')
Printf('took %f CPU minutes\n', clock$Cpu() / 60)
cat('done\n')
