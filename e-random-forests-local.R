# e-random-forests-local.R
# main program
# determine best hyperparameters for a random forest model
#
# command line
# --query INT: 1 / INT fraction of query transactions are used

source('Directory.R')
source('Libraries.R')

source('After2002.R')
source('ModelRandomForestsLocal.R')
source('Predictors.R')
source('PredictorsBest.R')
source('ReadTransactionSplits.R')

library(memoise)
library(optparse)

Control <- function(default.args) {
    # parse command line arguments in command.args
    command.args <- commandArgs(trailingOnly = TRUE)
    opt <- ParseCommandArgs(command.args, default.args)

    me <- 'e-random-forests-local' 

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
        sprintf( '%s--query-%d'
                ,me
                ,opt$query
                )

    # the hyperparameters were set by running e-random-forests-global
    # for Jan 2003 and observing the best cross-validated estimate of
    # the generalization error
    mtry = 2
    ntree = 1000

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
                    ,mtry = mtry
                    ,ntree = ntree
                    ,verbose.CrossValidate = TRUE
                    ,query = opt$query
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
EvaluateHps <- function(mtry, ntree, data, is.testing, is.training, control) {
    # evaluate hyperparamters mtry and num.trees
    # reduce to call to ModelRandomForestLocal followed by call to evaluate preditions
    # determine which transactions will be the query transactions
    #cat('EvaluateHps\n'); browser()

    clock <- Clock()

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

    mll <- ModelRandomForestsLocal( InTraining = InTraining
                                   ,queries = queries
                                   ,data.training = data.training
                                   ,formula = Formula( response = control$response
                                                      ,predictors = control$predictors
                                                      )
                                   ,mtry = mtry
                                   ,ntree = ntree
                                   )
    predictions <- exp(mll$predictions)    # possible NA
    num.training <- mll$num.training       # probably not used
    
    actuals <- queries$price
    evaluate <- EvaluatePredictions( prediction = predictions
                         ,actual = actuals)
    Printf( 'CPU minutes for query %d mtry %d ntree %d = %f\n'
           ,control$query
           ,mtry
           ,ntree
           ,clock$Cpu() / 60
           )
    evaluate
}
Main <- function(control, transaction.data.all.years) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    transaction.data <- After2002(transaction.data.all.years)

    # models have both mtry and num.trees.multiplier hyper parameters
    # there is exactly one model, because the hyperparameters were set elswhere
    model.hp <- list(list( mtry = control$mtry
                          ,ntree = control$ntree
                          )
    )

    EvaluateModel <- sapply( model.hp
                            ,function(hp) {
                                # return function that implements CrossValidation API
                                force(hp)
                                function(data, is.testing, is.training, control)
                                    EvaluateHps( hp$mtry
                                                ,hp$ntree
                                                ,data
                                                ,is.testing
                                                ,is.training
                                                ,control
                                                )
                            }
                            )
    model.name <- sapply( model.hp
                         ,function(hp) {
                             sprintf( 'mtry %d ntree %d'
                                     ,hp$mtry
                                     ,hp$ntree
                                     )
                         }
                         )
    str(model.name)


    #cat('about to call CrossValidate\n'); browser()
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


clock <- Clock()

#debug(Control)
default.args <- list(query=100)
control <- Control(default.args)


# cache transaction.data
if (!exists('transaction.data')) {
    transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                              ,split.names = control$split.names
                                              )
}

Main(control, transaction.data)
if (control$testing)
    cat('DISCARD: testing\n')
Printf('took %f CPU minutes\n', clock$Cpu() / 60)
cat('done\n')
