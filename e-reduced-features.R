# e-reduced-features.R
# main program
# Determine which reduced model is best using 10-fold cross validation.

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

    me <- 'e-reduced-features' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    # define the splits that we use
    predictors <- Predictors('all.level')  # be conservative and assume everything
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
        sprintf( '%s'
                ,me
                )

    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,query.fraction = (1 / opt$query)
                    ,num.training.days = 90
                    ,predictors = predictors
                    ,response = response
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
ParseCommandArgsOLD <- function(command.args) {
    # return name list of values from the command args
    opt.predictors <- make_option( opt_str = c('--predictors')
                                  ,action = 'store'
                                  ,type = 'character'
                                  ,help = 'name of feature set to use'
                                  )
    opt.query <- make_option( opt_str = c('--query')
                             ,action = 'store'
                             ,type = 'double'
                             ,default = .01
                             ,help = '1 / fraction of samples used as queries'
                             )
    option.list <- list( opt.predictors
                        ,opt.query
                        )
    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
}
Evaluate <- function(prediction, actual) {
    # return list of evaluations
    # ARGS
    # prediction: vector of predictions or NA
    # actual    : vector of actual values
    #cat('start Evaluate\n'); browser()

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
MakeX <- function(data, control) {
    # return matrix containing just the predictor columns

    predictor.df <- data[, control$predictors]

    # stop if any columns have zero variance
    lapply( control$predictors
           ,function(predictor) {
               values <- predictor.df[,predictor]
               standard.deviation <- sd(values, na.rm = TRUE)
               Printf( 'predictor %40s has %d NA values out of %d; sd %f\n'
                      ,predictor
                      ,sum(is.na(values))
                      ,length(values)
                      ,standard.deviation
                      )
#               if (predictor == 'fireplace.number' || predictor == 'fireplace.indicator.flag')
#                   browser()
               if (standard.deviation == 0)
                   stop(sprintf('predictor %s has zero standard deviation', predictor))
           }
           )

    # convert data frame to matrix
    result <- data.matrix(predictor.df)
    result
}
MakeY <- function(data, control) {
    result <- data[, control$response]
    result
}
ListRemoveNulls <-function(lst) {
    # return lst with NULL elements removed
    # ref: R Cookbook, 5.12 Removing NULL elements from a list
    is.null.element <- sapply(lst, is.null)
    lst[is.null.element] <- NULL
    lst
}
ListRemoveNulls <-function(lst) {
    # return lst with NULL elements removed
    # ref: R Cookbook, 5.12 Removing NULL elements from a list
    is.null.element <- sapply(lst, is.null)
    lst[is.null.element] <- NULL
    lst
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
    evaluate <- Evaluate( prediction = predictions
                         ,actual = actuals)
    evaluate
}
LCVAnalysis <- function(control, transaction.data) {
    # perform lasso regression using elasticnet function enet and predict.enet
    #cat('Analysis\n'); browser()

    clock <- Clock()
    # use lasso to determine the order in which variables should enter the model
    fitted <- enet( x = MakeX(transaction.data, control)
                   ,y = MakeY(transaction.data, control)
                   ,lambda = 0  # just lasso, not also ridge regression
                   ,trace = TRUE
                   )
    ordered.features.with.nulls <- sapply(fitted$action, function(x) attributes(x)$names)
    ordered.features <- ListRemoveNulls(ordered.features.with.nulls)
    str(ordered.features)

    # use cross validation to select from the models

    EvaluateModel <- sapply( 1:length(ordered.features)
                            ,function(n) {
                                # return function that implements the CrossValidate API
                                force(n)
                                function(data, is.testing, is.training, control)
                                    EvaluateFeatures( ordered.features[1:n]
                                                     ,data
                                                     ,is.testing
                                                     ,is.training
                                                     ,control
                                                     )
                            }
                            )
    model.name <- sapply( 1:length(ordered.features)
                         ,function(n) 
                             sprintf('using %d best features', n)
                         )

    cv.result <- CrossValidate( data = transaction.data
                               ,nfolds = control$nfolds
                               ,EvaluateModel = EvaluateModel
                               ,model.name = model.name
                               ,control = control
                               ,verbose = TRUE
                               )

    str(cv.result)
    str(ordered.features)

    elapsed.cpu <- clock$Cpu()
    elapsed.message <- sprintf( 'predicting with %f query fraction took %f CPU seconds\n'
                              ,control$query.fraction
                              ,elapsed.cpu
                              )
    Printf(elapsed.message)

    save( control
         ,cv.result
         ,ordered.features
         ,elapsed.cpu
         ,elapsed.message
         ,file = control$path.out.rdata
         )
}
Main <- function(control, transaction.data) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    LCVAnalysis(control, transaction.data)

    str(control)
    if (control$testing)
        cat('TESTING: DISCARD RESULTS\n')
    
}

#debug(Control)
default.args <- NULL  # synthesize the command line that will be used in the Makefile
#default.args <- list('--predictors', 'chopra', '--query.fraction', '.0001')

command.args <- if (is.null(default.args)) commandArgs(trailingOnly = TRUE) else default.args
control <- Control(command.args)


# cache transaction.data
if (!exists('transaction.data')) {
    transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                              ,split.names = control$split.names
                                              )
}

#debug(Main)
Main(control, transaction.data)
cat('done\n')
