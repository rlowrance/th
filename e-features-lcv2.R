# e-features-lcv2.R
# main program
# Determine rank ordering of features to use and write this to file WORKING/e-feature-lcv2.txt

# Use the LCV Approach:
# 1. Use elestic net's lasso functionality to determine rank ordering of features
#    in terms of their importance.
# 2. Cross validate n models, where n is the number of features and model n has
#    features of importance 1 through importance n. Implement this in e-cv.

source('Directory.R')
source('Libraries.R')

source('Predictors2.R')
source('ReadTransactionSplits.R')

library(elasticnet)
library(memoise)
library(optparse)

Control <- function(default.args) {
    stopifnot(is.null(default.args))
     
    me <- 'e-features-lcv2' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    # define the splits that we use
    predictors <- Predictors2( predictors.name = 'alwaysNoAssessment'
                              ,predictors.form = 'level'
                              )
    identification <- Predictors2('identification')
    prices <- Predictors2('price')
    response <- 'price.log'
    formula <- Formula( predictors = predictors
                       ,response = response
                       )

    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, me, '.log')
                    ,path.out.txt = paste0(working, me, '.txt')
                    ,predictors = predictors
                    ,response = response
                    ,formula = formula
                    ,split.names = unique(c(predictors, identification, prices))
                    ,testing = FALSE
                    ,debug = FALSE
                    )
    control
}
EvaluateOLD <- function(prediction, actual) {
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

    # stop if any columns have zero variance or an NA value
    # otherwise, elastic net will throw an error
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
ListRemoveNullsOLD <-function(lst) {
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
EvaluateFeaturesOLD <- function(features, data, is.testing, is.training, control) {
    # reduce to call to ModelLinearLocal followed by call to evaluate preditions
    # determine which transactions will be the query transactions
    #cat('EvaluateFeatures\n'); browser()

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
    # return list of features ordered in terms of their importance
    # perform lasso regression using elasticnet function enet and predict.enet

    # use lasso to determine the order in which variables should enter the model
    fitted <- enet( x = MakeX(transaction.data, control)
                   ,y = MakeY(transaction.data, control)
                   ,lambda = 0  # just lasso, not also ridge regression
                   ,trace = TRUE
                   )
    ordered.features.with.nulls <- sapply(fitted$action, function(x) attributes(x)$names)
    ordered.features.list <- ListRemoveNulls(ordered.features.with.nulls)
    ordered.features <- sapply(ordered.features.list, function(x) x)

    str(ordered.features)

    ordered.features
}
Main <- function(control, transaction.data.all.years) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    # eliminate data before 2003
    first.date <- as.Date('2003-01-01')
    is.ok.date <- transaction.data.all.years$saleDate >= first.date
    transaction.data <- transaction.data.all.years[is.ok.date, ]

    browser()
    ordered.features <- LCVAnalysis(control, transaction.data)
    writeLines( text = ordered.features
               ,con = control$path.out.txt
               )
}

clock <- Clock()

default.args <- NULL  # synthesize the command line that will be used in the Makefile
#default.args <- list('--query', '100')

control <- Control(default.args)


# cache transaction.data
if (!exists('e-features-lcv2-transaction.data')) {
    e.features.lcv2.transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                                              ,split.names = control$split.names
                                                              )
}

Main( control = control
     ,transaction.data = e.features.lcv2.transaction.data)
if (control$testing)
    cat('DISCARD: TESTING')

Printf('took %f CPU minutes\n', clock$Cpu() / 60)
Printf('took %f wallclock minutes\n', clock$Wallclock() / 60)
Printf('finished at %s\n', as.character(Sys.time()))

cat('done\n')
