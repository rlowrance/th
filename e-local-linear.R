# e-local-linear.R
# main program
# estimate generalization error using 10-fold cross validation for one combination of
#
# timePeriod, one of 2009 | 2003
# ndays of training data, a positive integer (N)
# predictors, one of all | butAssessment
# predictorsForm, one of linear | log
# query, positive integer, sample (1 / TIMEPERIOD) of queries
# regressor, one of logprice | price
# scenario, one of assessor | avm | mortgage
#
# write one file containing cv.result (cross validation results)
# WORKING/e-local-linear-TIMEPERIOD-NDAYS-PREDICTORS-PREDICTORSFORM-REGRESSOR-SCENARIO.RData
# Write file WORKING/e-enpsr--y/n-N-Log/linear-assessor/avm/mortgage-price/logprice.RData
# 
# Use 100% of the test data in each fold.
#
# command line
# --ndays INT
# --predictors {always|alwayNoassessment}
# --predictorsForm {linear|log}
# --query INT: 1 / INT fraction of query transactions are used
# --scenario {assessor|avm|mortgage}
# --response  {logprice|price}
# --timePeriod {2003|2009}

source('Directory.R')
source('Libraries.R')

#source('ModelLinearLocal2.R')
source('Predictors2.R') 
source('ReadTransactionSplits.R')

library(memoise)
library(optparse)

Control <- function(default.args) {
    # parse command line arguments in command.args
    opt <- ParseCommandArgs( command.args = commandArgs(trailingOnly = TRUE)
                            ,default.args = default.args
                            )

    me <- 'e-local-linear' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    # validate each command arg

    stopifnot(opt$ndays > 0)
    stopifnot(  opt$predictors == 'always'
              ||opt$predicrors == 'alwaysNoAssessment'
              )
    stopifnot(  opt$predictorsForm == 'linear'
              ||opt$predictorsForm == 'log'
              )
    stopifnot(opt$query > 0)
    stopifnot(  opt$scenario == 'assessor'
              ||opt$scenario == 'avm'
              ||opt$scenario == 'mortgage'
              )
    stopifnot(  opt$response == 'logprice'
              ||opt$response == 'price'
              )
    stopifnot(  opt$timePeriod == '2003'
              ||opt$timePeriod == '2009'
              )

    # define splits we use
    predictors <- Predictors2( predictors.name = opt$predictors
                              ,predictors.form = opt$predictorsForm
                              )
    identification <- Predictors2('identification')
    response <- switch( opt$response
                        ,logprice = 'price.log'
                        ,price = 'price'
                        )

    testing <- FALSE
    #testing <- TRUE

    out.base <-
        sprintf( '%s-%s-%s-%s-%s-%s-%d-%d'
                ,me
                ,opt$timePeriod
                ,opt$scenario
                ,opt$response
                ,opt$predictors
                ,opt$predictorsForm
                ,opt$ndays
                ,opt$query
                )

    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,opt = opt
                    ,model.name = out.base
                    ,predictors = predictors
                    ,response = response
                    ,nfolds = if (testing) 2 else 10
                    ,split.names = unique(c( predictors
                                            ,response
                                            ,identification
                                            )
                    )
                    ,testing = testing
                    ,debug = FALSE
                    ,verbose.CrossValidate = TRUE
                    )
    control
}
ParseCommandArgs <- function(command.args, default.args) {
    # return name list of values from the command args
    OptionChr <- function(name, help) {
        make_option( opt_str = c(sprintf('--%s', name))
                    ,action = 'store'
                    ,type = 'character'
                    ,default = default.args[[name]]
                    ,help = help
                    )
    }
    OptionInt <- function(name, help) {
        make_option( opt_str = c(sprintf('--%s', name))
                    ,action = 'store'
                    ,type = 'integer'
                    ,default = default.args[[name]]
                    ,help = help
                    )
    }

    option.list <-
        list( OptionInt('ndays', 'number of days in training period')
             ,OptionChr('predictors', 'name of predictor set')
             ,OptionChr('predictorsForm', 'one of {linear|log}')
             ,OptionInt('query', ' 1 / <fraction of test sample used as queries>')
             ,OptionChr('scenario', 'one of {assessor|avm|mortgage}')
             ,OptionChr('response', 'one of {logprice|price}')
             ,OptionChr('timePeriod', 'one of {2003|2009}')
             )

    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
    opt
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
PredictOneQuery <- function(data.training.fold, query, control) {
    browser()
    # return list
    # $ok: TRUE or FALSE
    # $value: prediction if $ok == TRUE
    # $problem: chr description of $ok == FALSE

    # use query saleDate to select training transactions
    saleDate <- query$saleDate

    Selector <- 
        switch( control$opt$scenario
               ,assessor = TrainingTransactionsAssessor
               ,avm      = TrainingTransactionsAVM
               ,mortgage = TrainingTransactionsMortgage
               ,stop('bad control$opt$scenario')
               )
    data.training <- Selector(data.training.fold, saleDate, control$opt$ndays)

    formula <- Formula( predictors = control$predictors
                       ,response = control$response
                       )
}
SelectTrainingDataAssessor <- function(data, saleDate, ndays) {
    last.date <- saleDate - 1
    first.date <- last.date - ndays + 1
    is.selected <-
        data$recordingDate >= first.date &
        data$recordingDate <= last.date
    selected <- data[is.selected,]
    selected
}
SelectTrainingDataAVM <- function(data, saleDate, ndays) {
    browser()
    last.date <- saleDate - 1
    first.date <- last.date - ndays + 1
    is.selected <-
        data$saleDate >= first.date &
        data$saleData <= last.date
    selected <- data[is.selected,]
    selected
}
SelectTrainingDataMortgage <- function(data, saleDate, ndays) {
    browser()
    last.date <- saleDate + round(ndays / 2)
    first.date <- last.date - round(ndays / 2)
    if (last.date - first.day != nday) 
        first.date <- first.date - 1
    is.selected <-
        data$saleDate >= first.date &
        data$saleData <= last.date
    selected <- data[is.selected,]
    selected
}
SelectTrainingData <- function(data.training.fold, query.saleDate, control) {
    # return transactions within opt$ndays of the saleDate, conditioned on opt$scenario
    Selector <- switch( control$opt$scenario
                       ,assessor = SelectTrainingDataAssessor
                       ,avm = SelectTrainingDataAVM
                       ,mortgage = SelectTrainingDataMortgage
                       ,stop('bad opt$scenario')
                       )
    result <- Selector(data.training.fold, query.saleDate, control$opt$ndays)
    result
}
PredictEachQuery <- function(data.training.fold, queries, control) {
    # return vector of predictions or NA

    verbose <- TRUE

    formula <- Formula( predictors = control$predictors
                       ,response = control$response
                       )

    Fit <- function(query.saleDate) {
        data <- SelectTrainingData(data.training.fold, query.saleDate, control)
        maybe.fitted <-
            tryCatch( lm(formula = formula, data = data)
                     ,warning = function(w) w
                     ,error = function(e) e
                     )
        result <- list( ok = inherits(maybe.fitted, 'lm')
                       ,value = maybe.fitted
                       )
        result
    }
    FitMemoised <- memoise(Fit)

    Predict <- function(fitted, query) {
        maybe.prediction <- 
            tryCatch( predict(object = fitted, newdata = query)
                     ,warning = function(w) w
                     ,error = function(e) e
                     )
        result <- list( ok = inherits(maybe.prediction, 'numeric')
                       ,value = maybe.prediction
                       )
        result
    }

    predictions <- as.double(rep(NA), nrow(queries))
    for (query.index in 1:nrow(queries)) {
        query <- queries[query.index,]
        query.saleDate <- query$saleDate
        maybe.fitted <- FitMemoised(query.saleDate)
        if (maybe.fitted$ok) {
            maybe.prediction <- Predict(maybe.fitted$value, query)
            if (maybe.prediction$ok) {
                predictions[[query.index]] <- maybe.prediction$value
            } else {
                if (verbose) {
                    Printf( 'prediction failed for query.index %d saleDate %s\n'
                           ,query.index
                           ,as.character(query.saleDate)
                           )
                    print(maybe.prediction$value)
                }
            }
        } else {
            if (verbose) {
                Printf( 'fitting failed for query.index %d saleDate %s\n'
                       ,query.index
                       ,as.character(query.saleDate)
                       )
                print(maybe.fitted$value)
            }
        }
        #Printf('query.index %d prediction %f\n', query.index, predictions[[query.index]])
    }
    predictions
}
Queries <- function(data.testing.fold, control) {
    # return query transactions, which are a subset of the testing fold

    first.test.date <-
        switch( control$opt$timePeriod
               ,'2009' = as.Date('2008-11-01')
               ,'2003' = as.Date('2003-01-01')
               ,stop('unimplemented opt$timePeriod')
               )
    in.test <- data.testing.fold$saleDate >= first.test.date
    data.testing <- data.testing.fold[in.test,]
    num.test.transactions <- max( 1
                                 ,round(nrow(data.testing) / control$opt$query)
                                 )
    which.test <- sample.int( n = nrow(data.testing)
                             ,size = num.test.transactions
                             ,replace = FALSE)
    queries <- data.testing[which.test,]
    Printf('will sample %d of %d test transactions\n', nrow(queries), nrow(data.testing))
    queries
}
EvaluateModelHp <- function(hp, data, is.testing, is.training, control) {
    # evaluate model with specified hyperparameters hp on one fold of data
    # return evaluation list that is then returned to CrossValidate
    
    # split the data into training and test folds

    data.training.fold <- data[is.training,]
    data.testing.fold <- data[is.testing, ]

    queries <- Queries(data.testing.fold, control)

    # predict and compare to actuals
    predictions.raw <- PredictEachQuery(data.training.fold, queries, control)
    if (control$response == 'price.log') {
        predictions <- exp(predictions.raw)
        actuals <- exp(queries$price.log)
    } else {
        predictions <- predictions.raw
        actuals <- queries$price
    }

    # always evaluate in the level domain (not log domain)
    evaluate <- EvaluatePredictions( prediction = predictions
                                    ,actual = actuals)
    evaluate
}
Main <- function(control, transaction.data.all.years) {
    browser()
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    # extract transactions just for the time period we want
    if (FALSE) {
        first.date <-
            if (control$opt$timePeriod == '2003') {
                as.Date('2003-01-01')
            } else if (control$opt$timePeriod == '2009') {
                as.Date('2008-11-01')
            } else {
                stop('bad opt$timePeriod')
            }

        in.time.period <-
            transaction.data.all.years$saleDate >= first.date
        transaction.data <- transaction.data.all.years[in.time.period,]
    }

    # build list of hyperparameters to be used in defining the models for CrossValidate
    # here there is only one model and the hyperparameters are from the command line options
    hps <- list(list( timePeriod = control$opt$timePeriod
                     ,scenario = control$opt$scenario
                     ,response = control$opt$response
                     ,predictors = control$opt$predictors
                     ,predictorsForm = control$opt$predictorsForm
                     ,ndays = control$opt$ndays
                     ,query = control$opt$query
                     )
    )
    

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

    model.name = control$model.name

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

default.args <-
    list( ndays=30
         ,predictors = 'always'
         ,predictorsForm = 'log'
         ,query = 1
         ,scenario = 'assessor'
         ,response = 'logprice'
         ,timePeriod = '2009'
         )
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
