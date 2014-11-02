# e-cv.R
# main program
# estimate generalization error using 10-fold cross validation for one combination of
#
# timePeriod, one of 2009 | 2003
# ndays of training data, a positive integer (N)
# predictorsName, one of all | butAssessment
# predictorsForm, one of linear | log
# query, positive integer, sample (1 / TIMEPERIOD) of queries
# regressor, one of logprice | price
# scenario, one of assessor | avm | mortgage
#
# write one file containing cv.result (cross validation results)
# WORKING/e-cv-TIMEPERIOD-NDAYS-predictorsName-PREDICTORSFORM-REGRESSOR-SCENARIO.RData
# Write file WORKING/e-enpsr--y/n-N-Log/linear-assessor/avm/mortgage-price/logprice.RData
# 
# example output file name
# e-cv-global-linear-2009-assessor-logprice-linear-alwaysNoAssessment-90-100---.RData
# e-cv-submarket-linearReg-2003-avm-price-log-always-30-10-2000--.RData
# e-cv-submarketIndicator-randomForest-2003-mortgage-logprice-linear-120-100--1000-2.RData
# 
# command line
# --scope {global, submarket, submarketIndicator}
# --model {linear, linearReg, randomForest}
# --timePeriod {2003|2009}
# --scenario {assessor|avm|mortgage}
# --response  {logprice|price}
# --predictorsForm {level|log}
# --predictorsName {always|alwaysNoAssessment|alwaysNoCensus}
# --ndays INT
# --query INT: 1 / INT fraction of query transactions in a fold that are used
# --c     INT: (1 / lamba) if regularizing, C == 0 ==> no regularizer
# --ntree INT
# --mtry  INT

source('Directory.R')
source('Libraries.R')

#source('ModelLinearLocal2.R')
source('Predictors2.R') 
source('ReadTransactionSplits.R')

library(lubridate)
library(memoise)
library(optparse)

Control <- function(default.args) {
    browser()
    # parse command line arguments in command.args
    opt <- ParseCommandArgs( command.args = commandArgs(trailingOnly = TRUE)
                            ,default.args = default.args
                            )

    me <- 'e-cv' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    # validate each command arg

    stopifnot(opt$ndays > 0)
    stopifnot( opt$predictorsName == 'always'
              |opt$predictorsName == 'alwaysNoAssessment'
              |opt$predictorsName == 'alwaysNoCensus'
              )
    stopifnot( opt$predictorsForm == 'level'
              |opt$predictorsForm == 'log'
              )
    stopifnot(opt$query > 0)
    stopifnot( opt$scenario == 'assessor'
              |opt$scenario == 'avm'
              |opt$scenario == 'mortgage'
              )
    stopifnot( opt$response == 'logprice'
              |opt$response == 'price'
              )
    stopifnot( opt$timePeriod == '2003on'
              |opt$timePeriod == '2008'
              )

    # define splits we use
    predictors <- Predictors2( predictors.name = opt$predictorsName
                              ,predictors.form = opt$predictorsForm
                              )
    identification <- Predictors2('identification')
    prices <- c('price.log', 'price')

    testing <- FALSE
    #testing <- TRUE

    out.base <-
        sprintf( '%s_%s_%s_%s_%s_%s_%s_%s_%d_%d_%d_%d_%d'
                ,me
                ,opt$scope
                ,opt$model
                ,opt$timePeriod
                ,opt$scenario
                ,opt$response
                ,opt$predictorsName
                ,opt$predictorsForm
                ,opt$ndays
                ,opt$query
                ,opt$c
                ,opt$ntree
                ,opt$mtry
                )

    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,opt = opt
                    ,model.name = out.base
                    ,nfolds = if (testing) 2 else 10
                    ,split.names = unique(c( predictors
                                            ,identification
                                            ,prices
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
        list( OptionChr('scope',          'one of {global, submarket, submarketIndicator}')
             ,OptionChr('model',          'one of {linear, linearReg, randomForest}')
             ,OptionChr('timePeriod',     'one of {2003on|2008}')
             ,OptionChr('scenario',       'one of {assessor|avm|mortgage}')
             ,OptionChr('response',       'one of {logprice|price}')
             ,OptionChr('predictorsForm', 'one of {level|log}')
             ,OptionChr('predictorsName', 'one of {always|alwaysNoAssessment|alwaysNoCensus')
             ,OptionInt('ndays',          'number of days in training period')
             ,OptionInt('query',          ' 1 / <fraction of test sample used as queries>')
             ,OptionInt('c',              '(1/lamdda) for regularized regression')
             ,OptionInt('ntree',          'number of trees (for randomForest)')
             ,OptionInt('mtry',           'number of features samples when growing tree (for randomForest)')
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
TrainingData <- function(data.training, scenario, ndays, saleDate) {
    # return data frame with ndays of training data for the scenario and saleDate and ndays
    last.date <-
        switch( scenario
               ,assessor = saleDate - 1
               ,avm      = saleDate - 1
               ,mortgage = saleDate + (ndays / 2)
               )
    first.date <- last.date - ndays + 1

    data <- 
        switch( scenario
               ,assessor = data.training[data.training$recordingDate >= first.date &
                                         data.training$recordingDate <= last.date
                                         ,
                                         ]
               ,avm      = data.training[data.training$saleDate >= first.date &
                                         data.training$saleDate <= last.date
                                         ,
                                         ]
               ,mortgage = data.training[data.training$saleDate >= first.date &
                                         data.training$saleDate <= last.date
                                         ,
                                         ]
               )
    data
}
PrintNoninformativePredictors <- function(informative.predictors, all.predictors) {
    non.informative <- setdiff(all.predictors, informative.predictors)
    stopifnot(length(non.informative) > 0)
    if (length(non.informative) == 1)
        Printf('1 non-informative feature: %s\n', non.informative[[1]])
    else {
        Printf('%d non-informative features:\n', length(non.informative))
        lapply(non.informative, function(x) Printf(' %s\n', x))
    }
}
ReplaceXWithY <- function(vec, x, y) {
    if (x %in% vec) 
        union(setdiff(vec, x), y)
    else
        vec
}
MakeFormula <- function(data, scope, response, predictorsForm, predictorsName, control) {
    # return list $ok $value (maybe) $problem (optional)
    # return a formula using predictors that are informative (have more than 1 value)
    # --scope {global, submarket, submarketIndicator}
    # --response  {logprice|price}
    # --predictorsForm {linear|log}
    # --predictorsName {always|alwaysNoAssessment}
    # NOTE: if non-informative features are not dropped, lm() works and predict() fails


    # an informative predictor has more than one value
    all.predictors.with.year<- Predictors2( predictors.name = predictorsName
                                           ,predictors.form = predictorsForm
                                           )
    all.predictors.1 <- ReplaceXWithY( vec = all.predictors.with.year
                                      ,x = 'year.built'
                                      ,y = c('age', 'age2')
                                      )
    all.predictors <- ReplaceXWithY( vec = all.predictors.1
                                    ,x = 'effective.year.built'
                                    ,y = c('effective.age', 'effective.age2')
                                    )
    is.informative <- sapply(all.predictors
                             ,function(predictor) {
                                 length(unique(data[[predictor]])) > 1
                             }
                             )
    informative.predictors <- all.predictors[is.informative]
    num.informative <-  length(informative.predictors)
    if (num.informative == 0) {
        return(list(ok = FALSE, problem = paste0('MakeFormula: ', '0 informative features')))
    }
    if (length(informative.predictors) != length(all.predictors)) {
        PrintNoninformativePredictors( informative.predictors = informative.predictors
                                      ,all.predictors = all.predictors
                                      )
    }

    # for now, only handle scope == global
    # if scope = submarketIndicator, must add indicators

    stopifnot(scope == 'global')

    formula = Formula( response = if (response == 'logprice') 'price.log' else 'price'
                      ,predictors = informative.predictors
                      )
    list(ok = TRUE, value = formula)
}
ConvertYearFeatures <- function(data, saleDate) {
    # replace any year.built feature with age and age2 (measured in years)
    # replace any effective.year.built feature with effective.age and effective.age2 (in years)
    Ages <- function(year.vector, sale.year) {
        # return age and age^2 as of the sale.year
        age <- sale.year - year.vector
        age2 <- age * age
        list( age = age
             ,age2 = age2
             )
    }
    Replace <- function(current, new1, new2, data) {
        if (current %in% names(data)) {
            ages <- Ages(data[[current]], year(saleDate))
            data[[new1]] <- ages$age
            data[[new2]] <- ages$age2
            data[[current]] <- NULL
        }
        data 
    }
    data.1 <- Replace('year.built', 'age', 'age2', data)
    data.2 <- Replace('effective.year.built', 'effective.age', 'effective.age2', data.1)
    data.2
}
PredictLinear <- function(scenario, ndays, data.training, query.transactions
                          ,scope, response, predictorsForm, predictorsName, control) {
    # Return evaluation of the specified model on the given training and test data
    # Return vector of predictions for the query.transactions using a local model

    verbose <- FALSE

    Fit <- function(saleDate) {
        # return list $ok $value (maybe) $problem (maybe)
        training.data <- TrainingData( data.training = data.training
                                      ,scenario = scenario
                                      ,ndays = ndays
                                      ,saleDate = saleDate
                                      )
        if (nrow(training.data) == 0) {
            return(list(ok = FALSE, problem = 'no training samples'))
        }
        # convert year.built to age and age2
        # convert effective.year.built to effective.age and effective.age2
        converted.data <- ConvertYearFeatures( data = training.data
                                              ,saleDate = saleDate
                                              )

        stopifnot(scope == 'global')  # must take city-sample for submarket scope
        # drop feature that are non-informative; otherwise, lm sets coefficient to NA and predict fails
        maybe.formula <- MakeFormula( data = converted.data
                                     ,scope = scope
                                     ,response = response
                                     ,predictorsForm = predictorsForm
                                     ,predictorsName = predictorsName
                                     ,control = control
                                     )
        if (!maybe.formula$ok)
            return(list(ok = false, problem = maybe.formula$problem))
        maybe.fitted <-
            tryCatch( lm(formula = maybe.formula$value, data = converted.data)
                     ,warning = function(w) w
                     ,error = function(e) e
                     )
        if (inherits(maybe.fitted, 'lm'))
            list(ok = TRUE, value = maybe.fitted)
        else
            list(ok = FALSE, problem = paste0('Fit: ', maybe.fitted))
    }
    FitMemoised <- memoise(Fit)
    Predict <- function(fitted, query.transaction) {
        # return list $ok $value (maybe) $problem (maybe)
        maybe.predict <-
            tryCatch( predict(object = fitted, newdata = query.transaction)
                     ,warning = function(w) w
                     ,error = function(e) e
                     )
        if (inherits(maybe.predict, 'numeric')) 
            list(ok = TRUE, value = maybe.predict)
        else
            list(ok = FALSE, problem = paste0('Predict: ', maybe.predict))
    }
    FitPredict <- function(query.transaction) {
        saleDate <- query.transaction$saleDate
        maybe.fitted <- FitMemoised(saleDate)
        if (maybe.fitted$ok) {
            maybe.prediction <- Predict( fitted = maybe.fitted$value
                                        ,query.transaction = ConvertYearFeatures( data = query.transaction
                                                                                 ,saleDate = saleDate
                                                                                 )
                                        )
            return(maybe.prediction)
        } else {
            return(maybe.fitted)
        }
    }

    predictions <- as.double(rep(NA), nrow(query.transactions))
    for (query.index in 1:nrow(query.transactions)) {
        maybe.prediction <- FitPredict(query.transactions[query.index,])
        if (maybe.prediction$ok) {
            if (verbose) 
                Printf('prediction query.index %d value %f\n', query.index, maybe.prediction$value)
            predictions[[query.index]] <- maybe.prediction$value
        } else {
            Printf( 'prediction query.index %d failed: %s\n'
                   ,query.index
                   ,as.character(maybe.prediction$problem))
        }
    }
    predictions
}
Evaluate_10 <- function(scope, model, scenario, response
                       ,predictorsForm, predictorsName, ndays
                       ,c, ntree, mtry
                       ,data.training
                       ,query.transactions
                       ,control
                       ) {
    # Return evaluation of the specified model on the given training and test data
    # reduce over model

    predictions.raw <- 
        switch( model
               ,linear       = PredictLinear      ( scenario = scenario
                                                   ,ndays = ndays
                                                   ,data.training = data.training
                                                   ,query.transactions = query.transactions
                                                   ,scope = scope
                                                   ,response = response
                                                   ,predictorsForm = predictorsForm
                                                   ,predictorsName = predictorsName
                                                   ,control = control
                                                   )
               ,linearReg    = PredictLinearReg   ( scenario = scenario
                                                   ,ndays = ndays
                                                   ,query.transactions = query.transactions
                                                   ,c = c
                                                   ,data.training = data.training
                                                   ,scope = scope
                                                   ,response = response
                                                   ,predictorsForm = predictorsForm
                                                   ,predictorsName = predictorsName
                                                   ,control = contro
                                                   )
               ,randomForest = PredictRandomForest( scenario = scenario
                                                   ,ndays = ndays
                                                   ,query.transactions = query.transactions
                                                   ,scope = scope
                                                   ,ntree = ntree
                                                   ,mtry = mtry
                                                   ,data.training = data.training
                                                   ,scope = scope
                                                   ,response = response
                                                   ,predictorsForm = predictorsForm
                                                   ,predictorsName = predictorsName
                                                   ,control= control
                                                   )
               ,stop('bad model')
               )
    predictions <- if (response == 'logprice') exp(predictions.raw) else predictions.raw

    actuals <- query.transactions$price
    result <- EvaluatePredictions( prediction = predictions
                                  ,actual = actuals
                                  )
    result
}
Evaluate_11 <- function(scope, model, scenario, response
                       ,predictorsForm, predictorsName, ndays, query
                       ,c, ntree, mtry
                       ,data.training, data.testing
                       ,control) {
    # Return evaluation of the specified model on the given training and test data
    # Reduce over query

    num.test.transactions <- max( 1
                                 ,round(nrow(data.testing) / query)
                                 )
    which.test <- sample.int( n = nrow(data.testing)
                             ,size = num.test.transactions
                             ,replace = FALSE
                             )
    query.transactions <- data.testing[which.test,]
    Printf('will sample %d of %d test transactions\n', nrow(query.transactions), nrow(data.testing))
    result <- Evaluate_10( scope = scope
                          ,model = model
                          ,scenario = scenario
                          ,response = response
                          ,predictorsForm = predictorsForm
                          ,predictorsName = predictorsName
                          ,ndays = ndays
                          ,c = c
                          ,ntree = ntree
                          ,mtry = mtry
                          ,data.training = data.training
                          ,query.transactions = query.transactions
                          ,control = control
                          )
    result
}
Evaluate_12 <- function(scope, model, timePeriod, scenario, response
                        ,predictorsForm, predictorsName, ndays, query
                        ,c, ntree, mtry
                        ,data.training, data.testing
                        ,control) {
    # Return evaluation of the specified model on the given training and test data
    # Reduce over timePeriod
    if (timePeriod == '2008') {
        # just year 2008
        first.date <- as.Date('2008-01-01')
        last.date  <- as.Date('2008-12-31')
        is.training <- 
            data.training$saleDate >= first.date &
            data.training$saleDate <= last.date
        is.testing <- 
            data.testing$saleDate >= first.date &
            data.testing$saleDate <= last.date
    } else if (timePeriod == '2003on') {
        # after Jan 1 2003
        first.date <- as.Date('2003-01-01')
        is.training <- data.training$saleDate >= first.date
        is.testing  <- data.testing$saleDate  >= first.date
    } else {
        stop('bad timePeriod')
    }

    result <- Evaluate_11( scope = scope
                          ,model = model
                          ,scenario = scenario
                          ,response = response
                          ,predictorsForm = predictorsForm
                          ,predictorsName = predictorsName
                          ,ndays = ndays
                          ,query = query
                          ,c = c
                          ,ntree = ntree
                          ,mtry = mtry
                          ,data.training = data.training[is.training,]
                          ,data.testing = data.testing[is.testing,]
                          ,control = control
                          )
    result
}
EvaluateModelHp <- function(hp, data, is.testing, is.training, control) {
    # Return evaluation of the model specified in the hyperparameters using
    # the specified training and test data.
    
    # split the data into training and test folds

    data.training.fold <- data[is.training,]
    data.testing.fold <- data[is.testing, ]

    result <- Evaluate_12( scope = hp$scope
                ,model = hp$model
                ,timePeriod = hp$timePeriod
                ,scenario = hp$scenario
                ,response = hp$response
                ,predictorsForm = hp$predictorsForm
                ,predictorsName = hp$predictorsName
                ,ndays = hp$ndays
                ,query =  hp$query
                ,c = hp$c
                ,ntree = hp$ntree
                ,mtry = hp$mtry
                ,data.training = data.training.fold
                ,data.testing = data.testing.fold
                ,control = control
                )
    result
}
Main <- function(control, transaction.data.all.years) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    # build list of hyperparameters to be used in defining the models for CrossValidate
    # here there is only one model and the hyperparameters are from the command line options
    hps <- list(list( scope          = control$opt$scope
                     ,model          = control$opt$model
                     ,timePeriod     = control$opt$timePeriod
                     ,scenario       = control$opt$scenario
                     ,response       = control$opt$response
                     ,predictorsForm = control$opt$predictorsForm
                     ,predictorsName = control$opt$predictorsName
                     ,ndays          = control$opt$ndays
                     ,query          = control$opt$query
                     ,c              = control$opt$c
                     ,ntree          = control$opt$ntree
                     ,mtry           = control$opt$mtry
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

    cv.result <- CrossValidate( data = transaction.data.all.years
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
AllAlwaysPresent <- function(data) {
    # return TRUE iff there is no NA in the data frame
    na.found <- FALSE
    for (name in names(data)) {
        v <- data[[name]]
        na.indices <- is.na(v)
        if (sum(na.indices) > 0) {
            Printf('column %s has %d NA values\n', name, sum(na.indices))
            na.found <- TRUE
        }
    }
    !na.found
}
AllInformative <- function(data) {
    # return TRUE iff every column has more than one unique value
    found.just.1 <- FALSE
    for (name in names(data)) {
        v <- data[[name]]
        num.unique <- length(unique(v))
        stopifnot(num.unique > 0)
        if (num.unique == 1) {
            Printf('column %s has just 1 unique value\n', name)
            found.just.1 <- TRUE
        }
    }
    !found.just.1
}


################## EXECUTION STARTS HERE
clock <- Clock()

default.args <-
    list( scope          = 'global'
         ,model          = 'linear'
         ,timePeriod     = '2003on'
         ,scenario       = 'avm'
         ,response       = 'price'
         ,predictorsForm = 'level'
         ,predictorsName = 'alwaysNoCensus'
         ,ndays          = '120'
         ,query          = '100'
         ,c              = '0'
         ,ntree          = '0'
         ,mtry           = '0'
         )
control <- Control(default.args)

# cache transaction.data
if (!exists('transaction.data')) {
    transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                              ,split.names = control$split.names
                                              )
    stopifnot(AllAlwaysPresent(transaction.data))
    stopifnot(AllInformative(transaction.data))
}

Main(control, transaction.data)
if (control$testing)
    cat('TESTING: DISCARD RESULTS\n')
Printf('took %f CPU minutes\n', clock$Cpu() / 60)
Printf('took %f wallclock minutes\n', clock$Wallclock() / 60)
cat('done\n')
