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

# local models of various forms
source('PredictLinear.R')
source('PredictLinearReg.R')
source('PredictRandomForest.R')

source('Predictors2.R') 
source('ReadTransactionSplits.R')

library(lubridate)
library(memoise)
library(optparse)
library(pryr)

Counter <- function(initial.value = 0) {
    current.value <- initial.value

    Increment <- function() {
        current.value <<- current.value + 1
    }

    Get <- function() {
        current.value
    }

    list( Increment = Increment
         ,Get       = Get
         )
}

ReportMemoryUsage <- function(msg = '') {
    mem.used <- mem_used()
    Printf( '%s mem_used %f GB\n'
           ,msg
           ,as.numeric(mem.used) / 1e9
           )
}

MaybeReportMemoryUsage <- function(msg = '') {
    maybe <- TRUE
    if (maybe)
        ReportMemoryUsage(msg)
}

Control <- function(default.args) {
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
              |substr(opt$predictorsName, 1, 4) == 'best'
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
    years <- c('year.built', 'effective.year.built')

    testing <- FALSE
    #testing <- TRUE

    out.options <-
        sprintf( '%s_%s_%s_%s_%s_%s_%s_%d_%d_%d_%d_%d'
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
    out.base <- 'e-cv-cells/'

    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, out.options, '.log')
                    ,path.out.rdata = paste0(working, out.base, out.options, '.RData')
                    ,opt = opt
                    ,model.name = out.options
                    ,nfolds = if (testing) 2 else 10
                    ,split.names = unique(c( predictors
                                            ,identification
                                            ,prices
                                            ,years
                                            )
                    )
                    ,testing = testing
                    ,debug = FALSE
                    ,verbose.CrossValidate = TRUE
                    ,trace.memory.usage = TRUE
                    ,fold.counter = Counter()
                    ,me = me
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
Evaluate_10 <- function(scope, model, scenario, response
                       ,predictorsForm, predictorsName, ndays
                       ,c, ntree, mtry
                       ,data.training
                       ,query.transactions
                       ,control
                       ) {
    MaybeReportMemoryUsage('start Evaluate_10')
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
                                                   ,TrainingData = TrainingData
                                                   ,MakeFormula = MakeFormula
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
                                                   ,control = control
                                                   ,TrainingData = TrainingData
                                                   ,MakeFormula = MakeFormula
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
                                                   ,TrainingData = TrainingData
                                                   ,MakeFormula = MakeFormula
                                                   )
               ,stop('bad model')
               )
    predictions <- if (response == 'logprice') exp(predictions.raw) else predictions.raw

    actuals <- query.transactions$price
    result <- EvaluatePredictions( prediction = predictions
                                  ,actual = actuals
                                  )
    MaybeReportMemoryUsage('start Evaluate_10')
    result
}
Evaluate_11 <- function(scope, model, scenario, response
                       ,predictorsForm, predictorsName, ndays, query
                       ,c, ntree, mtry
                       ,data.training, data.testing
                       ,control) {
    MaybeReportMemoryUsage('start Evaluate_11')
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
    MaybeReportMemoryUsage('end Evaluate_11')
    result
}
Evaluate_12 <- function(scope, model, timePeriod, scenario, response
                        ,predictorsForm, predictorsName, ndays, query
                        ,c, ntree, mtry
                        ,data.training, data.testing
                        ,control) {
    MaybeReportMemoryUsage('start Evaluate_12')
    # Return evaluation of the specified model on the given training and test data
    # Reduce over timePeriod

    # eliminate data not know in timePeriod; Either:
    # - transaction not occurring in this time period; OR
    # - transactions with year.built or effective.year.built before the start of the time period
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
        is.training <- 
            data.training$saleDate >= first.date 
        is.testing  <- 
            data.testing$saleDate  >= first.date 
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
    MaybeReportMemoryUsage('end Evaluate_12')
    result
}
EvaluateModelHp <- function(hp, data, is.testing, is.training, control) {
    # Return evaluation of the model specified in the hyperparameters using
    # the specified training and test data.

    clock <- Clock()
    MaybeReportMemoryUsage(sprintf( 'start EvaluatModelHp fold %d'
                                   ,{control$fold.counter$Increment(); control$fold.counter$Get()}
                                   )
    )
    
    # split the data into training and test folds
    # also discard transactions with houses built after date of taxroll

    data.training.fold <- data[is.training, ]
    data.testing.fold <- data[is.testing, ]

    # eliminate house built or modified after the date of the taxroll

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
    MaybeReportMemoryUsage('end EvaluateModelHp')
    Printf( 'time for  fold %d:  %f CPU minutes %f elapsed minutes\n'
           ,control$fold.counter$Get()
           ,clock$Cpu() / 60
           ,clock$Wallclock() / 60
           )
    Printf(' model %s\n', control$model.name)
    result
}
Main <- function(control, transaction.data.all.years) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    # drop data the taxroll doesn't know about
    # NOTE: no observations are eliminated by this procedure
    # because the largest year.built and effective.year.built are 2008
    taxroll.year <- 2008
    is.known.to.taxroll <-
        transaction.data.all.years$year.built <= taxroll.year &
        transaction.data.all.years$effective.year.built <= taxroll.year
    data <-  transaction.data.all.years[is.known.to.taxroll, ]
    stopifnot(nrow(data) == nrow(transaction.data.all.years)) # no post-2008 transations should exist

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

    cv.result <- CrossValidate( data = data
                               ,nfolds = control$nfolds
                               ,EvaluateModel = EvaluateModel
                               ,model.name = model.name
                               ,control = control
                               ,verbose = control$verbose.CrossValidate
                               )

    cat('examine cv.result\n'); browser()

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
         ,response       = 'logprice'
         ,predictorsForm = 'level'
         ,predictorsName = 'best01'
         ,ndays          = '60'
         ,query          = '100'
         ,c              = '0'
         ,ntree          = '0'
         ,mtry           = '0'
         )
control <- Control(default.args)

# cache transaction.data
if (!exists('e-cv-transaction.data')) {
    e.cv.transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                                   ,split.names = control$split.names
                                              )
    stopifnot(AllAlwaysPresent(e.cv.transaction.data))
    stopifnot(AllInformative(e.cv.transaction.data))
}

Main( control = control
     ,transaction.data.all.years = e.cv.transaction.data
     )
if (control$testing)
    cat('TESTING: DISCARD RESULTS\n')
Printf('took %f CPU minutes\n', clock$Cpu() / 60)
Printf('took %f wallclock minutes\n', clock$Wallclock() / 60)
Printf('finished at %s\n', as.character(Sys.time()))
cat('done\n')
