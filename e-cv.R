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
# --scope {global, CCCCCC, ZZZZZ, CITYNAME} global/census tract/zip5/property city
# --model {linear, linearReg, randomForest}
# --timePeriod {2003|2009}
# --scenario {assessor|avm|mortgage}
# --response  {logprice|price}
# --predictorsForm {level|log}
# --predictorsName {always|alwaysNoAssessment|alwaysNoCensus}
# --ndays INT
# --query INT: 1 / INT fraction of query transactions in a fold that are used
# --lambda INT: (100 * lamba), the weight of the L2 regularizer
# --ntree INT
# --mtry  INT
# --fold  {all, combine, N} where N is 1, 2, .. 10
# --cvcell  PATH_TO_CELL, the file name has all the parameters

source('Directory.R')
source('Libraries.R')

source('Counter.R')
source('CvCell.R')        # machinery to manipule the e-cv-cells directory
source('Lines.R')
source('PredictLocal.R')  # all local models

source('Predictors2.R') 
source('ReadTransactionSplits.R')

library(lubridate)
library(MASS)
library(memoise)
library(optparse)
library(pryr)
library(randomForest)

PullOptionsFromCvCell <- function(path) {
    # return list of options
    split.1 <- strsplit( x = path
                        ,split = '/'
                        ,fixed = TRUE
                        )
    split.1.1 <- split.1[[1]]
    split.2 <- strsplit( x = split.1.1
                        ,split = '_'
                        ,fixed = TRUE
                        )
    piece <- split.2[[1]]
    result <- list( scope = piece[[1]]
                   ,model = piece[[2]]
                   ,timePeriod = piece[[3]]
                   ,scenario = piece[[4]]
                   ,response = piece[[5]]
                   ,predictorsName = piece[[6]]
                   ,predictorsForm = piece[[7]]
                   ,ndays = as.numeric(piece[[8]])
                   ,query = as.numeric(piece[[9]])
                   ,lambda = as.numeric(piece[[10]])
                   ,ntree = as.numeric(piece[[11]])
                   ,mtry = as.numeric(piece[[12]])
                   ,fold = 'all'
                   )
    result
}
Control <- function(default.args) {
    # parse command line arguments in command.args
    opt.raw <- ParseCommandArgs( command.args = commandArgs(trailingOnly = TRUE)
                                ,default.args = default.args
                                )
    
    opt <- 
        if (nchar(opt.raw$cvcell)  > 0)
            opt <- PullOptionsFromCvCell(opt.raw$cvcell)
        else
            opt.raw

    me <- 'e-cv' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    # validate each command arg
    IsPositive1To10 <- function(i) {
        i >= 1 && i <= 10
    }

    stopifnot(opt$ndays > 0)
    stopifnot( opt$predictorsName == 'always'
              |opt$predictorsName == 'alwaysNoAssessment'
              |opt$predictorsName == 'alwaysNoCensus'
              |substr(opt$predictorsName, 1, 4) == 'best'
              |substr(opt$predictorsName, 1, 3) == 'pca'
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
    if (opt$fold == 'all' | opt$fold == 'combine') {
        # do nothing, value is OK
    } else {
        i.fold <- as.integer(opt$fold)
        if (i.fold >= 1 | i.fold <= 10) {
            # do nothing, value is OK
        } else {
            stop(paste0('bad fold option = ', as.character(opt$fold)))
        }
    }

    # define splits we use
    predictors <- Predictors2( predictors.name = opt$predictorsName
                              ,predictors.form = opt$predictorsForm
                              )
    identification <- Predictors2('identification')
    prices <- c('price.log', 'price')
    years <- c('year.built', 'effective.year.built')

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
                ,opt$lambda
                ,opt$ntree
                ,opt$mtry
                )
    out.base <- 'e-cv-cells/'

    # read in the identifiers for the submarkets
    loaded <- load(file = paste0(working, 'submarkets.RData'))
    stopifnot(!is.null(codes.census.tract))
    stopifnot(!is.null(codes.property.city))
    stopifnot(!is.null(codes.zip5))

    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, out.options, '.log')
                    ,path.out.rdata = paste0(working, out.base, out.options, '.RData')
                    ,codes.census.tract = codes.census.tract
                    ,codes.property.city = codes.property.city
                    ,codes.zip5 = codes.zip5
                    ,opt = opt
                    ,fold.option = switch( opt$fold
                                          ,all = 'all'
                                          ,combine = 'combine'
                                          ,as.integer(opt$fold)
                                          )
                    ,model.name = out.options
                    ,nfolds = 10
                    ,split.names = unique(c( predictors
                                            ,identification
                                            ,prices
                                            ,years
                                            )
                    )
                    ,testing = FALSE
                    ,debug = FALSE
                    ,verbose.CrossValidate = TRUE
                    ,trace.memory.usage = TRUE
                    ,fold.counter = Counter()
                    ,me = me
                    ,master.clock = Clock()
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
        list( OptionChr('scope',          'one of {global, CCCCCC, ZZZZZZ, CITYNAME}')
             ,OptionChr('model',          'one of {linear, linL2, rf}')
             ,OptionChr('timePeriod',     'one of {2003on|2008}')
             ,OptionChr('scenario',       'one of {assessor|avm|mortgage}')
             ,OptionChr('response',       'one of {logprice|price}')
             ,OptionChr('predictorsForm', 'one of {level|log}')
             ,OptionChr('predictorsName', 'one of {always|alwaysNoAssessment|alwaysNoCensus')
             ,OptionInt('ndays',          'number of days in training period')
             ,OptionInt('query',          ' 1 / <fraction of test sample used as queries>')
             ,OptionInt('lambda',         '(100 * lamdda) for regularized regression')
             ,OptionInt('ntree',          'number of trees (for randomForest)')
             ,OptionInt('mtry',           'number of features samples when growing tree (for randomForest)')
             ,OptionChr('fold',           'one of {all, combine, N in 1..10}')
             ,OptionChr('cvcell',         'path to a cell, with other parameters in file name')
             )

    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
    opt
}
ReportMemoryUsage <- function(msg = '') {
    mem.used <- mem_used()
    Printf( '%s mem_used %f GB\n'
           ,msg
           ,as.numeric(mem.used) / 1e9
           )
}
MaybeReportMemoryUsage <- function(msg = '') {
    maybe <- FALSE
    if (maybe)
        ReportMemoryUsage(msg)
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
                   ,prediction = prediction
                   ,actual = actual
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
MakeFormula <- function(data, response, predictorsForm, predictorsName, control) {
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
Evaluate_9 <- function( model, scenario, response
                       ,predictorsForm, predictorsName, ndays
                       ,lambda, ntree, mtry
                       ,data.training
                       ,query.transactions
                       ,control
                       ) {
    MaybeReportMemoryUsage('start Evaluate_9')
    # Return evaluation of the specified model on the given training and test data
    # reduce over model
    Printf('model %s nrow(data.training) %d nrow(query.transactions) %d\n'
           ,model
           ,nrow(data.training)
           ,nrow(query.transactions)
           )

    # linear regression (OLS)
    ModelLinear.Fit <- function(formula, data, fit.model.data) {
        stopifnot(is.null(fit.model.data))
        result <- lm( formula = formula
                     ,data = data
                     )
        result
    }
    ModelLinear.Predict <- function(object, newdata) {
        result <- predict( object = object
                          ,newdata = newdata
                          )
        result
    }


    # ridge regression (L2 regularizer)
    ModelLinL2.Fit <- function(formula, data, fit.model.data) {
        # the lambda value is from the command line
        # it is 100x too big
        #cat('in ModelLinL2.fit\n'); browser()

        lambda <- fit.model.data$lambda / 100
        result <- lm.ridge( formula = formula
                           ,data = data
                           ,lambda = lambda
                           )
        result
    }

    ModelLinL2.Predict <- function(object, newdata) {
        #cat('in ModelLinL2.Predict\n'); browser()
        verbose <- FALSE
        if (verbose) str(newdata)

        StartsWith <- function(s, p) {
            # return TRUE iff string s starts with p
            nc.p <- nchar(p)
            substr(s, 1, nc.p) == p
        }
        EndsWithTRUE <- function(s) {
            # return TRUE iff string s is of form '...TRUE'.
            nc <- nchar(s)
            substr(s, nc - 3, nc) == 'TRUE'
        }
        BeforeTRUE <- function(s) {
            # assuming s is of form '...TRUE', return the '...' portion
            nc <- nchar(s)
            substr(s, 1, nc - 4)
        }
        After <- function(s, prefix) {
            # return portion of string s after the prefix
            nc.prefix <- nchar(prefix)
            substr(s, nc.prefix + 1, nchar(s))
        }

        coe <- coef(object)
        Coef <- function(name) {
            if (name == '')
                coe[[1]]
            else
                coe[[name]]
        }
        QueryValue <- function(name) {
            debug <- FALSE
            if (verbose) cat('in QueryValue name', name, '\n')
            if (name == '') {
                # intercept always has value 1
                result <- 1
                if (verbose) Printf('intercept result %f\n', result)
                result
            } else if (EndsWithTRUE(name)) {
                result <- newdata[[BeforeTRUE(name)]] == TRUE
                if (verbose) Printf('end with TRUE %s result %f\n', name, result)
                #browser()
                result
            } else if (StartsWith(name, 'zip5')) {
                value <- After(name, 'zip5')
                result <- newdata[['zip5']] == value
                if (verbose) Printf('starts with zip5 %s value %s result %f\n', name, value, result)
                if (debug && result == 1) browser()
                result
            } else if (StartsWith(name, 'census.tract')) {
                value <- After(name, 'census.tract')
                result <- newdata[['census.tract']] == value
                if (verbose) Printf('starts with census.tract %s value %s result %f\n', name, value, result)
                if (debug && result == 1) browser()
                result
            } else if (StartsWith(name, 'property.city')) {
                value <- After(name, 'property.city')
                result <- newdata[['property.city']] == value
                if (verbose) Printf('starts with property.city %s value %s result %f\n', name, value, result)
                if (debug && result == 1) browser()
                result
            } else {
                result <- newdata[[name]]
                if (verbose) Printf('result %f\n', result)
                result
            }
        }
        terms <- sapply( names(coe)
                        ,function(name)
                            Coef(name) * QueryValue(name)
                        )
        ridge.predict <- sum(unname(terms))
        ridge.predict
    }

    # random forests
    ModelRandomForest.Fit <- function(formula, data, fit.model.data) {
        # the lambda value is from the command line
        # it is 100x too big
        #cat('in ModelRandomForest.Fit\n'); browser()

        ntree <- fit.model.data$ntree
        mtry <- fit.model.data$mtry

        result <- randomForest( formula = formula
                               ,data = data
                               ,ntree = ntree
                               ,mtry = mtry
                               )
        result
    }

    ModelRandomForest.Predict <- function(object, newdata) {
        #cat('in ModelRandomForest.Predict\n'); browser()
        result <- predict( object = object
                          ,newdata = newdata
                          )
        result
    }

    # EXECUTION STARTS HERE

    predictions.raw <- 
        PredictLocal( scenario = scenario
                     ,ndays = ndays
                     ,data.training = data.training
                     ,query.transactions = query.transactions
                     ,response = response
                     ,predictorsForm = predictorsForm
                     ,predictorsName = predictorsName
                     ,control = control
                     ,TrainingData = TrainingData
                     ,MakeFormula = MakeFormula
                     ,FitModel = switch( model
                                        ,linear = ModelLinear.Fit
                                        ,linL2  = ModelLinL2.Fit
                                        ,rf = ModelRandomForest.Fit
                                        )
                     ,fit.model.data = switch( model
                                              ,linear = NULL
                                              ,linL2 = list(lambda = lambda)
                                              ,rf = list( ntree = as.numeric(ntree)
                                                         ,mtry = as.numeric(mtry)
                                                         )
                                              )
                     ,PredictModel = switch( model
                                            ,linear = ModelLinear.Predict
                                            ,linL2  = ModelLinL2.Predict
                                            ,rf = ModelRandomForest.Predict
                                            )
                     ,model.name = control$model.name
                     ,fold.number = control$fold.counter$Get()
                     )

    predictions <- if (response == 'logprice') exp(predictions.raw) else predictions.raw

    actuals <- query.transactions$price
    print(predictions)
    print(actuals)
    result <- EvaluatePredictions( prediction = predictions
                                  ,actual = actuals
                                  )
    MaybeReportMemoryUsage('end Evaluate_9')
    result
}
Evaluate_10 <- function( model, scenario, response
                        ,predictorsForm, predictorsName, ndays, query
                        ,lambda, ntree, mtry
                        ,data.training, data.testing
                        ,control) {
    MaybeReportMemoryUsage('start Evaluate_10')
    # Return evaluation of the specified model on the given training and test data
    # Reduce over query

    num.test.transactions <- 
        if (query == 1)
            nrow(data.testing)
        else
            max( 1
                ,round(nrow(data.testing) / query)
                )

    if (num.test.transactions == 0)
        return(EvaluatePredictions( prediction = NA
                                   ,actual = NA
                                   )
    )
    which.test <- sample.int( n = nrow(data.testing)
                             ,size = num.test.transactions
                             ,replace = FALSE
                             )
    query.transactions <- data.testing[which.test,]
    Printf('will sample %d of %d test transactions\n', nrow(query.transactions), nrow(data.testing))
    result <- Evaluate_9( model = model
                         ,scenario = scenario
                         ,response = response
                         ,predictorsForm = predictorsForm
                         ,predictorsName = predictorsName
                         ,ndays = ndays
                         ,lambda = lambda
                         ,ntree = ntree
                         ,mtry = mtry
                         ,data.training = data.training
                         ,query.transactions = query.transactions
                         ,control = control
                         )
    MaybeReportMemoryUsage('end Evaluate_10')
    result
}
Evaluate_11 <- function( model, timePeriod, scenario, response
                        ,predictorsForm, predictorsName, ndays, query
                        ,lambda, ntree, mtry
                        ,data.training, data.testing
                        ,control) {
    MaybeReportMemoryUsage('start Evaluate_11')
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

    result <- Evaluate_10( model = model
                          ,scenario = scenario
                          ,response = response
                          ,predictorsForm = predictorsForm
                          ,predictorsName = predictorsName
                          ,ndays = ndays
                          ,query = query
                          ,lambda = lambda
                          ,ntree = ntree
                          ,mtry = mtry
                          ,data.training = data.training[is.training,]
                          ,data.testing = data.testing[is.testing,]
                          ,control = control
                          )
    MaybeReportMemoryUsage('end Evaluate_11')
    result
}
SqueezeBlanks <- function(s) {
    # return chr s, but with all blanks removed
    gsub(' ', '', s)
}
Evaluate_12 <- function(scope, model, timePeriod, scenario, response
                        ,predictorsForm, predictorsName, ndays, query
                        ,lambda, ntree, mtry
                        ,data.training, data.testing
                        ,control) {
    # reduce over scope
    # sample values
    #   census.tract  535000
    #   property.city WATTS
    #   zip5          90001

    MaybeReportMemoryUsage('start Evaluate_12')

    if (scope == 'global')  {

        scope.data.training <- data.training
        scope.data.testing <- data.testing

    } else if (scope %in% control$codes.census.tract) {

        scope.data.training <- data.training[data.training$census.tract == scope, ]
        scope.data.testing <- data.testing[data.testing$census.tract == scope, ]

    } else if (scope %in% control$codes.property.city) {

        # squeeze out the blanks in property city names
        # the restatement of the factor levels looses the original factor level "SUN VALLEY"
        data.training$property.city <- factor(SqueezeBlanks(as.character(data.training$property.city)))
        # the restatement of the factor levels looses about 20 original factor levels
        data.testing$property.city <- factor(SqueezeBlanks(as.character(data.testing$property.city)))

        scope.data.training <- data.training[data.training$property.city == scope, ]
        scope.data.testing <- data.testing[data.testing$property.city == scope, ]

    } else if (scope %in% control$codes.zip5) {

        scope.data.training <- data.training[data.training$zip5 == scope, ]
        scope.data.testing <- data.testing[data.testing$zip5 == scope, ]

    } else {

        print(scope)
        stop('bad scope')

    }

    # check that some observations survived
    n.scope.data.training <- nrow(scope.data.training)
    n.scope.data.testing  <- nrow(scope.data.testing)
    Printf('scope %s retained %d testing %d training observations\n'
           ,scope
           ,n.scope.data.training
           ,n.scope.data.testing
           )
    if (n.scope.data.training == 0 ||
        n.scope.data.testing  == 0) {
        return(EvaluatePredictions( prediction = NA
                                   ,actual = NA
                                   )
        )
    }

    result <- Evaluate_11( model = model
                          ,timePeriod = timePeriod
                          ,scenario = scenario
                          ,response = response
                          ,predictorsForm = predictorsForm
                          ,predictorsName = predictorsName
                          ,ndays = ndays
                          ,query = query
                          ,lambda = lambda
                          ,ntree = ntree
                          ,mtry = mtry
                          ,data.training = scope.data.training
                          ,data.testing = scope.data.testing
                          ,control = control
                          )
    MaybeReportMemoryUsage('end Evaluate_12')
    result
}
EvaluateModelHp <- function(hp, data, is.testing, is.training, my.control) {
    # Return evaluation of the model specified in the hyperparameters using
    # the specified training and test data.

    clock <- Clock()
    MaybeReportMemoryUsage(sprintf( 'start EvaluatModelHp fold %d'
                                   ,{control$fold.counter$Increment(); control$fold.counter$Get()}
                                   )
    )

    # handle --fold command line parameter, which is in control$fold.option

    my.control$fold.counter$Increment()
    fold.counter <- my.control$fold.counter$Get()
    fold.option <- my.control$fold.option
    #cat('fold.option', fold.option, 'fold.counter', fold.counter, '\n'); browser()

    if (fold.option == 'all' ) {
        # do nothing special, just continue processing
    } else if (fold.option == 'combine') {
        stop('combine option should be handled by caller')
    } else if (is.integer(fold.option)) {
        if (fold.option == fold.counter) {
            # do nothing special, just continue processing
        } else {
            result <- NULL
            return(result)
        }
    } else {
        stop(paste0('bad fold.option: ', as.character(control$fold.option)))
    }
    
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
                          ,lambda = hp$lambda
                          ,ntree = hp$ntree
                          ,mtry = hp$mtry
                          ,data.training = data.training.fold
                          ,data.testing = data.testing.fold
                          ,control = control
                          )
    MaybeReportMemoryUsage('end EvaluateModelHp')
    Printf( 'cumulative time for  fold %d:  %f CPU minutes %f elapsed minutes\n'
           ,control$fold.counter$Get()
           ,control$master.clock$Cpu() / 60
           ,control$master.clock$Wallclock() / 60
           )
    Printf(' model %s\n', control$model.name)
    #cat('finished fold\n'); browser()
    result
}
CombineFoldResults <- function(expected.model.name, path.out.rdata) {
    # return precompute result from 10 fold files
    #cat('about to combine\n'); browser()
    my.cv.result <- NULL
    for (fold.number in 1:10) {
        file.path <- PathToFold(path.out.rdata, fold.number)
        loaded <- load(file = file.path) 
        # check that the cv.result has only one model, namely the expected model
        stopifnot(length(names(cv.result)) == 1)
        stopifnot(expected.model.name == names(cv.result) [[1]])
        fold.cv.result <- cv.result[[expected.model.name]] [[fold.number]]
        my.cv.result[[expected.model.name]] [[fold.number]] <- fold.cv.result
    }
    #cat('examine my.cv.result\n'); browser()
    my.cv.result
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

    # perform cross validation for model in hps and save results

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
                     ,lambda         = control$opt$lambda
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


    cv.result <- 
        if (control$fold.option == 'combine') {
            CombineFoldResults(control$model.name, control$path.out.rdata)
        } else {
            CrossValidate( data = data
                          ,nfolds = control$nfolds
                          ,EvaluateModel = EvaluateModel
                          ,model.name = model.name
                          ,control = control
                          ,verbose = control$verbose.CrossValidate
                          )
        }

    #cat('examine cv.result\n'); browser()

    file.path <- 
        if (is.integer(control$fold.option))
            PathToFold(control$path.out.rdata, control$fold.option)
        else
            control$path.out.rdata

    save( control
         ,cv.result
         ,model.name
         ,file = file.path
         )

    str(control)
}
PathToFold <- function(path.out.rdata, fold.option) {
    result <- paste0(path.out.rdata, '.fold_', as.character(fold.option))
    result
}

################## EXECUTION STARTS HERE
clock <- Clock()

# scope / number occurrences
# 535000 60
# 703200 1453
# 195100 1505
# 141600 2113
# 550600 2277
# 573600 2379
# 464100 2544
#
# LOSANGELES 135985
# WATTS      1
# BEVERLYHILLS 2059
#
# 90013 2
# 90022 3029
# 90032 5141
# 90042 8207
# 90650 11367
# 90808 13834
# 91767 13882
default.args <-
    list(#scope          = 'global'
         #scope          = '535000'  # census tract 60 observations
         #scope          = '464100'  # census tract 60 observations
         #scope          = 'LOSANGELES' # property.city 135985 observations
         #scope          = 'WATTS'      # property.city 1 observations
         #scope          = '90013'      # zip5 2 observations
         #scope          = '91767'      # zip5 13882 observations
         #scope          = '910706'
         #scope          = '91750'
          scope          = 'global'
         ,model          = 'linL2'
         ,timePeriod     = '2003on'
         ,scenario       = 'avm'
         ,response       = 'logprice'
         ,predictorsName = 'best15census'
         ,predictorsForm = 'level'
         ,ndays          = '30'
         ,query          = '1'
         ,lambda         = '5500'
         ,ntree          = '0'
         ,mtry           = '0'
         ,fold           = 'combine'   # must be 'all' for general use from the command line
         )
default.args <-
    list( scope          = ''
         ,model          = ''
         ,timePeriod     = ''
         ,scenario       = ''
         ,response       = ''
         ,predictorsName = ''
         ,predictorsForm = ''
         ,ndays          = ''
         ,query          = ''
         ,lambda         = ''
         ,ntree          = ''
         ,mtry           = ''
         ,fold           = 'all'  # must be 'all' for general use from the command line
         )
#default.args <-
#    list( cvcell = 
#          'global_rf_2003on_avm_logprice_best08_level_58_100_0_495_6'
#         )
control <- Control(default.args)

# cache transaction.data
if (!exists('e-cv-transaction.data')) {
    e.cv.transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                                   ,split.names = control$split.names
                                              )
    stopifnot(AllAlwaysPresent(e.cv.transaction.data))
    stopifnot(AllInformative(e.cv.transaction.data))
    # convert some numeric location features into factors
    zip5.factor <- as.factor(e.cv.transaction.data$zip5)
    census.tract.factor <- as.factor(e.cv.transaction.data$census.tract)
    e.cv.transaction.data$zip5 <- zip5.factor
    e.cv.transaction.data$census.tract <- census.tract.factor
    stopifnot(is.factor(e.cv.transaction.data$property.city))
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
