# e-verify-assessment.R
# main program
# verify that many sales in Nov 2007 - Oct 2008 were for the assessed price

source('Directory.R')
source('Libraries.R')

#source('ModelLinearLocal2.R')
source('Predictors2.R') 
source('ReadTransactionSplits.R')

library(lubridate)
library(memoise)
library(optparse)

Control <- function(default.args) {
    # parse command line arguments in command.args
#    opt <- ParseCommandArgs( command.args = commandArgs(trailingOnly = TRUE)
#                            ,default.args = default.args
#                            )

    me <- 'e-verify-assessment' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    # define splits we use

    features <- c('price', 'total.assessment')
    identification <- Predictors2('identification')

    testing <- FALSE
    #testing <- TRUE

    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, me, '.log')
                    ,path.out.rdata = paste0(working, me, '.RData')
                    ,split.names = unique(c( features
                                            ,identification
                                            )
                    )
                    ,testing = FALSE
                    ,debug = FALSE
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
             ,OptionChr('timePeriod',     'one of {2003|2009}')
             ,OptionChr('scenario',       'one of {assessor|avm|mortgage}')
             ,OptionChr('response',       'one of {logprice|price}')
             ,OptionChr('predictorsForm', 'one of {level|log}')
             ,OptionChr('predictorsName', 'name of predictor set')
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
PredictLinear <- function(scenario, ndays, data.training, queries
                          ,scope, response, predictorsForm, predictorsName, control) {
    # Return evaluation of the specified model on the given training and test data
    # Return vector of predictions for the queries using a local model

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
    Predict <- function(fitted, query) {
        # return list $ok $value (maybe) $problem (maybe)
        maybe.predict <-
            tryCatch( predict(object = fitted, newdata = query)
                     ,warning = function(w) w
                     ,error = function(e) e
                     )
        if (inherits(maybe.predict, 'numeric')) 
            list(ok = TRUE, value = maybe.predict)
        else
            list(ok = FALSE, problem = paste0('Predict: ', maybe.predict))
    }
    FitPredict <- function(query) {
        saleDate <- query$saleDate
        maybe.fitted <- FitMemoised(saleDate)
        if (maybe.fitted$ok) {
            maybe.prediction <- Predict( fitted = maybe.fitted$value
                                        ,query = ConvertYearFeatures( data = query
                                                                     ,saleDate = saleDate
                                                                     )
                                        )
            return(maybe.prediction)
        } else {
            return(maybe.fitted)
        }
    }

    predictions <- as.double(rep(NA), nrow(queries))
    for (query.index in 1:nrow(queries)) {
        maybe.prediction <- FitPredict(queries[query.index,])
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
                       ,queries
                       ,control
                       ) {
    # Return evaluation of the specified model on the given training and test data
    # reduce over model

    predictions.raw <- 
        switch( model
               ,linear       = PredictLinear      ( scenario = scenario
                                                   ,ndays = ndays
                                                   ,data.training = data.training
                                                   ,queries = queries
                                                   ,scope = scope
                                                   ,response = response
                                                   ,predictorsForm = predictorsForm
                                                   ,predictorsName = predictorsName
                                                   ,control = control
                                                   )
               ,linearReg    = PredictLinearReg   ( scenario = scenario
                                                   ,ndays = ndays
                                                   ,queries = queries
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

    actuals <- queries$price
    result <- EvaluatePredictions(prediction = predictions
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
    queries <- data.testing[which.test,]
    Printf('will sample %d of %d test transactions\n', nrow(queries), nrow(data.testing))
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
                          ,queries = queries
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
    first.date <-
        switch( timePeriod
               ,'2009' = as.Date('2008-11-01')
               ,'2003' = as.Date('2003-01-01')
               ,stop('unimplemented opt$timePeriod')
               )

    is.training <- data.training$saleDate >= first.date
    is.testing <- data.testing$saleDate >= first.date

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
Main <- function(control, data) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    # determine fraction of transactions with zero error rate

    all.rows <- NULL
    for (year in 2006:2009) {
        for (month in 1:12) {
            in.month <-
                data$sale.year == year &
                data$sale.month == month
            if (sum(in.month) > 0) {
                data.month <- data[in.month,]
                error <- data.month$price - data.month$assessment
                fraction.error.zero <- sum(error == 0) / nrow(data.month)
                Printf( 'year %4d month %02d fraction error zero %f\n'
                       ,year
                       ,month
                       ,fraction.error.zero
                       )
                next.row <- data.frame( year = year
                                       ,month = month
                                       ,fraction.error.zero = fraction.error.zero
                                       )
                all.rows <- rbind(all.rows, next.row)
            }
        }
    }
    accuracy.by.month <- all.rows

    save( control
         ,accuracy.by.month
         ,file = control$path.out.rdata
         )

    str(control)
}


################## EXECUTION STARTS HERE
clock <- Clock()

default.args <- NULL
control <- Control(default.args)

# cache transaction.data
if (!exists('transaction.data')) {
    transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                              ,split.names = control$split.names
                                              )
    # add sale year and sale month
    splitDate <- SplitDate(transaction.data$recordingDate)
    data <- data.frame( sale.year = splitDate$year
                       ,sale.month = splitDate$month
                       ,price = transaction.data$price
                       ,assessment = transaction.data$total.assessment
                       )

}

Main(control, data)
if (control$testing)
    cat('TESTING: DISCARD RESULTS\n')
Printf('took %f CPU minutes\n', clock$Cpu() / 60)
Printf('took %f wallclock minutes\n', clock$Wallclock() / 60)
cat('done\n')
