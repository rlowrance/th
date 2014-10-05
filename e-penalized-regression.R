# e-penalized-regression.R
# main program
# Determine best penalized regression model for log-level AMV with 90
# days of training data.
#
# For now, just implement lasso.
#
# Command line arguments
# --which             : one of {cv, charts, both}; default both
#                       cv means just do cross validation (write e-forms*.RData)
#                       chart means just produce chart output (write e-forms*.txt)
# --query.fraction    : number
#                       fraction of samples in test period to use
# Approach: Use k-fold cross validation to compare estimated generalization errors for
# model variants

source('Directory.R')

source('Libraries.R')

source('ModelLinearLocal.R')
source('ReadTransactionSplits.R')

library(elasticnet)
library(memoise)
library(optparse)

Control <- function(command.args) {
    # parse command line arguments in command.args
    opt <- ParseCommandArgs(command.args)

    me <- 'e-penalized-regression' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    # defines the splits that we use
     predictors.level = c( # continuous size positive
                          'land.square.footage'
                          ,'living.area'
                           # continuous size nonnegative
                          ,'bedrooms'
                          ,'bathrooms'
                          ,'parking.spaces'
                           # continuous non size
                          ,'median.household.income'
                          ,'year.built'
                          ,'fraction.owner.occupied'
                          ,'avg.commute.time'
                           # discrete
                          ,'factor.is.new.construction'
                          ,'factor.has.pool'
                          )
    other.names = c(# dates
                    'saleDate'
                    ,'recordingDate'
                    # prices
                    ,'price'   # NOTE: MUST HAVE THE PRICE
                    ,'price.log'
                    # apn
                    ,'apn'
                    )
    formula <- Formula( predictors = predictors.level
                       ,response = 'price.log'
                       )
    testing <- FALSE
    #testing <- TRUE
    out.base <- sprintf('%s--query.fraction-%f'
                        ,me
                        ,opt$query.fraction
                        )
    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, '--which-', opt$which, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,path.out.chart1 = paste0(working, out.base, '.txt')
                    ,query.fraction = opt$query.fraction
                    ,num.training.days = 90
                    ,predictors.level = predictors.level
                    #,predictors.log = predictors.log
                    #,response.level = 'price'
                    ,response.log = 'price.log'
                    ,formula = formula
                    ,split.names = unique(c(predictors.level, other.names))
                    ,nfolds = 10
                    ,testing.period = list( first.date = as.Date('1984-02-01')
                                           ,last.date = as.Date('2009-03-31')
                                           )
                    ,chart1.format.line.d = '%30s %d'
                    ,chart1.format.line.f = '%30s %f'
                    ,testing = testing
                    ,debug = TRUE
                    ,which = opt$which
                    )
    control
}
ParseCommandArgs <- function(command.args) {
    # return name list of values from the command args
    opt.which <- make_option( opt_str = c('--which')
                             ,action = 'store'
                             ,type = 'character'
                             ,default = 'both'
                             ,help = 'which action to take'
                             )
    opt.query.fraction <- make_option( opt_str = c('--query.fraction')
                                          ,action = 'store'
                                          ,type = 'double'
                                          ,default = .01
                                          ,help = 'fraction of samples used as queries'
                                          )
    option.list <- list( opt.which
                        ,opt.query.fraction
                        )
    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
}
Chart1Body <- function(control, cv.result, ordered.features) {
    # return a vector lines, the body of chart 1
    #cat('Chart1Body\n'); browser()
    FeatureNames <- function() {
        c( 'Feature names ordered by importance'
          ,'1 == most important'
          ,sapply( 1:length(ordered.features)
                  ,function(index) 
                      sprintf(' %2d: %s'
                              ,index
                              ,ordered.features[[index]]
                              )
                  )
          )
    }
    FeaturePerformance <- function() {
        RMedianSE <- function(model.index) {
            model.result <- cv.result[[model.index]]
            rMedianSE.values <- sapply( 1:length(model.result)
                                       ,function(fold.index) {
                                           evaluate <- model.result[[fold.index]]
                                           evaluate$rootMedianSquaredError
                                       }
                                       )
            result <- median(rMedianSE.values)
            result
        }
        sapply( 1:length(cv.result)
               ,function(model.index) {
               sprintf( ' RMedianSE for features 1 through %2d = %f'
                       ,model.index
                       ,RMedianSE(model.index)
                       )
               }
               )
    }
    result <- c( FeatureNames()
                ,' '
                ,FeaturePerformance()
                )

}
Chart1Heading <- function(control) {
    description <- c( sprintf( 'cv.result of %f Percent Random Sample of Training Transactions'
                              ,control$query.sample * 100
                              )
                     ,'AVM scenario'
                     ,'Log-linear form'
                     ,sprintf( 'Testing period: %s through %s'
                              ,control$testing.period$first.date
                              ,control$testing.period$last.date
                              )
                     ,sprintf( 'Number of training days: %d'
                              ,control$num.training.days
                              )
                     )
}
Chart1 <- function(control, cv.result, ordered.features) {
    chart1 <- c( Chart1Heading(control)
                ,' '
                ,Chart1Body(control, cv.result, ordered.features)
                )
}
ModelAvmLinearLocal <- function(queries, data.training, formula, num.training.days) {
    # return vector of predictions from local AVM model trained for each query
    #cat('start ModelAvmLinearLocal\n'); browser()
    InTraining <- function(saleDate) {
        # return selector vector to identify training samples for the saleDate
        in.training <- data.training$saleDate >= (saleDate - num.training.days) &
                       data.training$saleDate <= (saleDate - 1)

    }
    result <- ModelLinearLocal( InTraining
                               ,queries
                               ,data.training
                               ,formula
                               )
    result
}
Queries <- function(data, control) {
    # return dataframe of queries from the testing data
    #cat('start Queries\n'); browser()
    ok.testing.first.date <- data$saleDate >=  control$testing.period$first.date
    ok.testing.last.date <- data$saleDate <= control$testing.period$last.date
    is.query <- ok.testing.first.date & ok.testing.last.date
    queries <- data[is.query, ]
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
FitPredictAvmLogLevel <- function(num.training.days, data, is.testing, is.training, query.fraction, control) {
    # return evaluation of the predicted vs. actual values
    #cat('start FitPredictAvmLogLevel\n'); browser()
    verbose <- TRUE
    response <- control$response.log
    predictors <- control$predictors.level
    queries.all <- Queries(data[is.testing, ], control)

    # sample the possible queries
    r <- runif( n = nrow(queries.all)
               ,min = 0
               ,max = 1)
    use.query <- ifelse( r < query.fraction
                        ,TRUE
                        ,FALSE
                        )
    queries <- queries.all[use.query, ]

    data.training <- data[is.training, ]

    model.result <- ModelAvmLinearLocal( queries = queries
                                        ,data.training = data.training
                                        ,formula = control$formula
                                        ,num.training.days = num.training.days
                                        )
    # model.result is a list $predictions $problems $queries
    prediction.raw <- model.result$prediction
    prediction <- if (response == 'price') prediction.raw else exp(prediction.raw)

    # determine error rates and coverage
    actual <- queries$price
    result <- Evaluate( actual = actual
                       ,prediction = prediction
                       )
    if (verbose) {print('FitPredictAvm'); str(result)}
    result
}
DetermineBestTrainingPeriod <- function(query, data.training, control) {
    # determine best training period for the query transaction 
    # approach: define comparables; for them, determine best training period as the training period
    #           that has the lowest RMedianSE
    # return list
    # $best.num.training.days: best number of training days for the comparison or NA
    # $num.days              : number of weeks of prior transactions 
    # $comparison.size       : number of elements in the comparison set
    #cat('DetermineBestTrainingPeriod\n'); browser()

    if (FALSE && control$debug) {
        query.date <- query$saleDate
        if (query.date != as.Date('1986-03-15')) {
            Printf('skipping %s\n', query.date)
            result <- list( best.num.training.days = 90
                           ,num.days = 1
                           ,comparison.size = 1
                           )
            return(result)
        }
    }
   
    # determine number of days in the comparables period
    num.days <- 1  
    repeat {
        last.test.date <- query$saleDate - 1
        first.test.date <- last.test.date - num.days + 1
        in.test.set <- data.training$saleDate >= first.test.date & 
                       data.training$saleDate <= last.test.date

        if (sum(in.test.set) >= control$num.test.samples.needed) break
        num.days <- num.days + 1
        stopifnot(num.days <= control$max.num.days)  # prevent runaway
    }

    test <- data.training[in.test.set, ]
    train <- data.training[!in.test.set, ]

    stopifnot(nrow(train) > 0)
    stopifnot(nrow(test) > 0)

    RMSE <- function(num.training.days) {
        # return square root of median squared error when using specified number of training days
        #cat('RMSE\n'); browser()
        model.result <- ModelAvmLinearLocal( queries = test
                                            ,data.training = train
                                            ,formula = control$formula
                                            ,num.training.days = num.training.days
                                            )
        prediction = exp(model.result$prediction)
        actual <- test$price
        evaluate <- Evaluate( actual = actual
                             ,prediction = prediction
                             )
        evaluate$rootMedianSquaredError
    }

    num.training.days <- 
        if (control$testing) c(30, 90)
        else c(30, 60, 90, 120, 150, 180)

    rMedianSE <- sapply(num.training.days, RMSE)

    best.index <- which.min(rMedianSE)
    if (control$debug) {
        print(query)
        print(rMedianSE)
        print(best.index)
        print(num.training.days)
        cat('next statement has failed before\n')
        #browser()
    }
    best.num.training.days <- 
        if (length(best.index) == 1) 
            num.training.days[[best.index]]
        else
            NA
    Printf( 'query date %s best.num.training.days %d test days %d nrow test %d train %d\n'
           ,query$saleDate
           ,best.num.training.days
           ,num.days
           ,nrow(test)
           ,nrow(train)
           )
    result <- list( best.num.training.days = best.num.training.days
                   ,num.days = num.days
                   ,comparison.size = nrow(test)
                   ,query.date = query$saleDate
                   )
    result
}
Predict <- function(query, data.training, control) {
    # predict one query point
    # return list
    # $ prediction     : predicted value
    # $ num.days       : number of weeks for comparables to query
    # $ comparison.size: number of comparison transactions used
    # $ query.date     : Date of the query
    # return prediction for one query point
    #cat('Predict\n'); browser()
    
    dbtp <- DetermineBestTrainingPeriod( query = query
                                        ,data.training = data.training
                                        ,control = control
                                        )
    prediction <-
        if (is.na(dbtp$best.num.training.days)) {
            NA
        } else {
            mall <- ModelAvmLinearLocal( queries = query
                                        ,data.training = data.training
                                        ,formula = control$formula
                                        ,num.training.days = dbtp$best.num.training.days
                                        )
            exp(mall$predictions)
        }
    result <- list( prediction = prediction
                   ,num.days = dbtp$num.days
                   ,comparison.size = dbtp$comparison.size
                   ,best.num.training.days = dbtp$best.num.training.days
                   ,query.date = query$saleDate
                   )
    result
}
MakeX <- function(data, control) {
    # return matrix containing just the predictor columns
    predictor.df <- data[, control$predictors.level]
    result <- data.matrix(predictor.df)
    result
}
MakeY <- function(data, control) {
    result <- data[, control$response.log]
    result
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
                            ,formula = Formula( response = control$response.log
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
Analysis <- function(control, transaction.data) {
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
Charts <- function(my.control) {
    # produce all the charts
    # for now, there is only one
    #cat('starting Charts\n'); browser()

    # recover cetain values from the predictions
    cv.result <- NULL
    loaded <- load(file = my.control$path.out.rdata)
    str(loaded)  # NOTE: control has been replaced
    stopifnot(!is.null(cv.result))
    stopifnot(!is.null(ordered.features))

    # produce charts
    # use the controls from when data were created
    chart1 <- Chart1(control, cv.result, ordered.features)
                            
    writeLines( text = chart1
               ,con = my.control$path.out.chart1
               )
    print(chart1)

    print(elapsed.message)  # put the elapsed time from the Analysis phase in the final version of the log

    # save results from the analysis pass and this pass
    save( control
         ,cv.result
         ,ordered.features
         ,elapsed.cpu
         ,elapsed.message
         ,chart1
         ,file = my.control$path.out.rdata
         )
}
Both <- function(control, transaction.data) {
    evaluation <- Analysis(control, transaction.data)
    Charts(control)
}
Main <- function(control, transaction.data) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    switch( control$which
           ,analysis = Analysis(control, transaction.data)
           ,charts = Charts(control)
           ,both = Both(control, transaction.data)
           )
    str(control)
    if (control$testing)
        cat('TESTING: DISCARD RESULTS\n')
    
}

#debug(Control)
default.args <- NULL  # synthesize the command line that will be used in the Makefile
#default.args <- list('--which', 'cv',    '--testSampleFraction', '.001')
#default.args <- list('--which', 'chart', '--testSampleFraction', '.001')
#default.args <- list('--which', 'both',  '--testSampleFraction', '.001')
#default.args <- list('--query.fraction', '.000001')
#default.args <- list('--query.fraction', '.00001')
#default.args <- list('--query.fraction', '.001')
#default.args <- list('--which', 'charts', '--query.fraction', '.01')
default.args <- list('--which', 'charts', '--query.fraction', '.0001')
default.args <- list('--query.fraction', '.001')

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
