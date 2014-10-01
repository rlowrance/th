# e-adjust-training-period.R
# main program
# Determine median RMedianSE for log-level AVM model when
# the training period is adjusted for each transaction
# based on cross-validation of the prior-day's query.
#
# Command line arguments
# --which             : one of {cv, chart, both}; default both
#                       cv means just do cross validation (write e-forms*.RData)
#                       chart means just produce chart output (write e-forms*.txt)
# --testSampleFraction: number
#                       fraction of samples in test period to use
# Approach: Use k-fold cross validation to compare estimated generalization errors for
# model variants

source('Directory.R')

source('Libraries.R')

source('ModelLinearLocal.R')
source('ReadTransactionSplits.R')

library(memoise)
library(optparse)

Control <- function(command.args) {
    # parse command line arguments in command.args
    opt <- ParseCommandArgs(command.args)

    me <- 'e-adjust-training-period' 

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
    testing <- TRUE
    out.base <- sprintf('%s--testSampleFraction-%f'
                        ,me
                        ,opt$testSampleFraction
                        )
    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,path.out.chart1 = paste0(working, out.base, '.txt')
                    ,query.fraction = opt$query.fraction
                    ,best.num.tests = opt$best.num.tests
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
                    ,chart1.format.header = '%-27s | %24s %24s %24s'
                    ,chart1.format.data =   '%-27s | %24.0f %24.3f %24.3f'
                    ,testing = testing
                    ,debug = FALSE
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
    opt.best.num.tests <- make_option( opt_str = c('--best.num.tests')
                                      ,action = 'store'
                                      ,type = 'double'
                                      ,default = 100
                                      ,help = 'fraction of test sample used for cross validation'
                                      )
    option.list <- list( opt.which
                        ,opt.query.fraction
                        ,opt.best.num.tests
                        )
    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
}
CreateChart1Body <- function(control, summary) {
    # return a vector lines, the body of chart 1
    result <- sprintf( control$chart1.format.header
                      ,' '
                      ,'median'
                      ,'mean fraction'
                      ,'mean'
                      )
    result <- c( result
                ,sprintf( control$chart1.format.header
                         ,'scenario-form-training days'
                         ,'RMedianSE'
                         ,'within 10%'
                         ,'coverage'
                         )
                )

    for (row.index in 1:length(summary)) {
        name <- names(summary)[[row.index]]
        row <- summary[[row.index]]
        result <- c( result
                    ,sprintf( control$chart1.format.data
                             ,name
                             ,row$median.RMedianSE
                             ,row$mean.fraction.within.10.percent
                             ,row$mean.coverage
                             )
                    )
    }

    result
}

CreateChart1 <- function(control, description, summary) {
    # return a vector of lines, the txt for chart 1
    #cat('start CreateChart1\n'); browser()

    result <- c( description
                ,' '
                ,CreateChart1Body(control, summary)
                )

    result
}


SummarizeFolds <- function(cv.result) {
    # convert cross validation result into an estimate of the generalization error
    SummarizeModel <- function(cv.result, model.name) {
        # summarize across folds for one model
        #cat('start EstGenError', model.name, '\n'); browser()
        this.result <- cv.result[[model.name]]

        Mean <- function(field.name) {
            elements <- sapply( this.result
                               ,function(x) x[[field.name]]
                               )
            result <- mean(elements)
            result
        }
        Median <- function(field.name) {
            elements <- sapply( this.result
                               ,function(x) x[[field.name]]
                               )
            result <- median(elements)
            result
        }

        summary.model <- list( mean.RMSE = Mean( 'rootMeanSquaredError')
                              ,median.RMedianSE = Median('rootMedianSquaredError')
                              ,mean.coverage = Mean('coverage')
                              ,mean.fraction.within.10.percent = Mean('fraction.within.10.percent')
                              ,mean.fraction.within.20.percent = Mean('fraction.within.20.percent')
                              ,mean.fraction.within.30.percent = Mean('fraction.within.30.percent')
                              ,mean.price = Mean('mean.price')
                              ,median.price = Median('median.price')
                              )
        summary.model
    }
    # for each model
    summary <- list()
    model.names <- names(cv.result)
    str(model.names)
    for (model.name in model.names) {
        summary[[model.name]] <- SummarizeModel(cv.result, model.name)
    }
    summary
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
    # return best num.training.days for the query transaction
    # aproach: test each candidate num.training.days and select one with lowest RMSE
    # approach: for a sample of transactions on the day before the query date, determine
    # the best number of training days
    #cat('DetermineBestTrainingPeriod\n'); browser()
    

    # build test and train data frames
    first.test.date <- query$saleDate - 8
    last.test.date <- query$saleDate - 1
    Printf('test dates %s through %s\n', first.test.date, last.test.date)
    test.all <- subset( data.training
                       ,subset = (saleDate >= first.test.date) & (saleDate <= last.test.date)
                       )
    train <- subset( data.training
                    ,subset = saleDate < first.test.date
                    )
    test <- test.all[sample(nrow(test.all), control$best.num.tests), ]
    Printf('nrow test.all %d test %d train %d\n', nrow(test.all), nrow(test), nrow(train))
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
    best.num.training.days <- num.training.days[[best.index]]
}
Predict <- function(query, data.training, control) {
    # return prediction for one query point
    #cat('Predict\n'); browser()
    
    best.num.training.days <- DetermineBestTrainingPeriod( query = query
                                                          ,data.training = data.training
                                                          ,control = control
                                                          )
    mall <- ModelAvmLinearLocal( queries = query
                                ,data.training = data.training
                                ,formula = control$formula
                                ,num.training.days = best.num.training.days
                                )
    prediction <- exp(mall$predictions)
    stopifnot(length(prediction) == 1)
    prediction
}
PredictTestSamples <- function(control, transaction.data) {
    # return predictions for each query sample
    cat('PredictTestSamples\n'); browser()

    clock <- Clock()

    # determine which transactions will be the query transactions
    num.test.transactions <- 
        if(control$testing) 3
        else round(nrow(transaction.data) * control$query.fraction)
    which.test <- sample.int( n = nrow(transaction.data)
                             ,size = num.test.transactions
                             ,replace = FALSE
                             )

    # One model for each test transaction
    PredictTestSample <- function(test.sample.index) {
        # return prediction for the query point
        Predict( query = transaction.data[test.sample.index,]
                ,data.training = transaction.data[-test.sample.index, ]
                ,control = control
                )
    }
    prediction <- sapply(which.test, PredictTestSample)
    actual <- transaction.data[which.test, 'price']
    evaluation <- Evaluate( prediction = prediction
                           ,actual = actual
                           )
    str(actual)
    str(prediction)
    str(evaluation)

    str(control)

    elapsed.cpu <- clock$Cpu()
    Printf('predicting %d queries took %f CPU seconds\n'
           ,num.test.transactions
           ,elapsed.cpu
           )

    save( actual
         ,control
         ,elapsed.cpu
         ,evaluation
         ,file = control$path.out.rdata
         )
    evaluation
}
Chart <- function(my.control, transaction.data) {
    #cat('starting Chart\n'); browser()
    stop('rewrite me')
    cv.result <- NULL
    loaded <- load(file = my.control$path.out.rdata)
    str(loaded)  # NOTE: control has been replaced
    stopifnot(!is.null(cv.result))

    summary <- SummarizeFolds(cv.result)
    str(summary)

    # produce charts
    # use the controls from when data were created
    description <- c( sprintf( 'Errors on %f Percent Sample of Transactions'
                              ,control$query.sample * 100
                              )
                     ,'Using Best Number of Training Days'
                     ,'AVM scenario'
                     ,'Log-linear form'
                     ,sprintf( 'Testing period: %s through %s'
                              ,control$testing.period$first.date
                              ,control$testing.period$last.date
                              )
                     )
                            
    chart1 <- CreateChart1( control = control
                           ,description = description
                           ,summary = summary
                           )
    writeLines( text = chart1
               ,con = my.control$path.out.chart1
               )
    print(chart1)

    # save results
    str(description)
    str(summary)
    save(my.control, cv.result, description, chart1, summary, file = my.control$path.out.rdata)
}
Both <- function(control, transaction.data) {
    debug(PredictTestSamples)
    debug(Chart)
    evaluation <- PredictTestSamples(control, transaction.data)
    Chart(control, evaluation)
}
Main <- function(control, transaction.data) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    clock <- Clock()
    switch( control$which
           ,cv = Cv(control, transaction.data)
           ,chart = Chart(control, transaction.data)
           ,both = Both(control, transaction.data)
           )
    Printf( 'testSample %f CPU seconds %f Wallclock seconds %f\n'
           ,control$testSample
           ,clock$Cpu()
           ,clock$Wallclock()
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
default.args <- list('--query.fraction', '.000001', '--best.num.tests', '2')

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
