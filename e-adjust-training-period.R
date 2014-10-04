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
                    ,num.test.samples.needed = 1
                    ,max.num.day = 30
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
Chart1Body <- function(control, best.num.training.days, evaluation) {
    # return a vector lines, the body of chart 1
    #cat('Chart1Body\n'); browser()
    DistributionDays <- function(nums) {
        # determine distribution, accounting for NA values
        #cat('DistributionDays\n'); browser()
        days <- sort(unique(nums))
        len <- length(nums)
        result <- NULL
        for (day in days) {
            result <- c( result
                        ,sprintf('fraction with best.num.training.days = %3d is %f'
                                 ,day
                                 ,sum(nums == day, na.rm = TRUE) / len
                                 )
                        )
        }
        result <- c( result
                    ,sprintf( 'fraction with best.num.training.days =  NA is %f'
                             ,sum(is.na(nums)) / len
                             )
                    ,' '
                    ,sprintf( 'number of test samples = %d'
                             ,len
                             )
                    )
        result
    }
    LineD <- function(msg, value) {
        sprintf( control$chart1.format.line.d
                ,msg
                ,round(value)
                )
    }
    LineF <- function(msg, value) {
        sprintf( control$chart1.format.line.f
                ,msg
                ,value
                )
    }
    c( LineD('root mean squared error', evaluation$rootMeanSquaredError)
      ,LineD('root median squared error', evaluation$rootMedianSquaredError)
      ,LineF('fraction within  10 percent', evaluation$fraction.within.10.percent)
      ,LineF('coverage', evaluation$coverage)
      ,LineD('median price', evaluation$median.price)
      ,' '
      ,DistributionDays(best.num.training.days)
      )
}
Chart1Heading <- function(control, evaluation) {
    description <- c( sprintf( 'Evaluation of %f Percent Random Sample of Training Transactions'
                              ,control$query.sample * 100
                              )
                     ,'Using Best Number of Training Days'
                     ,sprintf('As Determined through %d Test Transactions the week before the Query'
                              ,control$best.num.tests
                              )
                     ,'AVM scenario'
                     ,'Log-linear form'
                     ,sprintf( 'Testing period: %s through %s'
                              ,control$testing.period$first.date
                              ,control$testing.period$last.date
                              )
                     )
}
Chart1 <- function(control, best.num.training.days, evaluation) {
    chart1 <- c( Chart1Heading(control)
                ,' '
                ,Chart1Body(control, best.num.training.days, evaluation)
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
Analysis <- function(control, transaction.data) {
    # return predictions for each query sample
    #cat('Analysis\n'); browser()

    clock <- Clock()

    # determine which transactions will be the query transactions
    num.test.transactions <- 
        if(control$testing) 3
        else round(nrow(transaction.data) * control$query.fraction)
    which.test <- sample.int( n = nrow(transaction.data)
                             ,size = num.test.transactions
                             ,replace = FALSE
                             )
    if (FALSE && control$debug) {
        # drop all transaction not on special date
        special.date <- as.Date('1986-03-15')
        all.which.date <- transaction.data$saleDate[which.test]
        is.special.date <- special.date == all.which.date
        which.test <- which.test[is.special.date]
    }
    Printf('will sample %d transactions\n', length(which.test))

    # One model for each test transaction
    PredictTestSample <- function(test.sample.index) {
        # return prediction for the query point
        p <- Predict( query = transaction.data[test.sample.index,]
                     ,data.training = transaction.data[-test.sample.index, ]
                     ,control = control
                     )
    }
    ListExtract <- function(lst, field) {
        sapply(lst[field,], function(x) x)
    }
    pwc <- sapply(which.test, PredictTestSample)
    prediction <- ListExtract(pwc, 'prediction')
    num.days <- ListExtract(pwc, 'num.days')
    comparison.size <- ListExtract(pwc, 'comparison.size')
    query.date <- ListExtract(pwc, 'query.date')
    best.num.training.days <- ListExtract(pwc, 'best.num.training.days')

    actual <- transaction.data[which.test, 'price']
    evaluation <- Evaluate( prediction = prediction
                           ,actual = actual
                           )
    str(actual)
    str(prediction)
    str(evaluation)

    str(control)

    elapsed.cpu <- clock$Cpu()
    elapsed.message <- sprintf( 'predicting %d queries with %f query fraction took %f CPU seconds\n'
                              ,num.test.transactions
                              ,control$query.fraction
                              ,elapsed.cpu
                              )
    Printf(elapsed.message)

    save( actual
         ,best.num.training.days
         ,comparison.size
         ,control
         ,elapsed.cpu
         ,elapsed.message
         ,evaluation
         ,num.days
         ,query.date
         ,file = control$path.out.rdata
         )
    evaluation
}
Charts <- function(my.control) {
    # produce all the charts
    # for now, there is only one
    cat('starting Charts\n'); browser()

    # recover cetain values from the predictions
    best.num.training.days <- NULL
    evaluation <- NULL
    loaded <- load(file = my.control$path.out.rdata)
    str(loaded)  # NOTE: control has been replaced
    stopifnot(!is.null(best.num.training.days))
    stopifnot(!is.null(evaluation))

    # produce charts
    # use the controls from when data were created
    chart1 <- Chart1(control, best.num.training.days, evaluation)
                            
    writeLines( text = chart1
               ,con = my.control$path.out.chart1
               )
    print(chart1)

    print(elapsed.message)  # put the elapsed time from the Analysis phase in the final version of the log

    print(summary(best.num.training.days))

    # save results from the analysis pass and this pass

    save( actual
         ,best.num.training.days
         ,comparison.size
         ,control
         ,elapsed.cpu
         ,elapsed.message
         ,evaluation
         ,num.days
         ,query.date
         ,my.control
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
