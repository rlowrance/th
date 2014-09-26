# e-forms.R
# main program
# Determine best model form: level-level, level-log, log-level, or log-log
# For now, 
# - use just AVM scenario
#
# Command line arguments
# --which             : one of {cv, chart, both}
#                       cv means just do cross validation (write e-forms*.RData)
#                       chart means just produce chart output (write e-forms*.txt)
# --testSampleFraction: number
#                       fraction of samples in test period to use
# Approach: Use k-fold cross validation to compare estimated generalization errors for
# model variants

source('DirectoryLog.R')
source('DirectorySplits.R')
source('DirectoryWorking.R')

source('Libraries.R')

source('ModelLinearLocal.R')
source('ReadTransactionSplits.R')

library(memoise)
library(optparse)

Control <- function(command.args) {

    # parse command line arguments in command.args
    opt.which <- make_option( opt_str = c('--which')
                             ,action = 'store'
                             ,type = 'character'
                             ,default = 'both'
                             ,help = 'which action to take'
                             )
    opt.trainingDays <- make_option( opt_str = c('--trainingDays')
                                    ,action = 'store'
                                    ,type = 'integer'
                                    ,help = 'days in training period'
                                    )
    opt.testSampleFraction <- make_option( opt_str = c('--testSampleFraction')
                                          ,action = 'store'
                                          ,type = 'double'
                                          ,help = 'fraction of test sample actually used'
                                          )
    option.list <- list( opt.which
                        ,opt.trainingDays
                        ,opt.testSampleFraction
                        )
    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )


         
                                        


    me <- 'e-forms' 

    log <- DirectoryLog()
    splits <- DirectorySplits()
    working <- DirectoryWorking()

    # defines the splits that we use
    predictors.level = c(# continuous size positive
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
    predictors.log = c(# continuous size positive
                       'land.square.footage.log'
                       ,'living.area.log'
                       # continuous size nonnegative
                       ,'bedrooms.log1p'
                       ,'bathrooms.log1p'
                       ,'parking.spaces.log1p'
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
                    ,'price'
                    ,'price.log'
                    # apn
                    ,'apn'
                    )
    testing <- FALSE
    #testing <- TRUE
    out.base <- sprintf('%s--trainingDays-%d--testSample-%f'
                        ,me
                        ,opt$trainingDays
                        ,opt$testSampleFraction
                        )
    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,path.out.chart1 = paste0(working, out.base, '.txt')
                    ,test.sample.fraction = opt$testSampleFraction
                    ,predictors.level = predictors.level
                    ,predictors.log = predictors.log
                    ,response.level = 'price'
                    ,response.log = 'price.log'
                    ,split.names = unique(c( predictors.level
                                            ,predictors.log
                                            ,other.names
                                            )
                    )
                    ,nfolds = 10
                    ,testing.period = list( first.date = as.Date('1984-02-01')
                                           ,last.date = as.Date('2009-03-31')
                                           )
                    ,num.training.days = opt$trainingDays
                    ,chart1.format.header = '%-27s | %24s %24s %24s'
                    ,chart1.format.data =   '%-27s | %24.0f %24.3f %24.3f'
                    ,testing = testing
                    ,debug = FALSE
                    ,which = opt$which
                    )
    control
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
                         ,'scenario & form'
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

ModelAssessorLinearLocal <- function(queries, data.training, formula, num.training.days) {
    # return vector of predictions from local assssor model trained for each query
    #cat('start ModelAssessorLinearLocal\n'); browser()
    InTraining <- function(saleDate) {
        # return selector vector to identify training samples for the saleDate
        in.training <- data.training$recordingDate >= (saleDate - num.training.days) &
                       data.training$recordingDate <= (saleDate - 1)

    }
    ModelLinearLocal( InTraining
                     ,queries
                     ,data.training
                     ,formula
                     ,num.training.days
                     )
}
ModelAvmLinearLocal <- function(queries, data.training, formula, num.training.days) {
    # return vector of predictions from local assssor model trained for each query
    #cat('start ModelAvmLinearLocal\n'); browser()
    InTraining <- function(saleDate) {
        # return selector vector to identify training samples for the saleDate
        in.training <- data.training$saleDate >= (saleDate - num.training.days) &
                       data.training$saleDate <= (saleDate - 1)

    }
    ModelLinearLocal( InTraining
                     ,queries
                     ,data.training
                     ,formula
                     )
}
ModelMortgageLinearLocal <- function(queries, data.training, formula, num.training.days) {
    # return vector of predictions from local assssor model trained for each query
    #cat('start ModelMortgageLinearLocal\n'); browser()
    debug <- TRUE
    debug <- FALSE
    InTraining <- function(saleDate) {
        # return selector vector to identify training samples for the saleDate
        days.around <- num.training.days / 2
        in.training <- data.training$saleDate >= (saleDate - days.around) &
                       data.training$saleDate <= (saleDate + days.around)
        if (debug) {
            cat( 'debug ModelMortgageLinearLocal'
                ,'saleDate', as.character(as.Date(saleDate, origin = '1970-01-01'))
                ,'num.training.days', num.training.days
                ,'days.around', days.around
                ,'number training samples', sum(in.training)
                ,'\n'
                )
            if (sum(in.training) <= 2) browser()
        }
        in.training
    }
    ModelLinearLocal( InTraining
                     ,queries
                     ,data.training
                     ,formula
                     ,num.training.days
                     )
}
Queries <- function(data, control) {
    # return dataframe of queries from the testing data
    #cat('start Queries\n'); browser()
    ok.testing.first.date <- data$saleDate >=  control$testing.period$first.date
    ok.testing.last.date <- data$saleDate <= control$testing.period$last.date
    is.query <- ok.testing.first.date & ok.testing.last.date
    queries <- data[is.query, ]
}
FitPredictAvm <- function(response, predictors, data, is.testing, is.training, control) {
    verbose <- TRUE
    queries.all <- Queries(data[is.testing, ], control)

    # sample the possible queries
    r <- runif( n = nrow(queries.all)
               ,min = 0
               ,max = 1)
    use.query <- ifelse( r < control$test.sample.fraction
                        ,TRUE
                        ,FALSE
                        )
    queries <- queries.all[use.query, ]

    data.training <- data[is.training, ]
    formula <- Formula( predictors = predictors
                       ,response = response
                       )
    model.result <- ModelAvmLinearLocal( queries = queries
                                        ,data.training = data.training
                                        ,formula = formula
                                        ,num.training.days = control$num.training.days
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
Evaluate <- function(prediction, actual) {
    # return list of evaluations

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
}
FitPredictAvmLevelLevel <- function(data, is.testing, is.training, control) {
    FitPredictAvm( response = control$response.level
                  ,predictors = control$predictors.level
                  ,data = data
                  ,is.testing = is.testing
                  ,is.training = is.training
                  ,control = control
                  )
}
FitPredictAvmLevelLog <- function(data, is.testing, is.training, control) {
    FitPredictAvm( response = control$response.level
                  ,predictors = control$predictors.log
                  ,data = data
                  ,is.testing = is.testing
                  ,is.training = is.training
                  ,control = control
                  )
}
FitPredictAvmLogLevel <- function(data, is.testing, is.training, control) {
    FitPredictAvm( response = control$response.log
                  ,predictors = control$predictors.level
                  ,data = data
                  ,is.testing = is.testing
                  ,is.training = is.training
                  ,control = control
                  )
}
FitPredictAvmLogLog <- function(data, is.testing, is.training, control) {
    FitPredictAvm( response = control$response.log
                  ,predictors = control$predictors.log
                  ,data = data
                  ,is.testing = is.testing
                  ,is.training = is.training
                  ,control = control
                  )
}
Cv <- function(control, transaction.data) {
    if (control$testing) {
        EvaluateModel <- list( FitPredictAvmLevelLevel
                              ,FitPredictAvmLogLog
                              )
        model.name <- list( 'AVM level level'
                           ,'AVM log log'
                           )
        nfolds <- 2
    } else {
        EvaluateModel <- list( FitPredictAvmLevelLevel
                              ,FitPredictAvmLevelLog
                              ,FitPredictAvmLogLevel
                              ,FitPredictAvmLogLog
                              )
        model.name <- list( 'AVM level level'
                           ,'AVM level log'
                           ,'AVM log level'
                           ,'AVM log log'
                           )
        nfolds <- control$nfolds
    }

    cv.result <- CrossValidate( data = transaction.data
                               ,nfolds = nfolds
                               ,EvaluateModel = EvaluateModel
                               ,model.name = model.name
                               ,control = control
                               ,verbose = TRUE
                               )
    # save results
    save(control, cv.result, file = control$path.out.rdata)
    str(cv.result)
}
Chart <- function(my.control, transaction.data) {
    cv.result <- NULL
    loaded <- load(file = my.control$path.out.rdata)
    str(loaded)  # NOTE: control has been replaced
    stopifnot(!is.null(cv.result))

    summary <- SummarizeFolds(cv.result)
    str(summary)

    # produce charts
    # use the control from when they were created
    description <- c( 'Estimated Generalization Error'
                     ,sprintf('From %d-fold Cross Validation', control$nfolds)
                     ,'AVM scenario'
                     ,sprintf('Training period: %d days', control$num.training.days)
                     ,sprintf( 'Testing period: %s through %s'
                              ,control$testing.period$first.date
                              ,control$testing.period$last.date
                              )
                     ,sprintf( 'Using %f fraction random sample of test transactions'
                              ,control$test.sample.fraction
                              )
                     )
                            
    chart1 <- CreateChart1( control = control
                           ,description = description
                           ,summary = summary
                           )
    writeLines( text = chart1
               ,con = control$path.out.chart1
               )
    print(chart1)

    # save results
    str(description)
    str(summary)
    save(description, control, cv.result, chart1, summary, file = control$path.out.rdata)
}
Both <- function(control, transaction.data) {
    Cv(control, transaction.data)
    Chart(control, transaction.data)
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
    
}

#debug(Control)
default.args <- NULL  # synthesize the command line that will be used in the Makefile
#default.args <- list('--which', 'cv',    '--testSampleFraction', '.001')
#default.args <- list('--which', 'chart', '--testSampleFraction', '.001')
#default.args <- list('--trainingDays', '90', '--testSampleFraction', '.001')
#default.args <- list('--trainingDays', '120', '--testSampleFraction', '.01')

command.args <- if (is.null(default.args)) commandArgs(trailingOnly = TRUE) else default.args
#command.args <- if (is.null(default.args)) CommandArgs(defaultArgs = default.args) else default.args
#parsed.command.args <- ParseCommandLine( cl = command.args
#                                        ,keywords = c('which', 'trainingDays', 'testSampleFraction')
#                                        ,ignoreUnexpected = TRUE
#                                        )
#debug(Control)
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
